###############################################################################
#Project: FSDT 
#
###############################################################################

#drop all objects from memory
rm(list = ls())

#### Load/Install required packages ####
#devtools::install_github('hadley/ggplot2')
pkgs <- c("dplyr", "foreign", "Hmisc", "data.table", "broom","stringi", "DT","treemap",
          "haven", "shiny", "shinydashboard", "leaflet", "tools","purrr","assertthat",
          "highcharter", "tidyr", "RColorBrewer", "ggmap", "rgdal", "rgeos", "purrr",
          "maptools", "dplyr", "tmap", "sp","tidyverse", "weights", "rlang", "dummies")

if(!"pacman" %in% installed.packages()[,1]){install.packages("pacman")}
pacman::p_load(pkgs, character.only = T, install = F, dependencies = TRUE)
if(!"plyr" %in% installed.packages()[,1]){install.packages("plyr")}

#### Import the raw data into R ####
finScope.raw = suppressWarnings(read.spss("FinScope Tanzania 2017_27092017.sav", to.data.frame=TRUE))

## Rename some variables
setnames(finScope.raw, c("C8", "C9"), c("age", "gender")) 

###############################################################################
#Descriptives
###############################################################################
## Location distribution
dataLoc <- as.data.frame(finScope.raw %>% 
                           group_by(Cluster) %>%
                           summarise(sample_count = n(),
                                     pop_count = sum(Final_weight)) %>%
                           mutate(percentage = pop_count*100/sum(pop_count)))

# gender distribution
dataGen <- as.data.frame(finScope.raw %>% 
                           group_by(gender) %>%
                           summarise(sample_count = n(),
                                     pop_count = sum(Final_weight)) %>%
                           mutate(percentage = pop_count*100/sum(pop_count),
                                  cum = cumsum(percentage)))

###############################################################################
#Analysis: Propensity score matching
#Estimate the propensity score (the probability of being mm given a set of covariates)
###############################################################################

#----- Define variables : that can define difference -----
# 1. Connectivity :
# mobile phone access
# internet access
# computer access
setnames(finScope.raw, c("C23.1", "C23.2", "C23.3"),
         c("connectivity_mobile", "connectivity_internet", "connectivity_computer"))

# 2. Education level of individual
finScope.raw$educ <- if_else(finScope.raw$C11.1 %in% c("Post primary technical training","University or other higher education"), "Technical/Tertiary education",
                             if_else(finScope.raw$C11.1 %in% c("Some primary", "Primary completed"), "Primary level",
                                     if_else(finScope.raw$C11.1 %in% c("Some secondary", "Secondary competed"), "Secondary level",
                                             if_else(finScope.raw$C11.1=="No formal education","No formal education",""))))

# 3. Ownership of property
# 4. Land ownership
setnames(finScope.raw, c("educ", "C22", "C13"), c("education", "property_own", "land_own"))

# 5. Income generating activity
incAct <- unique(finScope.raw$IncomeMain)
finScope.raw$income_activity <- if_else(finScope.raw$IncomeMain %in% grep("salaried", incAct,value = T, ignore.case = T), "Formal/Informal salaried",
                               if_else(finScope.raw$IncomeMain %in% grep("Traders", incAct,value = T, ignore.case = T), "Non-agricultural/Agricultural Traders",
                                       if_else(finScope.raw$IncomeMain %in% c("Rental income", "Interest from savings, investments, stocks, unit trusts etc.", "Pension","Welfare", "Other"), "Other",
                                               as.character(finScope.raw$IncomeMain))))

# 5. Location: Cluster
# 6. PPI category : PPI_Category
# 7. Wealth index: use row means
# create dummies assest owned/husehold characteristics
# wall
# cooking
# roof
setnames(finScope.raw, c("KH5", "KH6", "KH8"), c("wall","roof","cooking"))

# floor
finScope.raw$floor <- if_else(finScope.raw$KH7 %in% c("Cement", "Ceramic tiles", "Parquet or polished wood", "Vinyl or asphalt strips"), "Cement/Tiles/Polished wood",
                              if_else(finScope.raw$KH7 %in% c("Wood planks", "Palm/bamboo", "Parquet or polished wood", "Earth/sand", "Dung", "Other"), "Earth/Wood/Other",""))

# tv
finScope.raw$tv <- if_else(finScope.raw$KH9=="Yes",1,0)

# radios, cassette/tape recorders, or hi-fi systems
finScope.raw$radio <- if_else(finScope.raw$KH10=="Yes",1,0)

# lanterns/electric lighting
finScope.raw$lighting <- if_else(finScope.raw$KH11=="Yes",1,0)

# tables
finScope.raw$table <- if_else(finScope.raw$KH12=="Yes",1,0)

# mobile phone
finScope.raw$mobile <- if_else(finScope.raw$KH14=="Yes",1,0)

# toilet facility
finScope.raw$toilet <- if_else(finScope.raw$KH15 %in% c("Other", "Composting toilet/ecosan latrine\t"), "No toilet/bush /field", as.character(finScope.raw$KH15))
# Assets
old <- c("KH16_1","KH16_2","KH16_3","KH16_4","KH16_5","KH16_6","KH16_7","KH16_8","KH16_9","KH16_10","KH16_11","KH16_12","KH16_13","KH16_14","KH16_15","KH16_16","KH16_17","KH16_18","KH16_19")
new <- c("computer","internet","iron","fridge","washing_machine","cooker","bicycle","motor_cycle","motor_vehicle","tri_cycle","motor_cycle","hoe","wheelbarrow","plough","power_tiller","oxen","donkey","piped_water","electricity")

data <- as.data.frame(sapply(finScope.raw[old], function(x) x=if_else(x=="Yes",1,0)))
names(data) <- new

# Household xstics
xstics <- c("roof", "wall", "floor", "cooking", "toilet")
house_xstic <- dummy.data.frame(finScope.raw[xstics], sep = "_")

#8. age of the respondents: age

#---- Method 2: Compute asset index the abstract way:  add up the assets then divide by tottal ----
asset.data <- cbind(finScope.raw[c("tv", "radio", "table", "mobile")], data)
names(asset.data)
asset.data$asset_index = rowMeans(asset.data)
finScope.raw <- cbind(finScope.raw, asset.data["asset_index"])

#---- Generate new variables  - recommendations from julia ------
#### Instead of education level use
# Literacy level

finScope.raw <- finScope.raw %>%
  mutate(K1.1_new = trimws(as.character(K1.1)),
         K1.2_new = trimws(as.character(K1.2))) %>%
  mutate(literacy_kis = factor(if_else(K1.1_new == "Can write only", "Can read and write", K1.1_new)),
         literacy_eng = factor(if_else(K1.2_new == "Can write only", "Can read and write", K1.2_new)))
# Numeracy level - add, subtract, multiply, divide
finScope.raw <- finScope.raw %>%
  mutate(addition = if_else(K2.1=="TSH 21 000", 1, 0),
         subtraction = if_else(K2.2=="TSH 29 000", 1, 0),
         multiplication = if_else(K2.3=="TSH 700 000", 1, 0),
         division = if_else(K2.4=="TSH 30 000", 1, 0)) %>%
  mutate(numeracy_score = addition + subtraction + multiplication + division) %>%
  mutate(addition2 = if_else(addition==1, "Can add", "Cannot add"),
         subtraction2 = if_else(subtraction==1, "Can subtract", "Cannot subtract"),
         multiplication2 = if_else(multiplication==1, "Can multiply", "Cannot multiply"),
         division2 = if_else(division==1, "Can divide", "Cannot divide"))

#### Instead of main income generating activity
# Have dependants and self_reliant people
finScope.raw <-  finScope.raw %>%
  mutate(income_source_type = if_else(D2.1_10=="Yes", "Do not get money - someone else pays my expenses",
         as.character(D2.4))) %>%
  mutate(income_source_type2 =factor(if_else(income_source_type=="{0}", "",
                                         if_else(income_source_type %in% c("Interest from savings, investments, stocks, unit trusts etc.", "Rental income"), "Interest from savings etc /Rental income",
                                                 as.character(income_source_type)))))
#### Connectivity - use seperate ones
#combine internt and computer
finScope.raw <- finScope.raw %>%
  mutate(internet_computer_access = if_else(connectivity_computer=="Yes" |
                                               connectivity_internet=="Yes",1,0))


#### Instead of ppi score/asset index - use monthly income
(income_vars <- grep("D8_", names(finScope.raw), value = T))
inc_freq_vars <- income_vars[seq(1,length(income_vars),2)]
inc_amt_vars <- income_vars[seq(2,length(income_vars),2)]

inc_amts <- list()
income_amt <- c()

for(i in 1:length(inc_freq_vars)){
 income_amt <- if_else(finScope.raw[,inc_freq_vars[i]]=="Per Annum",
                                       finScope.raw[,inc_amt_vars[i]]/12,
                                                    finScope.raw[,inc_amt_vars[i]])
  inc_amts[[i]] <- income_amt
}

inc_amts_data <- as.data.frame(Reduce(f = cbind, x = inc_amts))

sums <- function(x){
  return(sum(x, na.rm = T))
}
inc_amts_data$tot_month_amt <- apply(X = inc_amts_data, MARGIN = 1, FUN = sums)

finScope.raw <- cbind(finScope.raw, inc_amts_data[,"tot_month_amt"])
finScope.raw <- finScope.raw %>%
  dplyr::rename(tot_month_amt = `inc_amts_data[, "tot_month_amt"]`)

# replace incomes higher than the 99% values to the 99% pervcentile value
(perce <- quantile(finScope.raw$tot_month_amt, c(0.05, 0.99)))

finScope.raw <- finScope.raw %>%
  mutate(tot_month_amt2 = if_else(tot_month_amt>as.numeric(perce[2]), as.numeric(perce[2]),
                                   tot_month_amt))


# #########PSM on new variables#########
finScope.raw$mm <- if_else(finScope.raw$MM=="Have or use services",1,0)
finScope.raw$banked <- if_else(finScope.raw$Banked=="Have or use services",1,0)

psm.vars <- c("Access to mobile" = "connectivity_mobile", 
              "Access to computer/internet" = "internet_computer_access", 
              "Kiswahili literacy" = "literacy_kis",
              "English literacy" = "literacy_eng",
              "Numeracy score" = "numeracy_score",
              "Property ownership" = "property_own", 
              "Land ownership" = "land_own", 
              "PPI Category" = "PPI_Category",
              "Income relied on" = "income_source_type2",
              "Location" = "Cluster",
              "Monthly income amount" = "tot_month_amt2", 
              "Age" ="age", "Gender" = "gender")

all.vars <- c("Education level" = "education", 
              "Asset/Wealth index" = "asset_index", 
              "Main income generating activity" = "income_activity", psm.vars)
 
cross.tab.vars <- c("Access to mobile" = "connectivity_mobile", 
                    "Access to computer/internet" = "internet_computer_access", 
                    "Kiswahili literacy" = "literacy_kis",
                    "English literacy" = "literacy_eng",
                    "Numeracy score" = "numeracy_score",
                    "Property ownership" = "property_own", 
                    "Land ownership" = "land_own", 
                    "PPI Category" = "PPI_Category",
                    "Income relied on" = "income_source_type2",
                    "Location" = "Cluster",
                    "Monthly income amount" = "tot_month_amt2", 
                    "Age" ="age", "Education level" = "education", 
                    "Asset/Wealth index" = "asset_index", "Main income generating activity" = "income_activity") 

# list of variables to crosstab with segments
# keep factors only
# keep those with atmost 12 levels
# drop variables with _other
vars.list <- sapply(finScope.raw, function(x) class(x))
vars.list <- sapply(finScope.raw[,names(vars.list)[vars.list %in% c("character", "factor")]], function(x) length(unique(x)))
vars.list2 <- names(vars.list)[between(vars.list,2,12)]
vars.list3 <- vars.list2[!vars.list2 %in% c(grep("_other", vars.list2, ignore.case = T, value = T), "gender")]

## Mark points on map: variables on the choices list
finScope.raw <- finScope.raw %>%
  mutate(use.formal = if_else(Formal %in% c("Have or use services","Have the service"),1,0),
         use.informal = if_else(Informal %in% c("Have or use services","Have the service"),1,0),
         use.banks = if_else(Banked %in% c("Have or use services","Have the service"),1,0),
         use.mnos = if_else(MM %in% c("Have or use services","Have the service"),1,0),
         use.mfi = if_else(MFI %in% c("Have or use services","Have the service"),1,0),
         use.pension = if_else(Pension %in% c("Have or use services","Have the service"),1,0),
         use.insurance = if_else(Insurance %in% c("Have or use services","Have the service"),1,0),
         use.sacco = if_else(SACCO %in% c("Have or use services","Have the service"),1,0),
         use.saving.group = if_else(SG %in% c("Have or use services","Have the service"),1,0),
         use.shylock = if_else(Shylock %in% c("Have or use services","Have the service"),1,0),
         use.capital = if_else(Capitalm %in% c("Have or use services","Have the service"),1,0),
         borrow = if_else(G2.1.1=="Yes", 1, 0),
         save = if_else(F4.1_1=="Yes" | 	F4.1_2=="Yes" | 	F4.1_3=="Yes" | 	F4.1_4=="Yes" | 	
                          F4.1_5=="Yes" | 	F4.1_6=="Yes" | 	F4.1_7=="Yes" | 	F4.1_8=="Yes" | 	
                          F4.1_9=="Yes" | 	F4.1_10=="Yes" | 	F4.1_11=="Yes" | 	F4.1_12=="Yes" |
                          F4.1_13=="Yes" | 	F4.1_14=="Yes", 1,0),
         farmer.yes = if_else(Farmers=="Farmers", 1, 0),
         comfor_bank_fin_inst = if_else(E5i=="True", 1, 0),
         save_bank_confident = if_else(F3.2.1_1=="Yes", 1, 0),
         loans_bank_confident = if_else(G1.2.2=="Yes", 1, 0),
         send = if_else(H1=="Yes", 1, 0),
         receive = if_else(H4=="Yes", 1, 0))

map_vars <- c("Have/use formal services" = "use.formal", 
              "Have/use informal services" = "use.informal",       
              "Have/use banks" = "use.banks",
              "Have/use mobile money services" = "use.mnos",
              "Have/use micro finance institutions" = "use.mfi",             
              "Have/use pension" = "use.pension",
              "Have/use insurance" = "use.insurance",
              "Have/use SACCOS" = "use.sacco",          
              "Have/use saving groups" = "use.saving.group",
              "Have/use Shylock services" = "use.shylock",
              "Have/use capital markets" = "use.capital",
              "Borrowed money in the last 12 months" = "save",
              "Are you a farmer" = "farmer.yes",
              "Comfortable to go into a bank or another financial institution " = "comfor_bank_fin_inst",
              "Confident to deal with banks regarding savings" = "save_bank_confident",
              "Confident to deal with banks regarding borrowing/loans?" = "loans_bank_confident",
              "Have sent money to someone" = "send",
              "Have received the money from someone" = "receive")

## Import the Tanzania Regions shapefile
tz_regions <- readOGR("shapefile/Regions/", "Regions", verbose = F)

# collapse the "songwe region to Mbeya since it is new - hasn't been captured 
finScope.raw$Region_Nam <- if_else(finScope.raw$Region=="Songwe", "Mbeya", as.character(finScope.raw$Region))
finScope.raw$Region_Nam <- if_else(finScope.raw$Region=="Dar es Salaam", "Dar es salaam", as.character(finScope.raw$Region))



# mm.psm.vars %in% names(finScope.raw)
# 
# mm_psm.data <- finScope.raw[c("SbjNum", mm.psm.vars)]
# str(mm_psm.data)
# 
# ##### MNO vs NON_MNO #####
# # Estimate the propensity score using a logit model
# psm.model <- glm(mm ~ .,family = binomial(), data = mm_psm.data[,-1])
# summary(psm.model)
# 
# # Calculate the propensity score: predicted probability of being mm
# psm.pred <- data.frame(mm_pr_score = predict(psm.model, type = "response"),
#                        mm = psm.model$model$mm) %>%
#   mutate(mm2 = if_else(mm==1,"MNO user", "Non-MNO user"))
# head(psm.pred)
# 
# # plot a histogram of the scores by banking status
# ggplot(psm.pred, aes(x = mm_pr_score)) +
#   geom_histogram(bins = 50, colour="black", fill="lightblue",
#                  aes(y=..count../sum(..count..))) +
#   facet_wrap(~mm2) +
#   xlab("Probability of using MNOs") +
#   theme_bw()
# ggsave(filename = "Output/Second Analysis Iteration/MM/Graphs/hist_pr_score.jpeg")
# 
# # add the psm score to the dataset
# finScope.raw <- cbind(finScope.raw, psm.pred["mm_pr_score"])
# 
# # compute quintiles: using the scores of the unmm people
# (quint <- quantile(finScope.raw[which(finScope.raw$mm==0), "mm_pr_score"], seq(0, 1, 1/3))) # find quintiles
# 
# # distribtuion of the mm variable
# tab <- wtd.table(finScope.raw$mm, weights = finScope.raw$Final_weight)
# tab$sum.of.weights/sum(tab$sum.of.weights)*100
# 
# # create segments
# finScope.raw  <- finScope.raw %>%
#   mutate(mm_segments = if_else(mm==1, "MNO users",
#                                if_else(mm_pr_score>=as.numeric(quint[2]), "DFS able", "non-DFS able")))
# 
# # distribution of the new segments
# tab <- wtd.table(finScope.raw$mm_segments, weights = finScope.raw$Final_weight)
# tab$sum.of.weights/sum(tab$sum.of.weights)*100
# 
# # Histogram of propensity score
# ggplot(finScope.raw[finScope.raw$mm==0,], aes(x=mm_pr_score)) + theme_bw() +
#   geom_histogram(bins = 50, colour="black", fill="lightblue",
#                  aes(y=..count../sum(..count..))) +
#   geom_vline(xintercept=quint[2], linetype="dashed", color = "red") + 
#   labs(y = "Proportion", x = "Propensity Score (Similarity to MNO users)") +
#   theme(axis.text = element_text(size = rel(1.6)),
#         axis.title = element_text(size = rel(1.6)),
#         plot.title = element_text(size = rel(1.7), hjust = 0.5),
#         legend.title=element_blank(),
#         legend.position = "none")
# ggsave(filename = "Output/Second Analysis Iteration/MM/Graphs/hist_pr_score_cutoffs.jpeg")
# 
# ##### Banked vs non_banked #####
# finScope.raw$banked <- if_else(finScope.raw$Banked=="Have or use services",1,0)
# 
# bk.psm.vars <- c("connectivity_mobile", "internet_computer_access", 
#                  "literacy_kis","literacy_eng","numeracy_score",
#                  "education", "property_own", "land_own", "PPI_Category",
#                  "income_source_type2", "Cluster",
#                  "banked", "tot_month_amt2", "age", "gender")
# 
# bk_psm.data <- finScope.raw[c("SbjNum", bk.psm.vars)]
# str(bk_psm.data)
# 
# # Estimate the propensity score using a logit model
# psm.model <- glm(banked ~ .,family = binomial(), data = bk_psm.data[,-1])
# summary(psm.model)
# 
# # Calculate the propensity score: predicted probability of being banked
# psm.pred <- data.frame(bk_pr_score = predict(psm.model, type = "response"),
#                        banked = psm.model$model$banked) %>%
#   mutate(banked2 = if_else(banked==1,"Banked", "Non-banked"))
# head(psm.pred)
# 
# # plot a histogram of the scores by banking status
# ggplot(psm.pred, aes(x = bk_pr_score)) +
#   geom_histogram(bins = 50, colour="black", fill="lightblue",
#                  aes(y=..count../sum(..count..))) +
#   facet_wrap(~banked2) +
#   xlab("Probability of being banked") +
#   theme_bw()
# ggsave(filename = "Output/Second Analysis Iteration/Banking/Graphs/hist_pr_score.jpeg")
# 
# # add the psm score to the dataset
# finScope.raw <- cbind(finScope.raw, psm.pred["bk_pr_score"])
# 
# # compute quintiles: using the scores of the unbanked people
# (quint <- round(quantile(finScope.raw[which(finScope.raw$banked==0), "bk_pr_score"], seq(0, 1, 1/3)),4)) # find quintiles
# 
# # distribtuion of the banked variable
# tab <- wtd.table(finScope.raw$banked, weights = finScope.raw$Final_weight)
# tab$sum.of.weights/sum(tab$sum.of.weights)*100
# 
# # create segments
# #Banked	Bankable	Development	Unbankable
# #16.72068158	23.64057025	32.31724689	27.32150128
# cut_offs <- c(0.105, 0.02398)
# finScope.raw  <- finScope.raw %>%
#   mutate(bk_segments = if_else(banked==1, "Banked",
#                             if_else(bk_pr_score>0.105, "Bankable",
#                                     if_else(bk_pr_score >=0.02398 & bk_pr_score<=0.11, "Development", "Unbankable"))))
# 
# 
# # dim(finScope.raw[which(finScope.raw$bk_pr_score<=0.11 & finScope.raw$bk_pr_score<=0.03398),])
# # (finScope.raw[which(finScope.raw$bk_pr_score<0.11),"bk_pr_score"])
# 
# # distribution of the new segments
# tab <- wtd.table(finScope.raw$bk_segments, weights = finScope.raw$Final_weight)
# tab$sum.of.weights/sum(tab$sum.of.weights)*100
# 
# 
# # Histogram of propensity score
# ggplot(finScope.raw[finScope.raw$banked==0,], aes(x=bk_pr_score)) + theme_bw() +
#   geom_histogram(bins = 50, colour="black", fill="lightblue",
#                  aes(y=..count../sum(..count..))) +
#   geom_vline(xintercept=cut_offs[1], linetype="dashed", color = "red") + 
#   geom_vline(xintercept=cut_offs[2], linetype="dashed", color = "red") + 
#   labs(y = "Proportion", x = "Propensity Score (Similarity to banked individuals)") +
#   theme(axis.text = element_text(size = rel(1.6)),
#         axis.title = element_text(size = rel(1.6)),
#         plot.title = element_text(size = rel(1.7), hjust = 0.5),
#         legend.title=element_blank(),
#         legend.position = "none")
# ggsave(filename = "Output/Second Analysis Iteration/Banking/Graphs/hist_pr_score_cutoffs.jpeg")
# 
# 
# ####### DECK graphs######
# # function to compute percentages - overall
# percent_overall <- function(x, lab="", folder,sub){
#   # x = variable to compute one way frequency
#   # lab = label of the variable
#   # folder - folder where we are storing output
#   # sub - plot subtitle
#   
#   # drop missings from data first
#   data <- finScope.raw
#   names(data)[which(names(data)==x)] <- "var"
#   data$var <- as.character(data$var)
#   data$var <- trimws(data$var)
#   data <- data[data$var!="" , ]
#   data <- data[is.na(data$var)==F, ] 
#   data <- data[data$var!="NA", ]
#   
#   # table
#   df2 <- as.data.frame(data %>% 
#                          group_by_(.dots="var") %>%
#                          summarise(sample_count = n(),
#                                    pop_count = sum(Final_weight)) %>%
#                          mutate(percentage = pop_count*100/sum(pop_count)))
#   
#   # graph
#   ggplot(df2, aes(y = percentage, x=var)) +
#     geom_bar(stat="identity", colour = "deepskyblue3", fill = "deepskyblue3") + theme_bw() +
#     labs(y = "Percentage of the population", x = paste(sub)) +
#     #ggtitle("Percentage of the population") +
#     theme(axis.text = element_text(size = rel(1.5)),
#           axis.title = element_text(size = rel(1.5)),
#           plot.title = element_text(size = rel(1.6), hjust = 0.5),
#           legend.title=element_blank(),
#           legend.position = "none")
#   
#   ggsave(filename = paste("Output/",folder, "/Graphs/",x,"_dist.jpeg",sep=""))
#   
#   # export the summarized table
#   names(df2)[which(names(df2)=="var")] <- x
#   write.xlsx(df2, paste("Output/",folder, "/Tables/",x,"_dist.xlsx",sep=""))
#   
# }
# 
# ### Functions to do the plots
# percent_segment <- function(x, lab="", folder, sub, segment){
#   # x = variable to crosstab with segment
#   # lab = label of the variable
#   # folder - folder where we are storing output
#   # sub - plot subtitle
#   
#   # drop missings from data first
#   data <- finScope.raw
#   names(data)[which(names(data)==x)] <- "var"
#   names(data)[which(names(data)==segment)] <- "segments2"
#   data$var <- as.character(data$var)
#   data$var <- trimws(data$var)
#   data <- data[data$var!="" , ]
#   data <- data[is.na(data$var)==F, ] 
#   data <- data[data$var!="NA", ]
#   
#   # table
#   df2 <- as.data.frame(data %>% 
#                          group_by_(.dots=c("segments2", "var")) %>%
#                          summarise(sample_count = n(),
#                                    pop_count = sum(Final_weight)) %>%
#                          mutate(percentage = pop_count*100/sum(pop_count)))
#   
#   # graph
#   ggplot(df2, aes(x=segments2, y=percentage, fill=var)) +
#     geom_bar(stat="identity") + theme_bw() +
#     scale_fill_brewer() + 
#     labs(y = "Percentage of the population", x = "Segments", 
#          subtitle = paste(sub), title = paste("Percentage of the population by segments",sep=" ")) +
#     #ggtitle(paste("Percentage of the population by Segments",sep=" ")) +
#     theme(axis.text = element_text(size = rel(1.5)),
#           axis.title = element_text(size = rel(1.5)),
#           plot.title = element_text(size = rel(1.6), hjust = 0.5),
#           plot.subtitle = element_text(size = rel(1.2), hjust = 0.5),
#           legend.title=element_blank(),
#           legend.text = element_text(size = rel(0.9)),
#           legend.position = "bottom") +
#     guides(fill=guide_legend(ncol=2))
# 
#   ggsave(filename = paste("Output/",folder, "/Graphs/","Segments_",x,"_dist.jpeg",sep=""))
#   
#   # export the summarized table
#   names(df2)[which(names(df2)=="var")] <- x
#   write.xlsx(df2, paste("Output/",folder, "/Tables/","Segments_",x,"_dist.xlsx",sep=""))
#   }
# 
# 
# #function to compute percentages by segment and gender
# percent_segment_gender <- function(x, lab="", folder, sub, segment){
#   # x = variable to crosstab with segment
#   # lab = label of the variable
#   # folder - folder where we are storing output
#   # sub - plot subtitle
#   
#   # drop missings from data first
#   data <- finScope.raw
#   names(data)[which(names(data)==x)] <- "var"
#   names(data)[which(names(data)==segment)] <- "segments2"
#   data$var <- as.character(data$var)
#   data$var <- trimws(data$var)
#   data <- data[data$var!="" , ]
#   data <- data[is.na(data$var)==F, ] 
#   data <- data[data$var!="NA", ]
#   
#   sex = c("Male", "Female")
#   # table
#   for (s in sex) {
#     df2 <- as.data.frame(data %>% 
#                            filter(gender==s) %>%
#                            group_by_(.dots=c("segments2", "var")) %>%
#                            summarise(sample_count = n(),
#                                      pop_count = sum(Final_weight)) %>%
#                            mutate(percentage = pop_count*100/sum(pop_count)))
#     
#     # graph
#     
#     ggplot(df2, aes(x=segments2, y=percentage, fill=var)) +
#       geom_bar(stat="identity") + theme_bw() + 
#       scale_fill_brewer() + 
#       labs(y = "Percentage of the population", x = "Segments", caption = paste("Gender:",s),
#            subtitle = paste(sub), title = paste("Percentage of the population by segments",sep=" ")) +
#       # ggtitle(paste("Percentage of the population by Segments",sep=" ")) +
#       theme(axis.text = element_text(size = rel(1.5)),
#             axis.title = element_text(size = rel(1.5)),
#             plot.title = element_text(size = rel(1.6), hjust = 0.5),
#             plot.subtitle = element_text(size = rel(1.2), hjust = 0.5),
#             legend.title=element_blank(),
#             legend.text = element_text(size = rel(0.9)),
#             legend.position = "bottom") +
#       guides(fill=guide_legend(ncol=2))
#     # + coord_flip()
#     
#     ggsave(filename = paste("Output/",folder, "/Graphs/", s,"Segments_",x,"dist.jpeg",sep=""))
#     
#     
#     # export the summarized table
#     names(df2)[which(names(df2)=="var")] <- x
#     write.xlsx(df2, paste("Output/",folder, "/Tables/",s,"Segments_",x,"dist.xlsx",sep=""))
#     
#   }
# }
# 
# ##### PLOTS
# finScope.raw$bk_segments2 <- if_else(finScope.raw$bk_segments=="Banked", 1,
#                                      if_else(finScope.raw$bk_segments=="Bankable", 2,
#                                              if_else(finScope.raw$bk_segments=="Development", 3,
#                                                      if_else(finScope.raw$bk_segments=="Unbankable", 4, 0))))
# finScope.raw$bk_segments2 <- factor(finScope.raw$bk_segments2, levels = 1:4, labels = c("Banked","Bankable","Development", "Unbankable"))
# table(finScope.raw$bk_segments2)
# 
# finScope.raw$mm_segments2 <- if_else(finScope.raw$mm_segments=="MNO users", 1,
#                                      if_else(finScope.raw$mm_segments=="DFS able", 2,
#                                              if_else(finScope.raw$mm_segments=="non-DFS able", 3, 0)))
# finScope.raw$mm_segments2 <- factor(finScope.raw$mm_segments2, levels = 1:3, labels = c("MNO users","DFS able","non-DFS able"))
# table(finScope.raw$mm_segments2)
# 
# 
# ##Distribution of banks and MM users
# #banked
# library(openxlsx)
# finScope.raw <- finScope.raw %>%
#   mutate(banked2 = factor(if_else(banked==1, "Yes", "No")))
# 
# (df2 <- as.data.frame(finScope.raw %>% 
#                         group_by(banked2) %>%
#                         summarise(sample_count = n(),
#                                   pop_count = sum(Final_weight)) %>%
#                         mutate(percentage = pop_count*100/sum(pop_count))))
# write.xlsx(df2, "Output/Second Analysis Iteration/Banking/Tables/banked2_dist.xlsx")
# 
# 
# ggplot(df2, aes(y = percentage, x=1)) + 
#   geom_bar(stat="identity",  position = position_stack(),
#            aes(fill=banked2)) + theme_bw() + 
#   scale_fill_brewer() + 
#   labs(y = "Percentage of the population", x = "Banking Status") + 
#   ggtitle("Percentage of the population by banking status") +
#   theme(axis.text = element_text(size = rel(1.2)),
#         axis.title = element_text(size = rel(1.2)),
#         plot.title = element_text(size = rel(1.4), hjust = 0.5),
#         legend.title=element_blank(),
#         legend.text = element_text(size = rel(1.2)),
#         legend.position = "right")
# ggsave(filename = "Output/Second Analysis Iteration/Banking/Graphs/banked_unbanked.jpeg")
# 
# ## MM
# finScope.raw <- finScope.raw %>%
#   mutate(mm2 = factor(if_else(mm==1, "Yes", "No")))
# 
# (df2 <- as.data.frame(finScope.raw %>% 
#                         group_by(mm2) %>%
#                         summarise(sample_count = n(),
#                                   pop_count = sum(Final_weight)) %>%
#                         mutate(percentage = pop_count*100/sum(pop_count))))
# write.xlsx(df2, "Output/Second Analysis Iteration/MM/Tables/mm_dist.xlsx")
# 
# 
# ggplot(df2, aes(y = percentage, x=1)) + 
#   geom_bar(stat="identity",  position = position_stack(),
#            aes(fill=mm2)) + theme_bw() + 
#   scale_fill_brewer() + 
#   labs(y = "Percentage of the population", x = "Banking Status") + 
#   ggtitle("Percentage of the population by use of MNOs") +
#   theme(axis.text = element_text(size = rel(1.2)),
#         axis.title = element_text(size = rel(1.2)),
#         plot.title = element_text(size = rel(1.4), hjust = 0.5),
#         legend.title=element_blank(),
#         legend.text = element_text(size = rel(1.2)),
#         legend.position = "right")
# ggsave(filename = "Output/Second Analysis Iteration/MM/Graphs/mm_non_mm.jpeg")
# 
# #### Plot by specifc variables #### 
# ### Distribution of the segments
# #banked
# (df2 <- as.data.frame(finScope.raw %>% 
#                         group_by(bk_segments2) %>%
#                         summarise(sample_count = n(),
#                                   pop_count = sum(Final_weight)) %>%
#                         mutate(percentage = pop_count*100/sum(pop_count))))
# write.xlsx(df2, "Output/Second Analysis Iteration/Banking/Tables/banked2_segments.xlsx")
# 
# 
# ggplot(df2, aes(y = percentage, x=bk_segments2)) + 
#   geom_bar(stat="identity",  position = position_stack(),
#            aes(fill=bk_segments2)) + theme_bw() + 
#   scale_fill_brewer() + 
#   labs(y = "Percentage of the population", x = "Banking Segments") + 
#   ggtitle("Percentage of the population by banking segments") +
#   theme(axis.text = element_text(size = rel(1.2)),
#         axis.title = element_text(size = rel(1.2)),
#         plot.title = element_text(size = rel(1.4), hjust = 0.5),
#         legend.title=element_blank(),
#         legend.text = element_text(size = rel(1.2)),
#         legend.position = "right")
# ggsave(filename = "Output/Second Analysis Iteration/Banking/Graphs/banked_segments.jpeg")
# 
# 
# #mm
# (df2 <- as.data.frame(finScope.raw %>% 
#                         group_by(mm_segments2) %>%
#                         summarise(sample_count = n(),
#                                   pop_count = sum(Final_weight)) %>%
#                         mutate(percentage = pop_count*100/sum(pop_count))))
# write.xlsx(df2, "Output/Second Analysis Iteration/MM/Tables/mno_segments.xlsx")
# 
# 
# ggplot(df2, aes(y = percentage, x=mm_segments2)) + 
#   geom_bar(stat="identity",  position = position_stack(),
#            aes(fill=mm_segments2)) + theme_bw() + 
#   scale_fill_brewer() + 
#   labs(y = "Percentage of the population", x = "MNO use segments") + 
#   ggtitle("Percentage of the population by use of MNOs") +
#   theme(axis.text = element_text(size = rel(1.2)),
#         axis.title = element_text(size = rel(1.2)),
#         plot.title = element_text(size = rel(1.4), hjust = 0.5),
#         legend.title=element_blank(),
#         legend.text = element_text(size = rel(1.2)),
#         legend.position = "right")
# ggsave(filename = "Output/Second Analysis Iteration/MM/Graphs/mm_segments.jpeg")
# 
# 
# 
# ### Variable plots
# #----- Capability----
# # convert income values to monthly scale
# # list of variables
# sum.vars <- c("E12_1","E12_2","E12_3","E12_4","E12_5")
# 
# # list of variable labels
# label.sum.vars <- c("Community help","Strangers help","Community involvement sense","Community support each other","Community groupings")
# 
# # list of graph subtitles
# sub.vars <- c("Community help","Strangers help","Community involvement sense","Community support each other","Community groupings")
# 
# 
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # ----- Needs -------
# sum.vars <- c("E8.1","F4.4","CB6.1","CB6.3","CB7.1_1","CB7.1_2","CB7.1_3","CB7.1_4","CB7.1_5","CB9","CB9_other","SC3.1_1","SC3.1_2","SC3.1_3","SC3.1_4","SC3.1_5","SC3.1_6","SC3.1_7","SC3.1_8","SC9")
# label.sum.vars <- c("Costly_events","saving_needs","bank_freq","bank_not_use","bank_branch","bank_ATM","bank_mobile","bank_internet","bank_agent","bank_dislikes","bank_dislikes0","sacco_services1","sacco_services2","sacco_services3","sacco_services4","sacco_services5","sacco_services6","sacco_services7","sacco_services8","sacco_services9")
# sub.vars <- c("Costly events","Which of these serves your saving needs best?","Bank services use frequency","Reason for bank non-usage (recently)","Do you use this for bank transactions: Bank branch","Do you use this for bank transactions: ATM","Do you use this for bank transactions: Mobile phone","Do you use this for bank transactions: Internet","Do you use this for bank transactions: Bank agent","Bank dislikes","Bank dislikes other","Services you do with the Sacco: Save","Services you do with the Sacco: Buy shares","Services you do with the Sacco: Earn dividends","Services you do with the Sacco: Borrow with interest","Services you do with the Sacco:Borrow without interest","Services you do with the Sacco: Welfare/social fund","Services you do with the Sacco: Get farm inputs","Services you do with the Sacco: Get better price for produce","Sacco for not belonging to a sacco")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Motivations
# sum.vars <- c("C19","C26","F3.1_4","F3.2.1_1","F3.2.1_2","F3.2.1_3","F3.2.1_4","F3.2.1_5","F3.2.1_6","F3.2.2","F8.1","G1.1.1")
# label.sum.vars <- c("farm_purpose","phone_nrsn","statement1","save_confidence1","save_confidence2","save_confidence3","save_confidence4","save_confidence5","save_confidence6","save_most_confident","income_asset","satement_borrow")
# sub.vars <- c("Farming/fishing purpose","Reasons for missing mobile phone","You got information about different options before you decided where/how to save","Confidence deling with banks with regards to savings","Confidence deling with MFIs with regards to savings","Confidence deling with SACCOs with regards to savings","Confidence deling with MNOs with regards to savings","Confidence deling with pension fund with regards to savings","Confidence deling with saving groups with regards to savings","What do you feel most confident to deal with regarding your savings?","Bought/built as asset for income generation","You avoid borrowing money if you can")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Control
# sum.vars <- c("C24.1","C24.2","C25","CB2.3", "E5i","H1_5")
# label.sum.vars <- c("own_phone", "sim_card", "mobile_phone_type", "bank_access", "bank_confidence","money_control")
# sub.vars <- c("Mobile phone ownership","Sim card ownership","Type of mobile phone","Whether one can access bank when in need", "You feel comfortable to go into a bank or another financial institution","You have more control over your money if you use cash rather than cards or machines")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Autonomy in decision making
# sum.vars <- c("E1.1","E3.1","MI2.3","MI2.4", "CB2.1_1","I1.3_1")
# label.sum.vars <- c("spend_money", "money_advice", "mfi_access", "mfi_not_reason", "bank_use_name","understand_insurance")
# sub.vars <- c("Whether one has money to spend","Asking for advice on money matters","Whether one can access MFIs when in need","Reason for not being able to access the MFI account", "When you use a bank do you use it - In your name only","You understand how insurance works")
# 
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# }
# 
# # Investment
# 
# sum.vars <- c("C17.2_1","C17.2_2","C17.2_3","C17.2_4","C17.2_5","C17.2_6","C17.2_7","C17.2_8","C17.2_9","G13.1","G13.2.2_1","G13.2.2_2","G13.2.2_3","G13.2.2_4","G13.2.2_5","G13.2.2_6","G13.2.2_7","G13.2.2_8","G13.2.2_9","G13.2.2_10","G13.2.2_11","G13.2.2_12","G13.2.2_13")
# label.sum.vars <-c("hh_basis1","hh_basis2","hh_basis3","hh_basis4","hh_basis5","hh_basis6","hh_basis7","hh_basis8","hh_basis9","googs_obtained1","googs_obtained2","googs_obtained3","googs_obtained4","googs_obtained5","googs_obtained6","googs_obtained7","googs_obtained8","googs_obtained9","googs_obtained10","googs_obtained11","googs_obtained12","googs_obtained13","googs_obtained14")
# sub.vars <- c("Household  involvement basis: Cattle","Household  involvement basis: Goats,sheep, pigs","Household  involvement basis: Other livestock such a poultry","Household  involvement basis: Cash crops","Household  involvement basis: Food crops","Household  involvement basis: Fruits","Household  involvement basis: Vegetables","Household  involvement basis: Aquaculture","Household  involvement basis: Bee-keeping","Obtained goods/services n advance","Use of items such as food, sugar","Use of clothes","Use of personal hair care","Use of farming inputs","Use of farming produce","Use of  Farming equipment","Use of ploughing/harvesting services","Use of Stock for business","Use of Bicycle/motorcycle/car","use of applinaces such as fridge","Use of Building materials","Use of Transport services","Use of motor vehicle repair")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Savings 
# sum.vars <- c("C12.4","E11.2_5","F1","F5","F7.1","F7.2 ","F4.1_15")
# label.sum.vars <-c("land_money_src","save_for_future","saving_desc","saving_rsn","profit_business","last_buy","do_not_save")
# sub.vars <- c("Source of money to buy land","Saving money aside for future expenses","Saving description","Reasons for saving","Started profit making business","Last thing bought/built/started for profit","Do not put cash away but buys things as a way of saving")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Remittances
# sum.vars <- c("H1_3","H1_4","H1_5","H1_6","H2.1_1","H2.1_2","H2.1_3","H2.1_4","H2.1_5","H2.1_6","H2.4_1","H2.5_1","H2.4_2","H2.5_2","H2.4_3","H2.5_3","H2.4_5","H2.5_5","H2.4_6","H2.5_6","H5.1_1","H5.1_2","H5.1_3","H5.1_4")
# label.sum.vars <-c("statement1","statement2","statement3","statement4","statement5","statement6","statement7","statement8","statement9","statement10","statement11","statement12","statement13","statement14","statement15","statement16","statement17","statement18","statement19","statement20","statement21","statement22","statement23","statement24")
# sub.vars <- c("You are prepared to learn how to use new technology","You prefer to pay for goods and services in cash rather than using electronic means","You have more control over your money if you use cash rather than cards or machines","You prefer to use cash because everybody else uses cash","Send Money to Spouse","Send Money to Child","Send Money to Parent","Send Money to Other family member","Send Money to Friend","Send Money to Someone I borrowed money from","Frequency of sending money to Spouse","Method of sending money to Spouse","Frequency of sending money to Child","Method of sending money to Child","Frequency of sending money to Parent","Method of sending money to Parent","Frequency of sending money to Friend","Method of sending money to Friend","Frequency of sending money to Someone I borrowed money from","Method of sending money to Someone I borrowed money from","Receive Money to Spouse","Receive Money to Child","Receive Money to Parent","Receive Money to Other family member")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Risk Management
# finScope.clean$E9.2 <- if_else(finScope.clean$E9.2=="Don\222t know/Have not yet thought about how/where I would get ", 
#                                "Have not yet thought about how", as.character(finScope.clean$E9.2))
# 
# sum.vars <- c("E9.1","E9.2","E9.2_other","E14.1_1","E14.2_1","E14.2_2")
# label.sum.vars <-c("pay_unexpected","coping","coping_other","unforeseen_expenses","coping_unforseen","coping_no_money")
# sub.vars <- c("Whether one paying for UNEXPECTED expenses such as sudden medical emergencies","Ways of coping with unexpected expenses","Ways of coping with unexpected expenses - other","Did you have large unforeseen expenses during the past 12 months","How did you mainly cope when you got large unforeseen expenses","How did you mainly cope when you received no money/less money than you expected")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Credit 
# sum.vars <- c("G1.2.2","G2.1.1","G5.1_1","G5.1_2","G5.1_3","G5.1_4","G5.1_5","G5.1_6","G5.1_7","G5.1_8","G5.1_9","G5.1_10","G5.1_11","G5.1_12","G5.1_13")
# label.sum.vars <-c("confident","borrow","borow_where1","borow_where2","borow_where3","borow_where4","borow_where5","borow_where6","borow_where7","borow_where8","borow_where9","borow_where10","borow_where11","borow_where12","borow_where13")
# sub.vars <- c("Which of these would you feel MOST confident to deal with regarding borrowing/ loans?","Did you borrow money from anybody or any institution during the past 12 months?","Where you borrowed money from : Bank","Where you borrowed money from : Microfinance institution","Where you borrowed money from : SACCO","Where you borrowed money from : Postbank (TPB)","Where you borrowed money from : A Government Scheme","Where you borrowed money from : A mobile money service provider ","Where you borrowed money from : A Pension fund","Where you borrowed money from : Your employer","Where you borrowed money from : Family/friends that you had to pay back","Where you borrowed money from : Family/friends that you did not have to pay back","Where you borrowed money from : Savings group","Where you borrowed money from : Community money lender","Where you borrowed money from : A religious organisation")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Insurance 
# sum.vars <- c("I2","I1.3_2","I1.3_3","I1.3_4","I1.3_5","I6.2")
# label.sum.vars <-c("insurance","statements1","statements2","statements3","statements4","no_insurance_rsn")
# sub.vars <- c("Do you have insurance cover?","You trust insurance companies to pay out when something does go wrong","Insurance is for everyone and you are aware of products that can be accessed by everyone","Insurance can be regarded as long-term saving","You don???t need insurance as you get money if things go wrong","Other than health insurance, what is the main reason you don???t have insurance?")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Cash Flows 
# sum.vars <-c("C18.1_1","C18.1_2","C18.1_3","C18.1_4","C18.1_5","C18.1_6","C18.1_7","C18.1_8","C18.1_9","C18.1_10","C18.1_11","D1","D2.1_1","D2.1_2","D2.1_3","D2.1_4","D2.1_5","D2.1_6","D2.1_7","D2.1_8","D2.1_9","D2.1_10","E4_1","E4_2","E4_3","E4_4","BP1","J1.2", "J1.1.3")
# label.sum.vars <-c("farm_money1","farm_money2","farm_money3","farm_money4","farm_money5","farm_money6","farm_money7","farm_money8","farm_money9","farm_money10","farm_money11","pay_first","money_source1","money_source2","money_source3","money_source4","money_source5","money_source6","money_source7","money_source8","money_source9","money_source10","statement1","statement2","statement3","statement4","pay_bills","imp.provider", "money_manage")
# sub.vars <- c("Money for farming/fishing: I don???t have to buy because I manage with what I have","Money for farming/fishing: I have money to buy it, I use money from other sources ","Money for farming/fishing: Use savings I have","Money for farming/fishing: I sell some of my crops and use the money","Money for farming/fishing: I don???t have to buy because I manage with what I have","Money for farming/fishing: I sell some of my livestock and use the money","Money for farming/fishing: I sell non-agricultural things to get money","Money for farming/fishing: I do piece work/casual jobs to get money to buy it","Money for farming/fishing: I get it in exchange for work I do","Money for farming/fishing: I get it from a buyer to whom I have to sell my crop, livest","Money for farming/fishing: I have to borrow money","What was most important for you to pay or to do first when you get money?","How do you get the money you spend?: Salaries/wages","How do you get the money you spend?: Money from trading/selling","How do you get the money you spend?: Money from providing a service","How do you get the money you spend?: Piece work/Casual labor","How do you get the money you spend?: Rental income","How do you get the money you spend?: Interest from savings, investments etc","How do you get the money you spend?: Pension","How do you get the money you spend?: Social welfare money","How do you get the money you spend?: Rely on someone else","How do you get the money you spend?: Don???t get money ??? someone else pays my expense","You keep track of money that you receive and spend","You know how much money you spent last week","You adjust your expenses according to the money you have available","You often have to spend more money than you have available","Do you have utility bills such electricity, etc. that you have to pay on a regular basis?","Which one of these providers is the most important for you to help you manage your money?", "Most important in managing money")
# 
# sapply(finScope.clean[sum.vars], function(x) table(x))
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# # Household dynamics
# sum.vars <-c("E1.2_1","E1.2_2","E1.2_3","E1.2_4","E2","B1d")
# label.sum.vars <-c("no_money_rsn1","no_money_rsn2","no_money_rsn3","no_money_rsn4","fin.decision","money_relt_most")
# sub.vars <-c("Reason for not having money: Money goes into household expenses","Reason for not having money: Have to give my money to household member/family member","Reason for not having money: Don???t get an income","Reason for not having money: Other","Involvement in household financial decisions","On whose money do you rely on most?")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# # Social Networks 
# 
# sum.vars <-c("E12_3","E12_4","E12_5","F3.2.1_7","F4.1_8","F4.2_8")
# label.sum.vars <-c("statement1","statement2","statement3","save_trust","save_groups", "save_freq")
# sub.vars <-c("People in your community have a strong sense of involvement in the community","People in your community rely on each other for support","There is a strong tendency in your community where you live to form groups","Intitutions you feel confident to deal with regarding SAVINGS?","How they save money: Save with another community group or church", "How often do you save with another community group or church")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# ## More variables
# sum.vars <-c("CB2.2","CB10","MI10","SC9")
# label.sum.vars <-c("CB2.2","CB10","MI10","SC9")
# sub.vars <-c("Main reasons for not using banks in your name","Main reasons you stopped using banks","Main reasons you stopped using MFI","Main reason you don't belong to a SACCO")
# 
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# 
# ### More varaibles - correct graph
# finScope.clean$J1.1.3 <- trimws(as.character(finScope.clean$J1.1.3))
# finScope.clean$J1.1.3_other <- trimws(as.character(finScope.clean$J1.1.3_other))
# 
# finScope.clean <- finScope.clean %>%
#   mutate(J1.1.3_new = if_else(J1.1.3_other=="It helps one to get treatment from anywhere",
#                               "One can get treatment anywhere", "Other")) %>%
#   mutate(J1.1.3_new = if_else(J1.1.3=="Other, specify" , J1.1.3_new, J1.1.3)) 
# 
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#     print("overall distribution")
#     percent_overall("J1.1.3_new", folder = fol, sub = "Most important in managing money")
#     
#     print("distribution by segment")
#     percent_segment("J1.1.3_new", folder = fol, sub = "Most important in managing money", segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender("J1.1.3_new", folder = fol, sub = "Most important in managing money", segment = seg)
#   }
# 
# ### 
# finScope.clean$E14.1_1 <- if_else(finScope.clean$E14.1_1=="Yes (Please specify)","Yes","No")
# 
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   print("overall distribution")
#   percent_overall("E14.1_1", folder = fol, sub = "Did you have large unforeseen expenses during the past 12 months?")
#   
#   print("distribution by segment")
#   percent_segment("E14.1_1", folder = fol, sub = "Did you have large unforeseen expenses during the past 12 months?", segment = seg)
#   
#   print("distribution by segment and gender")
#   percent_segment_gender("E14.1_1", folder = fol, sub = "Did you have large unforeseen expenses during the past 12 months?", segment = seg)
# }
# 
# ### Risk mitigation
# finScope.clean$E9.1_new <- as.character(finScope.clean$E9.1)
# finScope.clean$E9.1_new <- gsub(pattern = " unexpected expenses| unexpected  expenses", "", x = finScope.clean$E9.1_new)
# finScope.clean$E9.1_new <- if_else(finScope.clean$E9.1_new=="Don\222t know (Don\222t read out)","",finScope.clean$E9.1_new)
# tabs("E9.1_new")
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   print("overall distribution")
#   percent_overall("E9.1_new", folder = fol, sub = "Do you pay for UNEXPECTED expenses such as sudden medical emergencies, etc")
#   
#   print("distribution by segment")
#   percent_segment("E9.1_new", folder = fol, sub = "Do you pay for UNEXPECTED expenses such as sudden medical emergencies, etc", segment = seg)
#   
#   print("distribution by segment and gender")
#   percent_segment_gender("E9.1_new", folder = fol, sub = "Do you pay for UNEXPECTED expenses such as sudden medical emergencies, etc", segment = seg)
# }
# 
# ### buy land sourse
# finScope.clean$C12.4_new <- as.character(finScope.clean$C12.4)
# finScope.clean$C12.4_new <- if_else(finScope.clean$C12.4_new=="Other specify","Other",finScope.clean$C12.4_new)
# 
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   print("overall distribution")
#   percent_overall("C12.4_new", folder = fol, sub = "Where did you get most of the money to buy the land?")
#   
#   print("distribution by segment")
#   percent_segment("C12.4_new", folder = fol, sub = "Where did you get most of the money to buy the land?", segment = seg)
#   
#   print("distribution by segment and gender")
#   percent_segment_gender("C12.4_new", folder = fol, sub = "Where did you get most of the money to buy the land?", segment = seg)
# }
# 
# finScope.clean$I6.2_new <- as.character(finScope.clean$I6.2)
# finScope.clean$I6.2_new <- if_else(finScope.clean$I6.2_new=="OOther specify","Other",finScope.clean$I6.2_new)
# 
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   print("overall distribution")
#   percent_overall("I6.2_new", folder = fol, sub = "What is the main reason you do not have insurance?")
#   
#   print("distribution by segment")
#   percent_segment("I6.2_new", folder = fol, sub = "What is the main reason you do not have insurance?", segment = seg)
#   
#   print("distribution by segment and gender")
#   percent_segment_gender("I6.2_new", folder = fol, sub = "What is the main reason you do not have insurance?", segment = seg)
# }
# 
# 
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   print("overall distribution")
#   percent_overall("E1.1", folder = fol, sub = "Do you have money of your own that you can do with as you wish?")
#   
#   print("distribution by segment")
#   percent_segment("E1.1", folder = fol, sub = "Do you have money of your own that you can do with as you wish?", segment = seg)
#   
#   print("distribution by segment and gender")
#   percent_segment_gender("E1.1", folder = fol, sub = "Do you have money of your own that you can do with as you wish?", segment = seg)
# }
# 
# # PSM vars
# sum.vars <- c("connectivity_mobile", "internet_computer_access", 
#                  "literacy_kis","literacy_eng","numeracy_score",
#                  "education", "property_own", "land_own", "PPI_Category",
#                  "income_source_type2", "Cluster", "addition2", "subtraction2",
#               "multiplication2", "division2")
# 
# sub.vars <-c("Connectivity : mobile", "Connectivity : computer/internet", 
#                "Literacy: Kiswahili","Literacy: English","Numeracy score",
#                "Education", "Property ownership", "Land Ownership", "PPI Category",
#                "Income source most relied on", "Location", "Numeracy skills: addition", "Numeracy skills: subtraction",
#              "Numeracy skills: multiplication", "Numeracy skills: Division")
#              
# ## Call the functions
# for(fol in c("Banking", "MM")){
#   if(fol == "Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
# 
# #### Zanzibar only output #####
# finScope.raw <- finScope.raw %>%
#   filter(RU=="Zanzibar")
# 
# ## Call the functions
# for(fol in c("Zanzibar_only/Banking", "Zanzibar_only/MM")){
#   if(fol == "Zanzibar_only/Banking"){
#     seg <- "bk_segments2"
#   } else {
#     seg <- "mm_segments2"
#   }
#   for(i in 1:length(sum.vars)){
#     print("overall distribution")
#     percent_overall(sum.vars[i], folder = fol, sub = sub.vars[i])
#     
#     print("distribution by segment")
#     percent_segment(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#     
#     print("distribution by segment and gender")
#     percent_segment_gender(sum.vars[i], folder = fol, sub = sub.vars[i], segment = seg)
#   }
# } 
# 
