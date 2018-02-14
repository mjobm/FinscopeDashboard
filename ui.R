## ui.R
#  Project: Finsope Analysis Dashboard - November 2017 
#
# Author: Fridah Wanjala
# Date: 6/11/2017


sidebar <-  dashboardSidebar(
  sidebarMenu(width = 12,
    menuItem("Home", tabName="home", icon=icon("home")),
    menuItem("Dashboard" , tabName = "dashboard",
              icon = icon("dashboard")) ,
    br(),'Analysis by segments:',
    menuItem("Banked vs Unbanked", tabName="banked", icon = icon("bank"),
             menuSubItem("Propensity Model Building", tabName="bankedModel", icon = icon("gears")),
             menuSubItem("Charts by Segments", tabName="bankedCharts", icon = icon("bar-chart-o"))
    ),
    menuItem("MNO vs Non-MNO Usage", tabName="mno", icon = icon("mobile-phone"),
             menuSubItem("Propensity Model Building", tabName="mnoModel", icon = icon("gears")),
             menuSubItem("Charts by Segments", tabName="mnoCharts", icon = icon("bar-chart-o"))
    ),
    br(), 'Other Analyses:', 
    menuItem("Maps", tabName="map", icon = icon("map-marker")),
    menuItem("Regional Descriptives", tabName="regional", icon=icon("table")),
    menuItem("Custom Charts", tabName="custom", icon=icon("area-chart"))),
  br(),
  radioButtons("region", label = "Select the segment for for analysis", 
               choices = c("Full sample", "Female sample"), selected = "Full sample"),
  br(), h5("Made by:"),
  div(tags$img(src="busara.png"))
  )

body <-   dashboardBody(
  tabItems(
  ### HOME TAB
  tabItem(tabName = 'home',
  fluidRow(p(includeHTML("www/home.html"))),
  fluidRow(
    box(status = "primary", width = 8, height = 12,
        leafletOutput("map_tz", width="100%",height="520px")))
    ),
    
    ### DASHBOARD TAB
    tabItem(tabName = 'dashboard',
            fluidRow(box(infoBoxOutput("sample",width  = 8), title = "Sample size", 
                         status = "info", footer = "Sample of the population that was interviewed"),
                     box(infoBoxOutput("population",width  = 12), title = "Population represented by the sample",
                         status = "info", footer = "Tanzania population of 16 or older")),
            
            fluidRow(box(title = "Location split", status = "primary", solidHeader = T, collapsible = T,
                         highchartOutput("loc")),
                     box(title = "Gender split", status = "primary", solidHeader = T, collapsible = T,
                         highchartOutput("gender")))
    ),
  
  ### BANKED VS UNBANKED TAB
  tabItem(tabName = 'bankedModel', 
          fluidRow(
            box(title = "Inputs", status = "warning", width = 12,
              checkboxGroupInput("model_vars", "Select variables for the propensity score model",
                                 choices = all.vars, selected = psm.vars, inline = T))),
          fluidRow(
            box(title = "Cut-off scores", status = "warning",width = 8,
                    sliderInput("cut_off", "Select cutoff scores for Bankable and Developement segments", 
                                min = 0, max = 0.6, step = 0.02, value = c(0.02, 0.1))),
                box(title = "Graphing parameters", status = "warning",width = 4,
                    checkboxGroupInput("graph_by", "Select parameters to graph by", choices = c("Segments" = "banked_segments", "Gender" = "gender"),
                                selected = c("Segments" = "banked_segments"), inline = F))
                ),
          
            fluidRow(
              box(status = "primary", width = 6, height = 4,
                  highchartOutput("unbanked_hist", height = 350)),
              box(status = "primary", width = 6, height =4,
                  highchartOutput("banked_segments",height = 350)))
  ),
  
  tabItem(tabName = 'bankedCharts',
          fluidRow(
            box(title = "Crosstab variables", status = "warning", width = 12,
                selectizeInput("cross_vars", "Search for a specific variable to crosstab with", choices = vars.list3, selected = "PPI_Category",
                               options = list(maxItems = 1, placeholder = 'Select a variable name')))),
          fluidRow(
            box(title = "Distribution by segments", status = "primary", width = 6, height =4,
                highchartOutput("banked_crosstab1",height = 350)), 
            box(title = "Distribution by segments and gender", status = "primary", width = 6, height = 4,
                htmlOutput("banked_crosstab2")))
  ),       
  
  ### MNO VS NON-MNO TAB
  tabItem(tabName = 'mnoModel', 
          fluidRow(
            box(title = "Inputs", status = "warning", width = 12,
                checkboxGroupInput("mno_model_vars", "Select variables for the propensity score model",
                                   choices = all.vars, selected = psm.vars, inline = T))
            ),
          
          fluidRow(
            box(title = "Cut-off scores", status = "warning",width = 8,
                sliderInput("mno_cut_off", "Select cutoff score for DFS and non-DFS able segments", 
                            min = 0, max = 0.6, step = 0.02, value = 0.3)),
            box(title = "Graphing parameters", status = "warning",width = 4,
                checkboxGroupInput("mno_graph_by", "Select parameters to graph by", choices = c("Segments" = "mno_segments", "Gender" = "gender"),
                                   selected = c("Segments" = "mno_segments"), inline = F))
          ),
          
          fluidRow(
            box(title = "Distribution of the predicted propensity scores", status = "primary", width = 6, height = 4,
                highchartOutput("mno_hist", height = 350)),
            box(title = "Distribution of the segments", status = "primary", width = 6, height =4,
                highchartOutput("mno_segments",height = 350)))
          ),
  
  tabItem(tabName = 'mnoCharts',
          fluidRow(
            box(title = "Crosstab variables", status = "warning", width = 12,
                selectizeInput("mno_cross_vars", "Search for a specific variable to crosstab with", choices = vars.list3, selected = "PPI_Category",
                               options = list(maxItems = 1, placeholder = 'Select a variable name')))),
          
          fluidRow(
            box(title = "Distribution by segments", status = "primary", width = 6, height =4,
                highchartOutput("mno_crosstab1",height = 350)), 
            box(title = "Distribution by segments and gender", status = "primary", width = 6, height = 4,
                htmlOutput("mno_crosstab2")))
  ),   
  
  
  ### Other Analyses TAB
  # maps
  tabItem(tabName = 'map',
          tabsetPanel(
            tabPanel('Points of selected variables on a map ',
            fluidRow(
             box(status = "primary", width = 12,
                           selectInput("mark_var", "Select one variable", choices = map_vars, 
                                       selected = "use.formal"))),
          fluidRow(
            box(status = "primary", width = 8, height = 12,
                leafletOutput("map_dist_tz", width="100%",height="520px")),
            box(status = "primary", width = 4,
                leafletOutput("map_dist_zanzi", width="100%",height="520px")))),
          
          tabPanel('Heat map of selected variables',
                   fluidRow(
                     box(status = "primary", width = 12, 
                         selectInput("mark_var2", "Select one variable", choices = map_vars, 
                                     selected = "use.formal"))),
                     fluidRow(
                       box(status = "primary", width = 8,
                           leafletOutput("map_chloro_tz", width="100%",height="520px")),
                       box(status = "primary", width = 4,
                           leafletOutput("map_chloro_zanzi", width="100%",height="520px"))))
          )),
  
  # regional descriptives
  tabItem(tabName = 'regional',
          fluidRow(
            box(title = "Set of variables", status = "warning", width = 12,
                selectizeInput("reg_cross_vars", "Search for a specific variable to crosstab with region", choices = vars.list3, selected = "PPI_Category",
                               options = list(maxItems = 1, placeholder = 'Select a variable name')))),
          
          fluidRow(
            box(title = "Distribution (%) of variables by region", status = "primary", width = 10, height = 4,
                dataTableOutput("reg_crosstab")),
            box(title = "Download the data (as a csv)", status = "info", width = 2, height = 4,
                downloadButton("downloadData", "Download")))     
  ),  
  
  # custom tables
  tabItem(tabName = 'custom', 
          fluidRow(
            box(title = "Set of variables", status = "warning", width = 12,
                selectizeInput("cust_cross_vars", "Search for a specific variable to check its distribution", choices = vars.list3, selected = "PPI_Category",
                               options = list(maxItems = 1, placeholder = 'Select a variable name')))),
          
          fluidRow(
            box(title = "Distribution of selected variable", status = "primary", width = 6, height =4,
                highchartOutput("cust_crosstab1",height = 350)), 
            box(title = "Distribution of selected variable by gender", status = "primary", width = 6, height = 4,
                htmlOutput("cust_crosstab2")))    
  ) 
))


# Define Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Finscope Analysis Dashboard 2017", titleWidth = 300),
  sidebar,
  body
)

