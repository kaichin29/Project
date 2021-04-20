# Install devtools from CRAN
install.packages("devtools")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("r-lib/devtools")


devtools::install_github("timelyportfolio/d3treeR")

packages = c('shiny',
             'shinydashboard',
             'tidyverse',
             'ggstatsplot',
             'ggplot2',
             'readr',
             'treemap',
             'RColorBrewer',
             'ggthemes',
             'forcats',
             'GGally',
             'plotly',
             'lubridate',
             'knitr',
             'htmlwidgets',
             'htmltools',
             'shinythemes',
             'shinyWidgets',
             'rlang',
             'shinycssloaders',
             'dplyr','qicharts2','ggQC','qcc','rmarkdown','gridExtra' )
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

## loading the data
PM_AGG <- read_csv("data/PM_AGG.csv")

## filtering the data

## filtering variables
max_date <- as.Date("2019-03-31")
min_date <- as.Date("2019-03-01")

date_range <- seq(min_date, max_date, "days")

options(spinner.type = 8)

ui <- tagList(
    navbarPage(
        theme = shinytheme("cerulean"), 
        "Understanding Prime Mover (PM) Waiting Time in Yard",
        tabPanel("Home",
                 sidebarPanel(
                     
                     ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Introduction",
                                  h4("Introduction"),
                                  p("Maritime trade has been the backbone of international trade as they account for approximately 92% of world trade. PSA Singapore handles about a fifth of the world’s transhipped containers as the world’s busiest transhipment port. For more than four decades, PSA continuously developed and upgraded its container handling infrastructure, pioneered new systems and processes, and streamlined operations to meet the rapid growth in its container terminal business as part of the strive for operational excellence."),
                                  br(),
                                  p("Upon a vessel’s arrival at a berth in the container terminal, containers are discharged from and loaded onto it. A typical discharging operation starts with a quay crane picking up a container from the vessel and placing it onto a PM, which will then transport to a storage yard. At the yard, a yard crane picks up the container from the PM and shifts it to a designated spot. Loading operations involve the transporting of containers in the opposite direction, from the yard to the vessel."),
                                  br(),
                                  p("Therefore, PM productivity is of key interest to PSA as it is the main driver for the time taken to load and unload vessels. PM Productivity is defined as the sum of the total number of containers handled divided by the total hours. The following are the key terms which PSA uses to define PM productivity."),
                                  p("Productivity = Total Containers Handled / Total Time", align = "center"),
                                  p("Total Time = Sum (Est. Travel Time + Est. Wait Time + Non Work Time)", align = "center"),
                                  br(),
                                  
                                  p("Total Time is defined as the time difference between the two operation activities."),
                                  p("Estimated Travel Time is the duration between two locations based on distance matrix with fixed speed limit/hr."),
                                  p("Non-Work Time is the time taken for a change of driver, meal break, and PM breakdown (if any). "),
                                  p("Estimated Wait Time = Total Time – Non-Work Time – Est. Travel Time"),
                                  br(),
                                  h4("Motivation"),
                                  p("The study objective is to seek insight from Prime Mover (PM) Operations records to identify common characteristics exhibited by PM with high and low waiting times, through understanding of PM events and operational data. This, in turn, enables us to pinpoint and identify correlated attributes and embark on further study to improve the overall productivity of PM operations and resource utilisation through active targeting of activities contributing to the PM waiting time."),
                                  br(),


                                  ),
                         
                         tabPanel("Glossary",
                                  h4("Key Definitions"),
                                  p("The data source used is provided by PSA Singapore's PM Ops anoymised dataset that contains operation event records from March 2019. The variables contained within the dataset is explained in the table below."),
                                  br(),
                                  img(src = "Glossary.PNG"),
                                  ),
                         
                         tabPanel("User Guide",
                                  h4("User Guide"),
                                  p("Please use the variables in the side bar to change the plot arguments. Data can be filtered using the various checkboxes and dropdown lists in the main panel. "),
                                  br(),
                                  p("Please refer to the user guide for more details on how to use the Shiny application", a("Link", href="file://blabla.html")), 
                                  em("Please note that due to the size of the data set, some graphs will take time to load.", style = "font-si9pt"),
                                  em("Click link for full user guide",  style = "font-si9pt")
                                  )
                         )   
                 )
        ),
        tabPanel("Exploratory Data Analysis",
                 tabsetPanel(
                     tabPanel("Treemap",
                         sidebarPanel(
                             selectInput(inputId = "tree_primary", 
                                         label =  "Choose your primary variable:",
                                         choices = c("Shift" = "EVENT_SHIFT_I",
                                                     "Day" = "day"),
                                         selected = "EVENT_SHIFT_I"),
                             
                             selectInput(inputId = "tree_secondary", 
                                         label =  "Choose your secondary variable:",
                                         choices = c("Day" = "day",
                                                      "Hour" = "hour"),
                                         selected = "day"),
                             
                             selectInput(inputId = "vsize", 
                                         label =  "Choose your independent variable:",
                                         choices = c("Waiting Time" = "PM_WAIT_TIME_Q",
                                                     "Travelling Time" = "PM_TRAVEL_TIME_Q"),
                                         selected = "PM_WAIT_TIME_Q"),
                             br(),
                             actionButton("goTree", "Apply Changes", class = "btn-success"),
                         ),
                         mainPanel(
                           fluidRow(
                             column(width = 6,
                                    prettyCheckboxGroup(inputId = "filterLength_tree",
                                                        label =  "Select Length of Container:",
                                                        choices = sort(unique(PM_AGG$LENGTH_Q)),
                                                        icon = icon("check-square-o"), 
                                                        status = "primary",
                                                        outline = TRUE, 
                                                        selected = sort(unique(PM_AGG$LENGTH_Q))),
                                    
                                    prettyCheckboxGroup(inputId = "filterShift_tree",
                                                        label =  "Select shift:",
                                                        choices = sort(unique(PM_AGG$EVENT_SHIFT_I)),
                                                        icon = icon("check-square-o"), 
                                                        status = "primary",
                                                        outline = TRUE,
                                                        selected = sort(unique(PM_AGG$EVENT_SHIFT_I))
                                                        ),
                                    
                                    prettyCheckboxGroup(inputId = "filterDG_tree",
                                                        label =  "Select DG status:",
                                                        choices = sort(unique(PM_AGG$DG)),
                                                        icon = icon("check-square-o"), 
                                                        status = "primary",
                                                        outline = TRUE,
                                                        selected = sort(unique(PM_AGG$DG))
                                                        ),
                                    
                                    prettyCheckboxGroup(inputId = "filterReefer_tree",
                                                        label =  "Select Reefer status:",
                                                        choices = sort(unique(PM_AGG$Reefer)),
                                                        icon = icon("check-square-o"), 
                                                        status = "primary",
                                                        outline = TRUE, 
                                                        selected = sort(unique(PM_AGG$Reefer))
                                                        )
                                    ),
                             column(width = 6,
                              
                                    pickerInput(inputId = "filterTerminal_tree",
                                                label = "Select Terminal:",
                                                choices = sort(unique(PM_AGG$Terminal)),
                                                multiple = TRUE, 
                                                options = list(`actions-box` = TRUE),
                                                selected = sort(unique(PM_AGG$Terminal))),
                                    
                                    pickerInput(inputId = "filterMove_tree",
                                                label = "Select Movement status:",
                                                choices = sort(unique(PM_AGG$MOVE_OP_C)),
                                                multiple = TRUE, 
                                                options = list(`actions-box` = TRUE),
                                                selected = sort(unique(PM_AGG$MOVE_OP_C))),
                                    
                                    pickerInput(inputId = "filterHour_tree",
                                                label = "Select Hour:",
                                                choices = sort(unique(PM_AGG$hour)),
                                                multiple = TRUE, 
                                                options = list(`actions-box` = TRUE),
                                                selected = sort(unique(PM_AGG$hour))),
                                    
                                    pickerInput(inputId = "filterDay_tree",
                                                label = "Select Day:",
                                                choices = sort(unique(PM_AGG$day)),
                                                multiple = TRUE, 
                                                options = list(`actions-box` = TRUE),
                                                selected = sort(unique(PM_AGG$day)))
                                    ),
                             sliderTextInput(inputId = "filterDate_tree",
                                             label = "Choose a date range",
                                             choices = date_range, 
                                             from_min = as.Date('2019-02-28'),
                                             to_max = as.Date('2019-03-31'),
                                             selected = c(as.Date('2019-03-03'), as.Date('2019-03-10'))
                                             ),
                             br(),
                             withSpinner(d3tree2Output("treemap"))
                         )
                         )
                 ),
                     
                     tabPanel("Bar charts",
                              sidebarPanel(
                                  selectInput(inputId = "bar_var", 
                                              label =  "Choose your primary variable:",
                                              choices = c("LENGTH of Container" = "LENGTH_Q",
                                                          "Shift" = "EVENT_SHIFT_I",
                                                          "DG" = "DG", 
                                                          "Reefer" = "Reefer",
                                                          "Terminal ID" = "Terminal",
                                                          "Day" = "day",
                                                          "Hour" = "hour"),
                                              selected = "LENGTH_Q"),
                                  
                                  selectInput(inputId = "bar_fill", 
                                              label =  "Choose your fill variable:",
                                              choices = c("LENGTH of Container" = "LENGTH_Q",
                                                          "Shift" = "EVENT_SHIFT_I",
                                                          "DG" = "DG", 
                                                          "Reefer" = "Reefer",
                                                          "Terminal ID" = "Terminal",
                                                          "Day" = "day",
                                                          "Hour" = "hour"),
                                              selected = "Terminal"),
                                  
                                  selectInput(inputId = "bar_facet", 
                                              label =  "Choose your grouping variable:",
                                              choices = c("LENGTH of Container" = "~LENGTH_Q",
                                                          "Shift" = "~EVENT_SHIFT_I",
                                                          "DG" = "~DG", 
                                                          "Reefer" = "~Reefer",
                                                          "Terminal ID" = "~Terminal",
                                                          "Day" = "~day",
                                                          "Hour" = "~hour",
                                                          "All" = "~."),
                                              
                                              selected = "~."),
                                  selectInput(inputId = "bar_type", 
                                              label =  "Choose your bar type:",
                                              choices = c("Stacked" = "stack",
                                                          "Side-by-side" = "dodge"),
                                              selected = "dodge"),
                                  
                                  selectInput(inputId = "bar_scale", 
                                              label =  "Choose your y-axis type:",
                                              choices = c("Free Y-axis" = "free_y",
                                                          "Standardised axis" = "fixed"),
                                              selected = "free_y"),
                                  br(),
                                  actionButton("goBar", "Apply Changes", class = "btn-success"),
                                  ),
                              mainPanel(
                                fluidRow(
                                  column(width = 6,
                                         prettyCheckboxGroup(inputId = "filterLength_bar",
                                                             label =  "Select Length of Container:",
                                                             choices = sort(unique(PM_AGG$LENGTH_Q)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE, 
                                                             selected = sort(unique(PM_AGG$LENGTH_Q))),
                                         
                                         prettyCheckboxGroup(inputId = "filterShift_bar",
                                                             label =  "Select shift:",
                                                             choices = sort(unique(PM_AGG$EVENT_SHIFT_I)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE,
                                                             selected = sort(unique(PM_AGG$EVENT_SHIFT_I))
                                         ),
                                         
                                         prettyCheckboxGroup(inputId = "filterDG_bar",
                                                             label =  "Select DG status:",
                                                             choices = sort(unique(PM_AGG$DG)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE,
                                                             selected = sort(unique(PM_AGG$DG))
                                         ),
                                         
                                         prettyCheckboxGroup(inputId = "filterReefer_bar",
                                                             label =  "Select Reefer status:",
                                                             choices = sort(unique(PM_AGG$Reefer)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE, 
                                                             selected = sort(unique(PM_AGG$Reefer))
                                         )
                                  ),
                                  column(width = 6,
                                         
                                         pickerInput(inputId = "filterTerminal_bar",
                                                     label = "Select Terminal:",
                                                     choices = sort(unique(PM_AGG$Terminal)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$Terminal))
                                         ),
                                         
                                         pickerInput(inputId = "filterMove_bar",
                                                     label = "Select Movement status:",
                                                     choices = sort(unique(PM_AGG$MOVE_OP_C)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$MOVE_OP_C))
                                         ),
                                         
                                         pickerInput(inputId = "filterHour_bar",
                                                     label = "Select Hour:",
                                                     choices = sort(unique(PM_AGG$hour)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$hour))
                                         ),
                                         
                                         pickerInput(inputId = "filterDay_bar",
                                                     label = "Select Day:",
                                                     choices = sort(unique(PM_AGG$day)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$day))
                                         )
                                  ),
                                  sliderTextInput(inputId = "filterDate_bar",
                                                  label = "Choose a date range",
                                                  choices = date_range, 
                                                  from_min = as.Date('2019-02-28'),
                                                  to_max = as.Date('2019-03-31'),
                                                  selected = c(as.Date('2019-03-03'), as.Date('2019-03-10'))
                                  ),
                                  br(),
                                  withSpinner(plotlyOutput("bar"))
                                )
                                
                                        )
                              ),
                     tabPanel("Histograms",
                              sidebarPanel(
                                  selectInput(inputId = "hist_var", 
                                             label =  "Choose your primary variable:",
                                             choices = c("Waiting Time" = "PM_WAIT_TIME_Q",
                                                         "Travel Time" = "PM_TRAVEL_TIME_Q"),
                                             selected = "PM_WAIT_TIME_Q"),
                                  
                                  selectInput(inputId = "hist_fill", 
                                              label =  "Choose your fill variable:",
                                              choices = c("LENGTH of Container" = "LENGTH_Q",
                                                          "Shift" = "EVENT_SHIFT_I",
                                                          "DG" = "DG", 
                                                          "Reefer" = "Reefer",
                                                          "Terminal ID" = "Terminal",
                                                          "Day" = "day",
                                                          "Hour" = "hour"),
                                              selected = "Terminal"),
                                  
                                  selectInput(inputId = "hist_facet", 
                                              label =  "Choose your grouping variable:",
                                              choices = c("LENGTH of Container" = "~LENGTH_Q",
                                                          "Shift" = "~EVENT_SHIFT_I",
                                                          "DG" = "~DG", 
                                                          "Reefer" = "~Reefer",
                                                          "Terminal ID" = "~Terminal",
                                                          "All" = "~.",
                                                          "Day" = "~day",
                                                          "Hour" = "~hour"),
                                              selected = "~."),
                                  
                                  selectInput(inputId = "hist_scale", 
                                              label =  "Choose your y-axis type:",
                                              choices = c("Free Y-axis" = "free_y",
                                                          "Standardised axis" = "fixed"),
                                              selected = "free_y"),
                                  
                                  sliderInput(inputId = "hist_binsize",
                                              "Choose a bin size",
                                              min = 5, max = 30, value = 20),
                                  
                                  sliderInput(inputId = "hist_limit",
                                              "Choose a reference point",
                                              min = 0, max = 30, value = 15),
                                  br(),
                                  actionButton("goHist", "Apply Changes", class = "btn-success"),
                                  
                              ),
                              mainPanel(
                                fluidRow(
                                  column(width = 6,
                                         prettyCheckboxGroup(inputId = "filterLength_hist",
                                                             label =  "Select Length of Container:",
                                                             choices = sort(unique(PM_AGG$LENGTH_Q)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE, 
                                                             selected = sort(unique(PM_AGG$LENGTH_Q))),
                                         
                                         prettyCheckboxGroup(inputId = "filterShift_hist",
                                                             label =  "Select shift:",
                                                             choices = sort(unique(PM_AGG$EVENT_SHIFT_I)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE,
                                                             selected = sort(unique(PM_AGG$EVENT_SHIFT_I))
                                         ),
                                         
                                         prettyCheckboxGroup(inputId = "filterDG_hist",
                                                             label =  "Select DG status:",
                                                             choices = sort(unique(PM_AGG$DG)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE,
                                                             selected = sort(unique(PM_AGG$DG))
                                         ),
                                         
                                         prettyCheckboxGroup(inputId = "filterReefer_hist",
                                                             label =  "Select Reefer status:",
                                                             choices = sort(unique(PM_AGG$Reefer)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE, 
                                                             selected = sort(unique(PM_AGG$Reefer))
                                         )
                                  ),
                                  column(width = 6,
                                         
                                         pickerInput(inputId = "filterTerminal_hist",
                                                     label = "Select Terminal:",
                                                     choices = sort(unique(PM_AGG$Terminal)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$Terminal))
                                         ),
                                         
                                         pickerInput(inputId = "filterMove_hist",
                                                     label = "Select Movement status:",
                                                     choices = sort(unique(PM_AGG$MOVE_OP_C)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$MOVE_OP_C))
                                         ),
                                         
                                         pickerInput(inputId = "filterHour_hist",
                                                     label = "Select Hour:",
                                                     choices = sort(unique(PM_AGG$hour)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$hour))
                                         ),
                                         
                                         pickerInput(inputId = "filterDay_hist",
                                                     label = "Select Day:",
                                                     choices = sort(unique(PM_AGG$day)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$day))
                                         )
                                  ),
                                  sliderTextInput(inputId = "filterDate_hist",
                                                  label = "Choose a date range",
                                                  choices = date_range, 
                                                  from_min = as.Date('2019-02-28'),
                                                  to_max = as.Date('2019-03-31'),
                                                  selected = c(as.Date('2019-03-03'), as.Date('2019-03-10'))
                                  ),
                                  br(),
                                  withSpinner(plotlyOutput("hist"))
                                )
                                
                                        )
                              ),
                     tabPanel("")
                  
                 ),
        ),
        tabPanel("Confirmatory Data Analysis",
                 tabsetPanel(
                     tabPanel("Scatter Plot",
                              sidebarPanel(
                                
                                  radioButtons("scatter_group",
                                               "Do you wish to break down by variable?",
                                               choices = c("No" = "single",
                                                           "Yes" = "group"),
                                               selected = "single"),
                                  
                                
                                  selectInput(inputId = "scatter_type", 
                                              label =  "Choose the type of accompanying plot:",
                                              choices = c("Histograms" = "histogram",
                                                          "Boxplots" = "boxplot"),
                                              selected = "boxplot"), 
                                  
                                  conditionalPanel(condition = "input.scatter_group =='group'",
                                                   selectInput(inputId = "scatter_var", 
                                                               label =  "Choose your grouping variable:",
                                                               choices = c("LENGTH of Container" = "LENGTH_Q",
                                                                           "Shift" = "EVENT_SHIFT_I",
                                                                           "DG" = "DG", 
                                                                           "Reefer" = "Reefer",
                                                                           "Terminal ID" = "Terminal",
                                                                           "Day" = "day",
                                                                           "Hour" = "hour"),
                                                               selected = "Terminal"),
                                                   ),
                                  actionButton("goScatter", "Apply Changes", class = "btn-success")
                                  
                              ),
                              
                              mainPanel(
                                fluidRow(
                                  column(width = 6,
                                         prettyCheckboxGroup(inputId = "filterLength_scatter",
                                                             label =  "Select Length of Container:",
                                                             choices = sort(unique(PM_AGG$LENGTH_Q)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE, 
                                                             selected = sort(unique(PM_AGG$LENGTH_Q))),
                                         
                                         prettyCheckboxGroup(inputId = "filterShift_scatter",
                                                             label =  "Select shift:",
                                                             choices = sort(unique(PM_AGG$EVENT_SHIFT_I)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE,
                                                             selected = sort(unique(PM_AGG$EVENT_SHIFT_I))
                                         ),
                                         
                                         prettyCheckboxGroup(inputId = "filterDG_scatter",
                                                             label =  "Select DG status:",
                                                             choices = sort(unique(PM_AGG$DG)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE,
                                                             selected = sort(unique(PM_AGG$DG))
                                         ),
                                         
                                         prettyCheckboxGroup(inputId = "filterReefer_scatter",
                                                             label =  "Select Reefer status:",
                                                             choices = sort(unique(PM_AGG$Reefer)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE, 
                                                             selected = sort(unique(PM_AGG$Reefer))
                                         )
                                  ),
                                  column(width = 6,
                                         
                                         pickerInput(inputId = "filterTerminal_scatter",
                                                     label = "Select Terminal:",
                                                     choices = sort(unique(PM_AGG$Terminal)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$Terminal))
                                         ),
                                         
                                         pickerInput(inputId = "filterMove_scatter",
                                                     label = "Select Movement status:",
                                                     choices = sort(unique(PM_AGG$MOVE_OP_C)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$MOVE_OP_C))
                                         ),
                                         
                                         pickerInput(inputId = "filterHour_scatter",
                                                     label = "Select Hour:",
                                                     choices = sort(unique(PM_AGG$hour)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$hour))
                                         ),
                                         
                                         pickerInput(inputId = "filterDay_scatter",
                                                     label = "Select Day:",
                                                     choices = sort(unique(PM_AGG$day)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$day))
                                         )
                                  ),
                                  sliderTextInput(inputId = "filterDate_scatter",
                                                  label = "Choose a date range",
                                                  choices = date_range, 
                                                  from_min = as.Date('2019-02-28'),
                                                  to_max = as.Date('2019-03-31'),
                                                  selected = c(as.Date('2019-03-03'), as.Date('2019-03-10'))
                                  ),
                                  br(),
                                  withSpinner(plotOutput("scatter"))
                                )
                              )),
                     
                     tabPanel("Box Plots",
                              sidebarPanel(
                                
                                radioButtons("box_group",
                                             "Do you wish to break down by variable?",
                                             choices = c("No" = "single",
                                                         "Yes" = "group"),
                                             selected = "single"),
                                
                                selectInput(inputId = "box_var",
                                            label = "Choose your variable", 
                                            choices = c("Length of Container" = "LENGTH_Q",
                                                        "Shift" = "EVENT_SHIFT_I",
                                                        "DG" = "DG",
                                                        "Reefer" = "Reefer",
                                                        "Terminal ID" = "Terminal",
                                                        "Day" = "day",
                                                        "Hour" = "hour"),
                                            selected = "LENGTH_Q"
                                            ),
                                
                                selectInput(inputId = "box_type", 
                                            label =  "Choose the type of plot:",
                                            choices = c("Violin Plots" = "violin",
                                                        "Box Plots" = "box",
                                                        "Box-Violin Plots" = "boxviolin"),
                                            selected = "boxplot"),
                                
                                conditionalPanel(condition = "input.box_group =='group'",
                                                 selectInput(inputId = "box_groupvar", 
                                                             label =  "Choose your grouping variable:",
                                                             choices = c("LENGTH of Container" = "LENGTH_Q",
                                                                         "Shift" = "EVENT_SHIFT_I",
                                                                         "DG" = "DG", 
                                                                         "Reefer" = "Reefer",
                                                                         "Terminal ID" = "Terminal",
                                                                         "Day" = "day",
                                                                         "Hour" = "hour"),
                                                             selected = "Terminal"),
                                                 ),
                                br(),
                                actionButton("goBox", "Apply Changes", class = "btn-success"),
                                
                              ),
                              
                              mainPanel(
                                fluidRow(
                                  column(width = 6,
                                         prettyCheckboxGroup(inputId = "filterLength_box",
                                                             label =  "Select Length of Container:",
                                                             choices = sort(unique(PM_AGG$LENGTH_Q)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE, 
                                                             selected = sort(unique(PM_AGG$LENGTH_Q))),
                                         
                                         prettyCheckboxGroup(inputId = "filterShift_box",
                                                             label =  "Select shift:",
                                                             choices = sort(unique(PM_AGG$EVENT_SHIFT_I)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE,
                                                             selected = sort(unique(PM_AGG$EVENT_SHIFT_I))
                                         ),
                                         
                                         prettyCheckboxGroup(inputId = "filterDG_box",
                                                             label =  "Select DG status:",
                                                             choices = sort(unique(PM_AGG$DG)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE,
                                                             selected = sort(unique(PM_AGG$DG))
                                         ),
                                         
                                         prettyCheckboxGroup(inputId = "filterReefer_box",
                                                             label =  "Select Reefer status:",
                                                             choices = sort(unique(PM_AGG$Reefer)),
                                                             icon = icon("check-square-o"), 
                                                             status = "primary",
                                                             outline = TRUE, 
                                                             selected = sort(unique(PM_AGG$Reefer))
                                         )
                                  ),
                                  column(width = 6,
                                         
                                         pickerInput(inputId = "filterTerminal_box",
                                                     label = "Select Terminal:",
                                                     choices = sort(unique(PM_AGG$Terminal)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$Terminal))
                                         ),
                                         
                                         pickerInput(inputId = "filterMove_box",
                                                     label = "Select Movement status:",
                                                     choices = sort(unique(PM_AGG$MOVE_OP_C)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$MOVE_OP_C))
                                         ),
                                         
                                         pickerInput(inputId = "filterHour_box",
                                                     label = "Select Hour:",
                                                     choices = sort(unique(PM_AGG$hour)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$hour))
                                         ),
                                         
                                         pickerInput(inputId = "filterDay_box",
                                                     label = "Select Day:",
                                                     choices = sort(unique(PM_AGG$day)),
                                                     multiple = TRUE, 
                                                     options = list(`actions-box` = TRUE),
                                                     selected = sort(unique(PM_AGG$day))
                                         )
                                  ),
                                  sliderTextInput(inputId = "filterDate_box",
                                                  label = "Choose a date range",
                                                  choices = date_range, 
                                                  from_min = as.Date('2019-02-28'),
                                                  to_max = as.Date('2019-03-31'),
                                                  selected = c(as.Date('2019-03-03'), as.Date('2019-03-10'))
                                  ),
                                  br(),
                                  withSpinner(plotOutput("box"))
                                )
                              )
                              )
                 ),
        tabPanel("Parato Analysis"),
        ),
        tabPanel("Control Chart Analysis"),
        tabPanel("Parato Analysis")
        
    )
)




##  server

server <- function(input, output, session) {
    
        

    output$treemap <- renderD3tree2({
      
      input$goTree
      
      isolate(
      PM_tree <- PM_AGG %>% 
        filter(LENGTH_Q %in% input$filterLength_tree) %>%
        filter(EVENT_SHIFT_I %in% input$filterShift_tree ) %>%
        filter(DG %in% input$filterDG_tree ) %>%
        filter(Reefer %in% input$filterReefer_tree ) %>%
        filter(Terminal %in% input$filterTerminal_tree) %>%
        filter(MOVE_OP_C %in% input$filterMove_tree ) %>%
        filter(hour %in% input$filterHour_tree ) %>%
        filter(day %in% input$filterDay_tree ) %>%
        filter(date >= input$filterDate_tree[1] & date <= input$filterDate_tree[2])
      )
      PM_tree$count <- 1
        
        
      input$goTree
      isolate(
        tree <- treemap(PM_tree, index = c(input$tree_primary, input$tree_secondary), vSize = "count", vColor = sum(input$vsize), palette = "Blues", title = "Treemap for PM Waiting time by CNTR Specifications and Shift", fontsize.labels = c(15,10))
      )
      return(d3tree2(tree, rootname = "Container Types"))
        
    })
    output$bar <- renderPlotly({  
      
      input$goBar
      
      
      isolate(
      PM_group <- PM_AGG %>% 
        filter(LENGTH_Q %in% input$filterLength_bar) %>%
        filter(EVENT_SHIFT_I %in% input$filterShift_bar ) %>%
        filter(DG %in% input$filterDG_bar ) %>%
        filter(Reefer %in% input$filterReefer_bar ) %>%
        filter(Terminal %in% input$filterTerminal_bar) %>%
        filter(MOVE_OP_C %in% input$filterMove_bar ) %>%
        filter(hour %in% input$filterHour_bar ) %>%
        filter(day %in% input$filterDay_bar ) %>%
        filter(date >= input$filterDate_bar[1] & date <= input$filterDate_bar[2])
      )
      
      bar_ID <- ggplot(data = PM_group, aes_string(x = input$bar_var, fill = input$bar_fill))+
            geom_bar(position = input$bar_type, alpha = 0.75, width = 0.5)+ 
            labs(title = paste0("Count of ", input$bar_var, " by ", input$bar_fill, " grouped by ", input$bar_facet))+
            theme_few()+
            facet_wrap(as.formula(input$bar_facet), scales = input$bar_scale)+
            scale_fill_brewer(palette = "BuPu")
    
    barplot <- ggplotly(bar_ID)
    return(barplot)
    })
    
    output$hist <- renderPlotly({
        
      input$goHist
      
      isolate(
      PM_group <- PM_AGG %>% 
        filter(LENGTH_Q %in% input$filterLength_hist) %>%
        filter(EVENT_SHIFT_I %in% input$filterShift_hist ) %>%
        filter(DG %in% input$filterDG_hist ) %>%
        filter(Reefer %in% input$filterReefer_hist ) %>%
        filter(Terminal %in% input$filterTerminal_hist) %>%
        filter(MOVE_OP_C %in% input$filterMove_hist ) %>%
        filter(hour %in% input$filterHour_hist ) %>%
        filter(day %in% input$filterDay_hist ) %>%
        filter(date >= input$filterDate_hist[1] & date <= input$filterDate_hist[2])
      )
  
        ref_line <- geom_vline(xintercept = input$hist_limit, linetype = "dashed", colour = "red", size = 0.25)
        
        hist <- ggplot(data = PM_group, aes_string(x = input$hist_var, fill = input$hist_fill))+
          geom_histogram(binwidth = input$hist_binsize)+
          facet_wrap(as.formula(input$hist_facet), scales = input$hist_scale)+
          labs(title = paste0("Count of ", input$hist_var, " by ", input$hist_fill, " grouped by ", input$hist_facet))+
          theme_few()+
          ref_line+
          scale_fill_brewer(palette = "BuPu")
        
    })
    
    output$scatter <- renderPlot({
      
      input$goScatter
      
      isolate(
      PM_group <- PM_AGG %>% 
        filter(LENGTH_Q %in% input$filterLength_scatter) %>%
        filter(EVENT_SHIFT_I %in% input$filterShift_scatter ) %>%
        filter(DG %in% input$filterDG_scatter ) %>%
        filter(Reefer %in% input$filterReefer_scatter ) %>%
        filter(Terminal %in% input$filterTerminal_scatter) %>%
        filter(MOVE_OP_C %in% input$filterMove_scatter ) %>%
        filter(hour %in% input$filterHour_scatter ) %>%
        filter(day %in% input$filterDay_scatter ) %>%
        filter(date >= input$filterDate_scatter[1] & date <= input$filterDate_scatter[2])
      )

      input$goScatter
      isolate(
        if(input$scatter_group == "single"){
          scatterplot <- isolate(ggscatterstats(data = PM_group, x = PM_WAIT_TIME_Q, y = PM_TRAVEL_TIME_Q, type = "nonparametric", marginal.type = input$scatter_type))
        } else {
          scatterplot <- isolate(grouped_ggscatterstats(data = PM_group, x = PM_WAIT_TIME_Q, y = PM_TRAVEL_TIME_Q, grouping.var = !!sym(input$scatter_var), type = "nonparametric", marginal.type = input$scatter_type))
        })
        
        return(scatterplot)
    })
    
    output$box <- renderPlot({
      
      input$goBox
      
      isolate(
      PM_group <- PM_AGG %>% 
        filter(LENGTH_Q %in% input$filterLength_box) %>%
        filter(EVENT_SHIFT_I %in% input$filterShift_box ) %>%
        filter(DG %in% input$filterDG_box ) %>%
        filter(Reefer %in% input$filterReefer_box ) %>%
        filter(Terminal %in% input$filterTerminal_box) %>%
        filter(MOVE_OP_C %in% input$filterMove_box ) %>%
        filter(hour %in% input$filterHour_box ) %>%
        filter(day %in% input$filterDay_box ) %>%
        filter(date >= input$filterDate_box[1] & date <= input$filterDate_box[2])
      )
      
      input$goBox
      isolate(
      if(input$box_group == "single"){
        box_plot <- isolate(ggbetweenstats(data = PM_group, x = !!sym(input$box_var), y = PM_WAIT_TIME_Q, plot.type = input$box_type, type = "np", pairwise.display = "s"))
      } else{
        box_plot <- isolate(grouped_ggbetweenstats(data = PM_group, x = !!sym(input$box_var), y = PM_WAIT_TIME_Q, grouping.var = !!sym(input$box_groupvar), type = "bf")
)
      }
      )
      return(box_plot)
      
    })
    


}



# Run the application 
shinyApp(ui = ui, server = server)
