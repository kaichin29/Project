# Install devtools from CRAN
#install.packages("devtools")

# Or the development version from GitHub:
# install.packages("devtools")
#devtools::install_github("r-lib/devtools")


#devtools::install_github("timelyportfolio/d3treeR")

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
             'dplyr','qicharts2','ggQC','qcc','rmarkdown','gridExtra','shinyBS','SixSigma', 'd3treeR')
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}


## loading the data
PM_AGG <- read_csv("data/PM_AGG2.csv")

## filtering the data

## filtering variables
max_date <- as.Date("2019-03-10")
min_date <- as.Date("2019-03-01")

date_range <- seq(min_date, max_date, "days")

options(spinner.type = 8)

## ZL data prep
ZL_Dataset <- read_csv("data/pm 201903_ZL.txt") 
ZL_DF <- subset(ZL_Dataset, select = -c(3,4,5,10,13,14,16,21,22,23,25,27,28,34,35,36,36,37)) %>%
  filter(EVENT_C %in% c('EQMT','EQOF')) %>%
  filter(LENGTH_Q %in% c(20,40,45))

ZL_DF$SHIFT_D = ymd(as.character(ZL_DF$SHIFT_D))
ZL_DF$EVENT_DT = ymd_hms(as.character(ZL_DF$EVENT_DT))

agg_base <-  ZL_DF %>%
  group_by(SHIFT_D,EVENT_SHIFT_I,Terminal,MOVE_OP_C,CNTR_TYPE_C,EQUIPMENT_TYPE_C,LENGTH_Q,CNTR_ST_C) %>%
  dplyr::summarise(N=n(),
                   Avg_WAIT_TIME = mean(PM_WAIT_TIME_Q),
                   Total_WAIT_TIME = sum(PM_WAIT_TIME_Q),
                   Avg_TRAVEL_TIME = mean(PM_TRAVEL_TIME_Q), 
                   Total_TRAVEL_TIME = sum(PM_TRAVEL_TIME_Q),
                   Avg_UNPRODUCTIVE_TIME = mean(PM_UNPRODUCTIVE_TIME_Q),
                   Total_UNPRODUCTIVE_TIME = sum(PM_UNPRODUCTIVE_TIME_Q),
                   Avg_NON_WORK_TIME = mean(PM_NON_WORK_TIME_Q),
                   Total_NON_WORK_TIME = sum(PM_NON_WORK_TIME_Q))%>%
    ungroup()

####

#Effect
effect<-"Less Productivity"

#Causes
causes.head<-c("Measurement", "Procedures/Plans", "Methods", "Environment", "Personnel", "Equipment")

#Individual Causes
causes<-vector(mode = "list", length = length(causes.head))
causes[1]<-list(c("Data error","KPI Definition" ))
causes[2]<-list(c("Roster/Schedule", "Logistic","Deployment","Compliance"))
causes[3]<-list(c("Sampling", "Analytical Procedure","Data Collection"))
causes[4]<-list(c("Traffic", "Weather","Seasonality"))
causes[5]<-list(c("Shifts","Training","Operators","Incentives"))
causes[6]<-list(c("Leakage", "Breakdown","Maintenance"))



#### sidebar ####
sidebar <- dashboardSidebar(width=275,
  sidebarMenu(
   # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
   # menuItem("Widgets", icon = icon("th"), tabName = "widgets",
   #          badgeLabel = "new", badgeColor = "green")
  
    
    
    radioGroupButtons(
      inputId = "toggleKPI",
      label = "KPI", 
      choices = c("Wait Time", "Travel Time"),
      status = "primary"
    ),
    
    dateRangeInput('dateRange',
                   label = 'Date range',
                   start = min_date , end = max_date
                   
    ),
    
    menuItem("Filters",tabName="Filters",icon = icon("filter"),
            
    
    prettyCheckboxGroup(inputId = "f_Terminal",
                        label =  "Terminal:",
                        choices = sort(unique(ZL_DF$Terminal)),
                        icon = icon("check-square-o"), 
                        status = "primary",
                        outline = TRUE,
                        inline = TRUE,
                        selected = sort(unique(ZL_DF$Terminal))),
    
    prettyCheckboxGroup(inputId = "f_MOVE_OP_C",
                        label =  "Move Ops:",
                        choices = sort(unique(ZL_DF$MOVE_OP_C)),
                        icon = icon("check-square-o"), 
                        status = "primary",
                        outline = TRUE,
                        inline = TRUE,
                        selected = sort(unique(ZL_DF$MOVE_OP_C))),
        
    prettyCheckboxGroup(inputId = "f_LENGTH_Q",
                        label =  "Length of Container:",
                        choices = sort(unique(ZL_DF$LENGTH_Q)),
                        icon = icon("check-square-o"), 
                        status = "primary",
                        outline = TRUE,
                        inline = TRUE,
                        selected = sort(unique(ZL_DF$LENGTH_Q))),
    
    prettyCheckboxGroup(inputId = "f_CNTR_TYPE_C",
                        label =  "Container Type:",
                        choices = sort(unique(ZL_DF$CNTR_TYPE_C)),
                        icon = icon("check-square-o"), 
                        status = "primary",
                        outline = TRUE,
                        inline = TRUE,
                        selected = sort(unique(ZL_DF$CNTR_TYPE_C))),
    
    prettyCheckboxGroup(inputId = "f_CNTR_ST_C",
                        label =  "Empty or Full Container:",
                        choices = sort(unique(ZL_DF$CNTR_ST_C)),
                        icon = icon("check-square-o"), 
                        status = "primary",
                        outline = TRUE,
                        inline = TRUE,
                        selected = sort(unique(ZL_DF$CNTR_ST_C))),
        
    prettyCheckboxGroup(inputId = "f_EVENT_SHIFT_I",
                        label =  "Day/Night Shift:",
                        choices = sort(unique(ZL_DF$EVENT_SHIFT_I)),
                        icon = icon("check-square-o"), 
                        status = "primary",
                        outline = TRUE,
                        inline = TRUE,
                        selected = sort(unique(ZL_DF$EVENT_SHIFT_I))),
    
    conditionalPanel(condition = "input.conditionedPlot  == 'Bar Chart' || input.conditionedPlot == 'Histogram' || input.conditionedPlot2 == 'Scatter Plot' || input.conditionedPlot2 == 'Box Plot'",
                     pickerInput(inputId = "f_hour",
                                 label = "Select Hour:",
                                 choices = sort(unique(PM_AGG$hour)),
                                 multiple = TRUE, 
                                 options = list(`actions-box` = TRUE),
                                 selected = sort(unique(PM_AGG$hour))
                     ),
                     
                     pickerInput(inputId = "f_day",
                                 label = "Select Day:",
                                 choices = sort(unique(PM_AGG$day)),
                                 multiple = TRUE, 
                                 options = list(`actions-box` = TRUE),
                                 selected = sort(unique(PM_AGG$day))
                     ))
                     
    ),
    
    actionButton("goButton", "Apply",width = 250,icon = icon("sync"))
    
    
  

  )
)
#### end of side bar####

body <- dashboardBody(
  
  
    navbarPage(
        theme = shinytheme("cerulean"), 
        "Understanding Prime Mover (PM) Waiting Time in Yard",

        tabPanel("Home",
                 #  sidebarPanel(),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Introduction",
                              h4("Introduction"),
                              p("Maritime trade has been the backbone of international trade as they account for approximately 92% of world trade. PSA Singapore handles about a fifth of the world’s transhipped containers as the world’s busiest transhipment port. For more than four decades, PSA continuously developed and upgraded its container handling infrastructure, pioneered new systems and processes, and streamlined operations to meet the rapid growth in its container terminal business as part of the strive for operational excellence."),
                              br(),
                              p("Upon a vessel’s arrival at a berth in the container terminal, containers are discharged from and loaded onto it. A typical discharging operation starts with a quay crane picking up a container from the vessel and placing it onto a PM, which will then transport to a storage yard. At the yard, a yard crane picks up the container from the PM and shifts it to a designated spot. Loading operations involve the transporting of containers in the opposite direction, from the yard to the vessel."),
                              br(),
                              p("Therefore, PM productivity is of key interest to PSA as it is the main driver for the time taken to load and unload vessels. PM Productivity is defined as the sum of the total number of containers handled divided by the total hours. The following are the key terms which PSA uses to define PM productivity."),
                              #p("Productivity = Total Containers Handled / Total Time", align = "center")
                              withMathJax("$$Productivity = Total Containers Handled / Total Time$$"),
                              withMathJax("$$Total Time = Sum (Est.Travel Time + Est.Wait Time + Unproductive Time + Non-Work Time)$$"),
                              br(),
                              p(strong("Total Time "),"is defined as the time difference between the two operation activities."),
                              p(strong("Estimated Travel Time ")," is the duration between two locations based on distance matrix with fixed speed limit/hr."),
                              p(strong("Unproductive Time ")," is the time logoff by the same driver. "),
                              p(strong("Non-Work Time ")," is the time taken for a change of driver, meal break, and PM breakdown (if any). "),
                              p(strong("Estimated Waiting time ="), withMathJax("Total Time – (Non-Work Time + Unproductive Time + Est. Travel Time)")) ,
                              br(),
                              br(),
                              plotOutput("fishbone"),
                              
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
                              p("Please refer to the user guide for more details on how to use the Shiny application", a("Link", href="https://github.com/kaichin29/Project/blob/master/Project/User%20Guide.pdf")), 
                              em("Please note that due to the size of the data set, some graphs will take time to load.", style = "font-si9pt"),
                              em("Click link for full user guide",  style = "font-si9pt")
                     )
                   )   
                 )
        ),
        
        tabPanel("Exploratory Data Analysis",
                 tabsetPanel(
                   tabPanel("Treemap",
                            fluidRow(
                              br(),
                              column(4,dropdownButton(size = 'sm',tags$h4("Set parameters"),
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
                                                      
                                                      circle = TRUE, status = "warning",
                                                      icon = icon("gear"), width = "100px",
                                                      tooltip = tooltipOptions(title = "Set parameters for treemap"))),
                              mainPanel(textOutput("Treemap"))
                            ),
                            withSpinner(d3tree2Output("treemap"))
                            ),
                   tabPanel("Bar Chart",
                            fluidRow(
                              br(),
                              column(4,dropdownButton(size = 'sm',tags$h4("Set parameters"),
                                                      selectInput(inputId = "bar_var", 
                                                                  label =  "Choose your primary variable:",
                                                                  choices = c("LENGTH of Container" = "LENGTH_Q",
                                                                              "Shift" = "EVENT_SHIFT_I",
                                                                              "Container Type" = "CNTR_TYPE_C", 
                                                                              "Empty / Full" = "CNTR_ST_C",
                                                                              "Terminal ID" = "Terminal",
                                                                              "Day" = "day",
                                                                              "Hour" = "hour"),
                                                                  selected = "LENGTH_Q"),
                                                      
                                                      selectInput(inputId = "bar_fill", 
                                                                  label =  "Choose your fill variable:",
                                                                  choices = c("LENGTH of Container" = "LENGTH_Q",
                                                                              "Shift" = "EVENT_SHIFT_I",
                                                                              "Container Type" = "CNTR_TYPE_C", 
                                                                              "Empty / Full" = "CNTR_ST_C",
                                                                              "Terminal ID" = "Terminal",
                                                                              "Day" = "day",
                                                                              "Hour" = "hour"),
                                                                  selected = "Terminal"),
                                                      
                                                      selectInput(inputId = "bar_facet", 
                                                                  label =  "Choose your grouping variable:",
                                                                  choices = c("LENGTH of Container" = "~LENGTH_Q",
                                                                              "Shift" = "~EVENT_SHIFT_I",
                                                                              "Container Type" = "~CNTR_TYPE_C", 
                                                                              "Empty / Full" = "~CNTR_ST_C",
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
                                                      
                                                      circle = TRUE, status = "warning",
                                                      icon = icon("gear"), width = "100px",
                                                      tooltip = tooltipOptions(title = "Set parameters for bar chart"))),
                              mainPanel(textOutput("Bar Chart"))
                            ),
                            withSpinner(plotlyOutput("bar")), 
                            
                   ),
                   tabPanel("Histogram",
                            fluidRow(
                              br(),
                              column(4,dropdownButton(size = 'sm',tags$h4("Set parameters"),
                                                      selectInput(inputId = "hist_var", 
                                                                  label =  "Choose your primary variable:",
                                                                  choices = c("Waiting Time" = "PM_WAIT_TIME_Q",
                                                                              "Travel Time" = "PM_TRAVEL_TIME_Q"),
                                                                  selected = "PM_WAIT_TIME_Q"),
                                                      
                                                      selectInput(inputId = "hist_fill", 
                                                                  label =  "Choose your fill variable:",
                                                                  choices = c("LENGTH of Container" = "LENGTH_Q",
                                                                              "Shift" = "EVENT_SHIFT_I",
                                                                              "Container Type" = "CNTR_TYPE_C", 
                                                                              "Empty / Full" = "CNTR_ST_C",
                                                                              "Terminal ID" = "Terminal",
                                                                              "Day" = "day",
                                                                              "Hour" = "hour"),
                                                                  selected = "Terminal"),
                                                      
                                                      selectInput(inputId = "hist_facet", 
                                                                  label =  "Choose your grouping variable:",
                                                                  choices = c("LENGTH of Container" = "~LENGTH_Q",
                                                                              "Shift" = "~EVENT_SHIFT_I",
                                                                              "Container Type" = "~CNTR_TYPE_C", 
                                                                              "Empty / Full" = "~CNTR_ST_C",
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
                                                      
                                                      circle = TRUE, status = "warning",
                                                      icon = icon("gear"), width = "100px",
                                                      tooltip = tooltipOptions(title = "Set parameters for histogram"))),
                              mainPanel(textOutput("Histogram"))
                            ),
                            withSpinner(plotlyOutput("hist"))
                            ), 
                   id = "conditionedPlot"
                 )),
        
        
        tabPanel("Confirmatory Data Analysis",
                 tabsetPanel(
                   tabPanel("Scatter Plot",
                            fluidRow(
                              br(),
                              column(4,dropdownButton(size = 'sm',tags$h4("Set parameters"),
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
                                                                                               "Container Type" = "~CNTR_TYPE_C", 
                                                                                               "Empty / Full" = "~CNTR_ST_C",
                                                                                               "Terminal ID" = "Terminal",
                                                                                               "Day" = "day",
                                                                                               "Hour" = "hour"),
                                                                                   selected = "Terminal"),
                                                      ),
                                                      
                                                      circle = TRUE, status = "warning",
                                                      icon = icon("gear"), width = "100px",
                                                      tooltip = tooltipOptions(title = "Set parameters for scatter plot"))),
                              mainPanel(textOutput("Scatter Plot"))
                            ),
                            withSpinner(plotOutput("scatter"))
                   ), tabPanel("Box-Violin Plot",
                               fluidRow(
                                 br(),
                                 column(4,dropdownButton(size = 'sm',tags$h4("Set parameters"),
                                                         radioButtons("box_group",
                                                                      "Do you wish to break down by variable?",
                                                                      choices = c("No" = "single",
                                                                                  "Yes" = "group"),
                                                                      selected = "single"),
                                                         
                                                         selectInput(inputId = "box_var",
                                                                     label = "Choose your variable", 
                                                                     choices = c("Length of Container" = "LENGTH_Q",
                                                                                 "Shift" = "EVENT_SHIFT_I",
                                                                                 "Container Type" = "~CNTR_TYPE_C", 
                                                                                 "Empty / Full" = "~CNTR_ST_C",
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
                                                                                                  "Container Type" = "~CNTR_TYPE_C", 
                                                                                                  "Empty / Full" = "~CNTR_ST_C",
                                                                                                  "Terminal ID" = "Terminal",
                                                                                                  "Day" = "day",
                                                                                                  "Hour" = "hour"),
                                                                                      selected = "Terminal"),
                                                         ),
                                                         
                                                         circle = TRUE, status = "warning",
                                                         icon = icon("gear"), width = "100px",
                                                         tooltip = tooltipOptions(title = "Set parameters for box-violin plot"))),
                                 mainPanel(textOutput("Box-Violin Plot"))
                               ),
                               withSpinner(plotOutput("box"))
                   ), 
                   id = "conditionedPlot2")
                   ),

        tabPanel("Pareto Analysis",
                 tabsetPanel(
                   
                   tabPanel("Overview",
                            
                            
                            box(plotOutput("pareto_1"),
                                prettyRadioButtons(
                                  inputId = "pareto_radio1",
                                  label = "Measure",
                                  choices = c("Total", "Average")
                                )
                            )
                   ),
                   
                   
                   
                   tabPanel("Breakdown",
                            fluidRow(
                              br(),
                              column(1,dropdownButton(size = 'xs',tags$h3("Set Threshold Duration"),
                                                      sliderInput(inputId = 'Duration_N',
                                                                  label = 'Duration',
                                                                  value = 30,
                                                                  min = 0,
                                                                  max = 60),
                                                      circle = TRUE, status = "warning",
                                                      icon = icon("gear"), width = "100px",
                                                      tooltip = tooltipOptions(title = "Set Threshold Duration"))
                                     
                              ),
                              mainPanel(textOutput("paretotitle"))
                              
                            ),
                            fluidRow(   
                              
                              box(withSpinner(plotlyOutput("pareto_2")),title = "Terminal", solidHeader = TRUE,collapsible = TRUE),
                              box(withSpinner(plotlyOutput("pareto_3")),title = "Operation Type", solidHeader = TRUE,collapsible = TRUE),
                              box(withSpinner(plotlyOutput("pareto_4")),title = "Container Length", solidHeader = TRUE,collapsible = TRUE),
                              box(withSpinner(plotlyOutput("pareto_5")),title = "Container Type", solidHeader = TRUE,collapsible = TRUE),
                              box(withSpinner(plotlyOutput("pareto_6")),title = "Equipment Type", solidHeader = TRUE,collapsible = TRUE)
                            )
                   )
                 )
        ),
        
        
        tabPanel("Control Chart Analysis",
                 tabsetPanel(
                   tabPanel("Mean(xbar) & Standard deviation(S)",
                            withSpinner(plotlyOutput("qic_Xbar")),
                            br(),
                            withSpinner(plotlyOutput("qic_S"))  
                            
                            
                   ),
                   tabPanel("Individual(I) & MovingRange(MR)",
                            fluidRow(
                              br(),
                              column(1,dropdownButton(size = 'xs',tags$h3("Set last number of records"),
                                           sliderInput(inputId = 'Last_N',
                                                       label = 'Last_N',
                                                       value = 100,
                                                       min = 10,
                                                       max = 1000),
                                           circle = TRUE, status = "warning",
                                           icon = icon("gear"), width = "100px",
                                           tooltip = tooltipOptions(title = "Set last number of records"))),
                            mainPanel(textOutput("I_MRtitle"))
                              )
                            ,
                            withSpinner(plotlyOutput("qic_i")),
                            br(),
                            withSpinner(plotlyOutput("qic_mr"))
                            ),
                   tabPanel("Count(C), Rate(U) & Proportion(P)",
                            fluidRow(
                              br(),
                              column(1,dropdownButton(size = 'xs',tags$h3("Set Threshold Duration"),
                                                      sliderInput(inputId = 'Duration_N1',
                                                                  label = 'Duration',
                                                                  value = 30,
                                                                  min = 0,
                                                                  max = 60),
                                                      circle = TRUE, status = "warning",
                                                      icon = icon("gear"), width = "100px",
                                                      tooltip = tooltipOptions(title = "Set Threshold Duration"))
                                     
                              ),
                            mainPanel(textOutput("C_U_Ptitle"))
                              
                            ),  
                           withSpinner(plotlyOutput("qic_c")),
                           br(),
                            withSpinner(plotlyOutput("qic_p")),
                           br(),
                            withSpinner(plotlyOutput("qic_u"))
                            
                            ),
                   tabPanel("Facets",
                            
                            dropdownButton(size = 'xs',tags$h3("Facets Setting"),
                                           selectInput(inputId="select_F1", label = h6("Facet1"), 
                                                       choices = list("~Terminal" , "~MOVE_OP_C" ,"~EVENT_SHIFT_I","~CNTR_TYPE_C","~EQUIPMENT_TYPE_C","~LENGTH_Q","~CNTR_ST_C"), 
                                                       selected = "~Terminal"),
                                           selectInput(inputId="select_chart", label = h6("Control chart"), 
                                                       choices = list("xbar" , "s" ), 
                                                       selected = "xbar")#,
                                          # actionButton("goButton", "Apply",icon = icon("sync"))
                                           
                                           
                                           ,
                                           circle = TRUE, status = "warning",
                                           icon = icon("gear"), width = "200px",
                                           tooltip = tooltipOptions(title = "Facets Setting")),
                            
                            withSpinner(plotlyOutput("qic_f"))
                           # withSpinner(plotOutput("qic_f"))
                            
                            
                            )
                 )
                 )
        
        
        

        
        

                 
                 
                 )
        
        
    )
#)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "MyDashboard"),
  sidebar,
  body
)


##  server

server <- function(input, output, session) {
  
  
 
  #toggle KPI
  KPI <- reactiveVal('ZL_DF$PM_WAIT_TIME_Q')

# link both duration inputs
  observeEvent(input$Duration_N, {
    updateNumericInput(session, "Duration_N1", value=input$Duration_N)
    
  })

  observeEvent(input$Duration_N1, {
    updateNumericInput(session, "Duration_N", value=input$Duration_N1)
    
  })

  #toggle between wait and travel    
  observeEvent(input$toggleKPI, {
  
   if (input$toggleKPI == 'Wait Time'){
     KPI(ZL_DF$PM_WAIT_TIME_Q)

   }else{
     KPI(ZL_DF$PM_TRAVEL_TIME_Q)

   }
    
  })
  # 
  # f1 <- reactiveVal('Terminal')
  # f2 <- reactiveVal('EVENT_SHIFT_I')
  # 
  # observeEvent(input$select_F1,{
  #   f1(input$select_F1)
  # })
  # observeEvent(input$select_F2,{
  #   f2(input$select_F2)
  # })
  # 
  #refresh filter

  ### treemap
  
  output$treemap <- renderD3tree2({
    input$goButton
    
    isolate(
      PM_tree <- PM_AGG %>%
        filter(LENGTH_Q %in% input$f_LENGTH_Q) %>%
        filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
        filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
        filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
        filter(Terminal %in% input$f_Terminal) %>%
        filter(MOVE_OP_C %in% input$f_MOVE_OP_C ) %>%
        filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    )
    PM_tree$count <- 1
    
           isolate(
             tree <- treemap(PM_tree, index = c(input$tree_primary, input$tree_secondary), vSize = "count", vColor = sum(input$vsize), palette = "Blues", title = "Treemap for PM Waiting time by CNTR Specifications and Shift", fontsize.labels = c(15,10))
           )
           return(d3tree2(tree, rootname = "Container Types"))
             
         })
  output$bar <- renderPlotly({  
    
    input$goButton
    
  
    isolate(
      PM_group <- PM_AGG %>%
        filter(LENGTH_Q %in% input$f_LENGTH_Q) %>%
        filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
        filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
        filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
        filter(Terminal %in% input$f_Terminal) %>%
        filter(MOVE_OP_C %in% input$f_MOVE_OP_C ) %>%
        filter(hour %in% input$f_hour ) %>%
        filter(day %in% input$f_day ) %>%
        filter(date >= input$dateRange[1] & date <= input$dateRange[2])
      )
    PM_group$LENGTH_Q <- as.factor(PM_group$LENGTH_Q)


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
    
    input$goButton
    
    isolate(
      PM_group <- PM_AGG %>%
        filter(LENGTH_Q %in% input$f_LENGTH_Q) %>%
        filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
        filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
        filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
        filter(Terminal %in% input$f_Terminal) %>%
        filter(MOVE_OP_C %in% input$f_MOVE_OP_C ) %>%
        filter(hour %in% input$f_hour ) %>%
        filter(day %in% input$f_day ) %>%
        filter(date >= input$dateRange[1] & date <= input$dateRange[2])
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
    
    input$goButton
    
    isolate(
      PM_group <- PM_AGG %>%
        filter(LENGTH_Q %in% input$f_LENGTH_Q) %>%
        filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
        filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
        filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
        filter(Terminal %in% input$f_Terminal) %>%
        filter(MOVE_OP_C %in% input$f_MOVE_OP_C ) %>%
        filter(hour %in% input$f_hour ) %>%
        filter(day %in% input$f_day ) %>%
        filter(date >= input$dateRange[1] & date <= input$dateRange[2])
      )
    
    input$goButton
    isolate(
      if(input$scatter_group == "single"){
        scatterplot <- isolate(ggscatterstats(data = PM_group, x = PM_WAIT_TIME_Q, y = PM_TRAVEL_TIME_Q, type = "nonparametric", marginal.type = input$scatter_type))
      } else {
        scatterplot <- isolate(grouped_ggscatterstats(data = PM_group, x = PM_WAIT_TIME_Q, y = PM_TRAVEL_TIME_Q, grouping.var = !!sym(input$scatter_var), type = "nonparametric", marginal.type = input$scatter_type))
      })
    
    return(scatterplot)
  })
  
  
  output$box <- renderPlot({
    
    input$goButton
    
    isolate(
      PM_group <- PM_AGG %>%
        filter(LENGTH_Q %in% input$f_LENGTH_Q) %>%
        filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
        filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
        filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
        filter(Terminal %in% input$f_Terminal) %>%
        filter(MOVE_OP_C %in% input$f_MOVE_OP_C ) %>%
        filter(hour %in% input$f_hour ) %>%
        filter(day %in% input$f_day ) %>%
        filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    )
    
    input$goButton
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
  
  
  makeReactiveBinding("ZL_DF_longwait") 
  refreshDataPareto <- reactive ({
    input$goButton 
    isolate(
    ZL_DF_longwait  <- subset(ZL_DF,KPI() >= input$Duration_N)%>%
      filter(Terminal %in% input$f_Terminal) %>%
      filter(MOVE_OP_C %in% input$f_MOVE_OP_C) %>%
      filter(LENGTH_Q %in% input$f_LENGTH_Q ) %>%
      filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%

      filter(SHIFT_D >= input$dateRange[1] & SHIFT_D <= input$dateRange[2]) 
    )
  })
  
  makeReactiveBinding("ZL_DF_qic") 
  refreshData_QIC <- reactive ({
    input$goButton 
    isolate(
    ZL_DF_qic <- ZL_DF %>%
      filter(Terminal %in% input$f_Terminal) %>%
      filter(MOVE_OP_C %in% input$f_MOVE_OP_C) %>%
      filter(LENGTH_Q %in% input$f_LENGTH_Q ) %>%
      filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
      filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
      filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
      filter(SHIFT_D >= input$dateRange[1] & SHIFT_D <= input$dateRange[2]) 
    )
  
  })

  makeReactiveBinding("ZL_DF_longwait1") 
  makeReactiveBinding("agg_Count")
  makeReactiveBinding("agg_merge")
  
  refreshData_CUP <- reactive ({
    input$goButton 
    isolate({
    ZL_DF_longwait1  <- subset(ZL_DF,KPI() >= input$Duration_N)%>%
      filter(Terminal %in% input$f_Terminal) %>%
      filter(MOVE_OP_C %in% input$f_MOVE_OP_C) %>%
      filter(LENGTH_Q %in% input$f_LENGTH_Q ) %>%
      filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
      filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
      filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
      filter(SHIFT_D >= input$dateRange[1] & SHIFT_D <= input$dateRange[2]) 
    
    
  
    
    agg_Count <-  ZL_DF_longwait1 %>%
      group_by(SHIFT_D) %>%
      dplyr::summarise(longwait_No =n()
      )%>%
      ungroup()
    
    
   
    agg_total <-  agg_base %>%
      filter(Terminal %in% input$f_Terminal) %>%
      filter(MOVE_OP_C %in% input$f_MOVE_OP_C) %>%
      filter(LENGTH_Q %in% input$f_LENGTH_Q ) %>%
      filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
      filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
      filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
      filter(SHIFT_D >= input$dateRange[1] & SHIFT_D <= input$dateRange[2])%>%
      group_by(SHIFT_D) %>%
      dplyr::summarise(Total_No =sum(N)
      )%>%
      ungroup()
    
   
    agg_merge <- merge( agg_Count, agg_total, by='SHIFT_D')
    
    })
    
  })
 

    

  #filter pareto overview
    output$pareto_1 <- renderPlot({ 
      agg1 <- agg_base %>%
        filter(Terminal %in% input$f_Terminal) %>%
        filter(MOVE_OP_C %in% input$f_MOVE_OP_C) %>%
        filter(LENGTH_Q %in% input$f_LENGTH_Q ) %>%
        filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
        filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
        filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
        filter(SHIFT_D >= input$dateRange[1] & SHIFT_D <= input$dateRange[2])%>%
        
        dplyr::summarise(Counts = sum(N),
                         Total_WAIT_TIME = sum(Total_WAIT_TIME),
                         Total_TRAVEL_TIME = sum(Total_TRAVEL_TIME),
                         Total_UNPRODUCTIVE_TIME = sum(Total_UNPRODUCTIVE_TIME),
                         Total_NON_WORK_TIME = sum(Total_NON_WORK_TIME),
                         Avg_WAIT_TIME = sum(Total_WAIT_TIME)/sum(N),
                         Avg_TRAVEL_TIME = sum(Total_TRAVEL_TIME)/sum(N),
                         Avg_UNPRODUCTIVE_TIME = sum(Total_UNPRODUCTIVE_TIME)/sum(N),
                         Avg_NON_WORK_TIME = sum(Total_NON_WORK_TIME)/sum(N),
                         Total_Duration = sum(Total_TRAVEL_TIME+Total_WAIT_TIME+Total_UNPRODUCTIVE_TIME+Total_NON_WORK_TIME)
        ) 
     

      if (input$pareto_radio1 == 'Total') {
   
      
      Data4Pareto <- data.frame(Time_Indicator = c("Travel Time","Wait Time", "Unproductive Time","Non-Work Time"),
                                Time = c(agg1$Total_TRAVEL_TIME,
                                         agg1$Total_WAIT_TIME,
                                         agg1$Total_UNPRODUCTIVE_TIME,
                                         agg1$Total_NON_WORK_TIME),
                                stringsAsFactors = FALSE)
      #convert minutes to man-days 
      Data4Pareto$Mandays = time_length(dminutes(Data4Pareto$Time), unit = "day")
      Data4Pareto <- Data4Pareto[order(Data4Pareto$Mandays,decreasing=TRUE),]
      Data4Pareto$Time_Indicator <- factor(Data4Pareto$Time_Indicator, levels=Data4Pareto$Time_Indicator)
      ggplot(Data4Pareto, aes(x=Time_Indicator, y=Mandays)) +
        stat_pareto(point.size = 3,line.color = "black" ) +
        theme_minimal() +
        labs(title = "Total PM Operation Duration - Pareto Chart", y = "Mandays" , x = "Time spent category"  ) 
      
      }
      else 
        {
      AvgData4Pareto <- data.frame(Time_Indicator = c("Travel Time","Wait Time", "Unproductive Time","Non-Work Time"),
                                   Time = c(agg1$Avg_TRAVEL_TIME,
                                            agg1$Avg_WAIT_TIME,
                                            agg1$Avg_UNPRODUCTIVE_TIME,
                                            agg1$Avg_NON_WORK_TIME))
      
      AvgData4Pareto <- AvgData4Pareto[order(AvgData4Pareto$Time,decreasing=TRUE), ]
      AvgData4Pareto$Time_Indicator <- factor(AvgData4Pareto$Time_Indicator,levels=AvgData4Pareto$Time_Indicator)
      
      
     ggplot(AvgData4Pareto, aes(x=Time_Indicator , y=Time)) +
        ggQC::stat_pareto(point.size = 3,
                          line.color = "black") +
        theme_minimal() + 
        labs(title = "Average PM Operation Duration - Pareto Chart", y = "Time (mins)" , x = "Time spent category"  ) 
      
      }
      
    })
    
    output$pareto_2 <- renderPlotly({
      
    ZL_DF_longwait  <- refreshDataPareto()
    ggplotly(paretochart(ZL_DF_longwait$Terminal,title = FALSE))

    })
    output$pareto_3 <- renderPlotly({
      ZL_DF_longwait  <- refreshDataPareto()
      ggplotly(paretochart(ZL_DF_longwait$MOVE_OP_C,title = FALSE))
    })
    output$pareto_4 <- renderPlotly({
      ZL_DF_longwait  <- refreshDataPareto()
      ggplotly(paretochart(ZL_DF_longwait$LENGTH_Q,title = FALSE))
    })
    output$pareto_5 <- renderPlotly({
      ZL_DF_longwait  <- refreshDataPareto()
      ggplotly(paretochart(ZL_DF_longwait$CNTR_TYPE_C,title = FALSE))
    })
    output$pareto_6 <- renderPlotly({
      ZL_DF_longwait  <- refreshDataPareto()
      ggplotly(paretochart(ZL_DF_longwait$EQUIPMENT_TYPE_C,title = FALSE))
    })
    
    #reactive title 
    output$paretotitle <- renderText({
      input$goButton 
      isolate(
     paste("Total PM",input$toggleKPI ,">= ",input$Duration_N, "mins", "- Pareto Chart")
)
    })
    
    output$I_MRtitle <- renderText({
      
      paste("Latest",input$Last_N, "Events")
      
    })
    
    output$C_U_Ptitle <- renderText({
      input$goButton 
      isolate(
      paste("Occurrences of PM",input$toggleKPI ,">= ",input$Duration_N, "mins")
      )
    })
    
##################control chart
    
    output$qic_Xbar <- renderPlotly({
      input$goButton 
      isolate({
      
      ZL_DF_qic  <- refreshData_QIC()
      
    
      
      if (input$toggleKPI == 'Wait Time'){
        
        p1 <- qic(PM_WAIT_TIME_Q,x = SHIFT_D ,data = ZL_DF_qic,chart = 'xbar') 
      }else{
      
        p1 <- qic(PM_TRAVEL_TIME_Q,x = SHIFT_D ,data = ZL_DF_qic,chart = 'xbar') 
        }
      
     # storing PIcharts into data.frame
      df1 <- p1$data
      
      ggp1 <- ggplot(df1, aes(x = ymd(x) , y = y , group = 1, text = paste("Date:", x ,"\n","Avg PM" , input$toggleKPI,":", round(y, 2)))) +
        theme_minimal() + 
        geom_line(color = "steelblue", size = 0.5) +
        geom_point(color = "steelblue", size = 1 ) +
        geom_point(data = subset(df1, y >= ucl), color = "red", size = 1) +
        geom_point(data = subset(df1, y <= lcl), color = "red", size = 1) +
        geom_hline(aes(yintercept = cl),linetype= "dashed") +
        geom_hline(aes(yintercept = aUCL),colour ="red",size = 0.5,linetype= "dashed",summary(p1)) +
        geom_hline(aes(yintercept = aLCL),colour ="red",size = 0.5,linetype= "dashed",summary(p1)) +
       # scale_y_continuous(breaks = round(seq(min(df1$y), max(df1$y), by = 0.1),1))+
        #rename tooltip attribute text. 
        labs(title = paste(input$toggleKPI,"Xbar Chart") ,
             y = paste("Avg ",input$toggleKPI,"(Mins)"), x = "Date")
      
      ggplotly(ggp1, tooltip=c("text")) %>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(df1$lcl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("LCL=",round(mean(df1$lcl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(df1$ucl*1.01),
        xref = "x",
        yref = "y",
        text = paste("UCL=",round(mean(df1$ucl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(df1$cl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("CL=",round(mean(df1$cl), 2)),
        showarrow = F,
        font = list(color = 'black',size = 10),
        opacity = 0.5
        
      )
      
      })
      
      
    })
    
    output$qic_S <- renderPlotly({

      input$goButton 
      isolate({          
      ZL_DF_qic  <- refreshData_QIC()
      
      
      if (input$toggleKPI == 'Wait Time'){
        p2 <- qic(PM_WAIT_TIME_Q,x = SHIFT_D ,data = ZL_DF_qic,chart = 's') 
      }else{
        p2 <- qic(PM_TRAVEL_TIME_Q,x = SHIFT_D ,data = ZL_DF_qic,chart = 's') 
      }
          # storing PIcharts into data.frame
      df2 <- p2$data
      
      ggp2 <- ggplot(df2, aes(x = ymd(x) , y = y , group = 1,text = paste("Date:", x ,"\n","Avg PM" , input$toggleKPI,":", round(y, 2)))) +
        theme_minimal() + 
        geom_line(color = "steelblue", size = 0.5) +
        geom_point(color = "steelblue", size = 1 ) +
        geom_point(data = subset(df2, y >= ucl), color = "red", size = 1) +
        geom_point(data = subset(df2, y <= lcl), color = "red", size = 1) +
        geom_hline(aes(yintercept = cl),linetype= "dashed") +
        geom_hline(aes(yintercept = aUCL),colour ="red",size = 0.5,linetype= "dashed",summary(p2)) +
        geom_hline(aes(yintercept = aLCL),colour ="red",size = 0.5,linetype= "dashed",summary(p2)) +
     #   scale_y_continuous(breaks = round(seq(min(df2$y), max(df2$y), by = 0.1),1))+
        #rename tooltip attribute text. 
        labs(title = paste(input$toggleKPI,"S-Chart") ,
             y = paste("Avg ",input$toggleKPI,"(Mins)"), x = "Date")

      ggplotly(ggp2, tooltip=c("text")) %>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(df2$lcl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("LCL=",round(mean(df2$lcl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(df2$ucl*1.01),
        xref = "x",
        yref = "y",
        text = paste("UCL=",round(mean(df2$ucl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(df2$cl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("CL=",round(mean(df2$cl), 2)),
        showarrow = F,
        font = list(color = 'black',size = 10),
        opacity = 0.5
        
      )
      
    }) 
      
    })

    
    output$qic_i <- renderPlotly({
      
      
    ZL_DF_tail <- tail(refreshData_QIC(), input$Last_N)
      # 
    if (input$toggleKPI == 'Wait Time'){
      P_I <- qic(PM_WAIT_TIME_Q, data = ZL_DF_tail,chart = 'i', )
    }else{
      P_I <- qic(PM_TRAVEL_TIME_Q, data = ZL_DF_tail,chart = 'i', )
    }
    DF_I = P_I$data
     
      ggp_I <- ggplot(DF_I, aes(x = x , y = y, text1 = y , text2 = x)  ) +
        theme_minimal() + 
        geom_line(color = "steelblue", size = 0.5) +
        geom_point(color = "steelblue", size = 1 ) +
        geom_point(data = subset(DF_I, y >= ucl), color = "red", size = 1) +
        geom_point(data = subset(DF_I, y <= lcl), color = "red", size = 1) +
        geom_hline(aes(yintercept = cl),linetype= "dashed") +
        geom_hline(aes(yintercept = aUCL),colour ="red",size = 0.5,linetype= "dashed",summary(P_I)) +
        geom_hline(aes(yintercept = aLCL),colour ="red",size = 0.5,linetype= "dashed",summary(P_I)) +
        #scale_y_continuous(breaks = round(seq(min(DF_I$y), max(DF_I$y), by = 10),1))+
        #rename tooltip attribute text. 
        labs(title = paste(input$toggleKPI,"Individual I-Chart") ,
             y = paste(input$toggleKPI,"(Mins)"), x = "Operations no.")
  
      
        ggplotly(ggp_I, tooltip=c("text2","text1"))  %>% add_annotations(
        x=input$Last_N,
        y=mean(DF_I$lcl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("LCL=",round(mean(DF_I$lcl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5

      )%>% add_annotations(
        x=input$Last_N,
        y=mean(DF_I$ucl*1.01),
        xref = "x",
        yref = "y",
        text = paste("UCL=",round(mean(DF_I$ucl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5

      )%>% add_annotations(
        x=input$Last_N,
        y=mean(DF_I$cl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("CL=",round(mean(DF_I$cl), 2)),
        showarrow = F,
        font = list(color = 'black',size = 10),
        opacity = 0.5

      )
      
      
    })
    
    output$qic_mr <- renderPlotly({
      
     
      ZL_DF_tail <- tail(refreshData_QIC(), input$Last_N)
      
      if (input$toggleKPI == 'Wait Time'){
        P_MR <- qic(PM_WAIT_TIME_Q, data = ZL_DF_tail,chart = 'mr', )
      }else{
        P_MR <- qic(PM_TRAVEL_TIME_Q, data = ZL_DF_tail,chart = 'mr', )
      }
   
      DF_MR = P_MR$data 
      
      ggp_MR <- ggplot(DF_MR, aes(x = x , y = y, text1 = y , text2 = x)  ) +
        theme_minimal() + 
        geom_line(color = "steelblue", size = 0.5) +
        geom_point(color = "steelblue", size = 1 ) +
        geom_point(data = subset(DF_MR, y >= ucl), color = "red", size = 1) +
        geom_point(data = subset(DF_MR, y <= lcl), color = "red", size = 1) +
        geom_hline(aes(yintercept = cl),linetype= "dashed") +
        geom_hline(aes(yintercept = aUCL),colour ="red",size = 0.5,linetype= "dashed",summary(P_MR))+
        # scale_y_continuous(breaks = round(seq(min(DF_MR$y), max(DF_MR$y), by = 10),1))+
        #rename tooltip attribute text. 
        labs(title = paste(input$toggleKPI,"- Moving Range MR-Chart") ,
             y = paste(input$toggleKPI,"(Mins)"), x = "Operations no.")+
        #if (DF_MR$lcl >=0) {
        geom_hline(aes(yintercept = aLCL),colour ="red",size = 0.5,linetype="dashed",summary(P_MR)) 
      # }
      
      
     ggplotly(ggp_MR, tooltip=c("text2","text1"))  %>% add_annotations(
        x=input$Last_N,
        y=mean(DF_MR$lcl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("LCL=",round(mean(DF_MR$lcl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=input$Last_N,
        y=mean(DF_MR$ucl*1.01),
        xref = "x",
        yref = "y",
        text = paste("UCL=",round(mean(DF_MR$ucl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=input$Last_N,
        y=mean(DF_MR$cl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("CL=",round(mean(DF_MR$cl), 2)),
        showarrow = F,
        font = list(color = 'black',size = 10),
        opacity = 0.5
        
      )
      
      
    })
    
    output$qic_c <- renderPlotly({
      

      agg_merge  <- refreshData_CUP()

      P_C <- qic(longwait_No,x=SHIFT_D, data = agg_merge, chart = 'c')
    #  P_P <- qic(longwait_No,n=Total_No,x=SHIFT_D, data = agg_merge, chart = 'p' )
    #  P_U <- qic(longwait_No,n=Total_No,x=SHIFT_D, data = agg_merge, chart = 'u' )
      
      DF_C = P_C$data
      
      ggp_C <- ggplot(DF_C, aes(x = ymd(x) , y = y, text1 = y , text2 = x)  ) +
        theme_minimal() + 
        geom_line(color = "steelblue", size = 0.5) +
        geom_point(color = "steelblue", size = 1 ) +
        geom_point(data = subset(DF_C, y >= ucl), color = "red", size = 1) +
        geom_point(data = subset(DF_C, y <= lcl), color = "red", size = 1) +
        geom_hline(aes(yintercept = cl),linetype= "dashed") +
        geom_hline(aes(yintercept = aUCL),colour ="red",size = 0.5,linetype= "dashed",summary(P_C)) +
        geom_hline(aes(yintercept = aLCL),colour ="red",size = 0.5,linetype= "dashed",summary(P_C)) +
        #scale_y_continuous(breaks = round(seq(min(DF_C$y), max(DF_C$y), by = 10),1))+
        #rename tooltip attribute text. 
        labs(title = "Count C-Chart" ,
             y = "Count", x = "Date")

   
     ggplotly(ggp_C, tooltip=c("text2","text1"))  %>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(DF_C$lcl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("LCL=",round(mean(DF_C$lcl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(DF_C$ucl*1.01),
        xref = "x",
        yref = "y",
        text = paste("UCL=",round(mean(DF_C$ucl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(DF_C$cl)*1.01,
        xref = "x",
        yref = "y",
        text = paste("CL=",round(mean(DF_C$cl), 2)),
        showarrow = F,
        font = list(color = 'black',size = 10),
        opacity = 0.5
        
      )
      
      
      
    })
    
    output$qic_u <- renderPlotly({
      
      
      agg_merge  <- refreshData_CUP()
      
     # P_C <- qic(longwait_No,x=SHIFT_D, data = agg_merge, chart = 'c')
      #  P_P <- qic(longwait_No,n=Total_No,x=SHIFT_D, data = agg_merge, chart = 'p' )
        P_U <- qic(longwait_No,n=Total_No,x=SHIFT_D, data = agg_merge, chart = 'u',multiply = 1000 )
      
      DF_U = P_U$data
      
      ggp_U <- ggplot(DF_U, aes(x = ymd(x) , y = y, text1 = y , text2 = x)  ) +
        theme_minimal() + 
        geom_line(color = "steelblue", size = 0.5) +
        geom_point(color = "steelblue", size = 1 ) +
        geom_point(data = subset(DF_U, y >= ucl), color = "red", size = 1) +
        geom_point(data = subset(DF_U, y <= lcl), color = "red", size = 1) +
        geom_hline(aes(yintercept = cl),linetype= "dashed") +
        geom_hline(aes(yintercept = aUCL),colour ="red",size = 0.5,linetype= "dashed",summary(P_U)) +
        #scale_y_continuous(breaks = round(seq(min(DF_U$y), max(DF_U$y), by = 10),1))+
        #rename tooltip attribute text. 
        labs(title = " Rate U-Chart" ,
             y = "Count Per 1000 Moves", x = "Date")+
        #if (DF_I$lcl >0) {
        geom_hline(aes(yintercept = aLCL),colour ="red",size = 0.5,linetype= "dashed",summary(P_U)) 
      # }
      
      
     ggplotly(ggp_U, tooltip=c("text2","text1")) %>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(DF_U$lcl)+0.2,
        xref = "x",
        yref = "y",
        text = paste("LCL=",round(mean(DF_U$lcl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(DF_U$ucl+0.2),
        xref = "x",
        yref = "y",
        text = paste("UCL=",round(mean(DF_U$ucl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(DF_U$cl)+0.2,
        xref = "x",
        yref = "y",
        text = paste("CL=",round(mean(DF_U$cl), 2)),
        showarrow = F,
        font = list(color = 'black',size = 10),
        opacity = 0.5
        
      )
      
    
    })
    
    
    
    output$qic_p <- renderPlotly({
      
      
      agg_merge  <- refreshData_CUP()
      
      # P_C <- qic(longwait_No,x=SHIFT_D, data = agg_merge, chart = 'c')
      P_P <- qic(longwait_No,n=Total_No,x=SHIFT_D, data = agg_merge, chart = 'p' )
      #  P_U <- qic(longwait_No,n=Total_No,x=SHIFT_D, data = agg_merge, chart = 'u' )
      
      DF_P = P_P$data
      
      ggp_P <- ggplot(DF_P, aes(x = ymd(x) , y = y, text1 = y , text2 = x)  ) +
        theme_minimal() + 
        geom_line(color = "steelblue", size = 0.5) +
        geom_point(color = "steelblue", size = 1 ) +
        geom_point(data = subset(DF_P, y >= ucl), color = "red", size = 1) +
        geom_point(data = subset(DF_P, y <= lcl), color = "red", size = 1) +
        geom_hline(aes(yintercept = cl),linetype= "dashed") +
        geom_hline(aes(yintercept = aUCL),colour ="red",size = 0.5,linetype= "dashed",summary(P_P)) +
        scale_y_continuous(labels = scales::percent)+

        #rename tooltip attribute text. 
        labs(title = "Proportion P-Chart" ,
             y = "Percent", x = "Date")+

        geom_hline(aes(yintercept = aLCL),colour ="red",size = 0.5,linetype= "dashed",summary(P_P)) 

      
      
      ggplotly(ggp_P, tooltip=c("text2","text1"))  %>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(DF_P$lcl+0.0002),
        xref = "x",
        yref = "y",
        text = paste("LCL=",round(mean(DF_P$lcl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(DF_P$ucl+0.0002),
        xref = "x",
        yref = "y",
        text = paste("UCL=",round(mean(DF_P$ucl), 2)),
        showarrow = F,
        font = list(color = 'red',size = 10),opacity = 0.5
        
      )%>% add_annotations(
        x=as.numeric(input$dateRange[2]),
        y=mean(DF_P$cl)+0.0002,
        xref = "x",
        yref = "y",
        text = paste("CL=",round(mean(DF_P$cl), 2)),
        showarrow = F,
        font = list(color = 'black',size = 10),
        opacity = 0.5
        
      )
      
    })
   
    
    output$qic_f <- renderPlotly({
      input$goButton 
      isolate({
        
        ZL_DF_qic  <- refreshData_QIC()



if (input$toggleKPI == 'Wait Time'){
  
  p3 <-qic(PM_WAIT_TIME_Q,x = SHIFT_D ,data = ZL_DF_qic,chart = input$select_chart ,facets   =  as.formula(input$select_F1) )
}else{
  p3 <-qic(PM_TRAVEL_TIME_Q,x = SHIFT_D ,data = ZL_DF_qic,chart = input$select_chart ,facets   =  as.formula(input$select_F1) )

}
   
    #  P_I <- qic(PM_WAIT_TIME_Q, data = ZL_DF_tail, chart = 'i',  ylab = 'PM_WAIT_TIME', xlab = 'Operations no.' )
    #  P_MR <- qic(PM_WAIT_TIME_Q, data = ZL_DF_tail, chart = 'mr',  ylab = 'PM_WAIT_TIME', xlab = 'Operations no.' )
     
      df3 <- p3$data
     # print(df3)
      ggp3 <- ggplot(df3, aes(x = x , y = y , group = 1, text = paste("x:", x ,"\n",input$toggleKPI,":", round(y, 2)))) +
        theme_minimal() + 
        geom_line(color = "steelblue", size = 0.3) +
        geom_point(color = "steelblue", size = 1 ) +
        geom_point(data = subset(df3, y >= ucl), color = "red", size = 1) +
        geom_point(data = subset(df3, y <= lcl), color = "red", size = 1) +
        geom_hline(aes(yintercept = CL),linetype= "dashed",summary(p3)) +
        geom_hline(aes(yintercept = aUCL),colour ="red",size = 0.15,linetype= "dashed",summary(p3)) +
        geom_hline(aes(yintercept = aLCL),colour ="red",size = 0.15,linetype= "dashed",summary(p3)) +
       facet_wrap(~facet1)+ 
       # facet_grid(facet1~.)+
        #rename tooltip attribute text. 
        labs(title = paste(input$toggleKPI,input$select_chart,"- Chart") ,
             y = paste(input$toggleKPI,"(Mins)"), x = "Date")
      
      ggplotly(ggp3, tooltip=c("text"))
      
      
      })  
      
    })
    
    
    #Fishbone Diagram
    
    output$fishbone <- renderPlot({ 
      
      ss.ceDiag(effect,causes.head,causes,sub="ISSS608 Visual Analytics Project",ss.col = c("","red"))
    })
    
       
}



# Run the application 
shinyApp(ui = ui, server = server)
