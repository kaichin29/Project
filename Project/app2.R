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
             'dplyr','qicharts2','ggQC','qcc','rmarkdown','gridExtra','ggpubr' )
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}


## loading the data
#PM_AGG <- read_csv("data/PM_AGG.csv")

## filtering the data

## filtering variables
max_date <- as.Date("2019-03-31")
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




sidebar <- dashboardSidebar(
  sidebarMenu(
   # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
   # menuItem("Widgets", icon = icon("th"), tabName = "widgets",
   #          badgeLabel = "new", badgeColor = "green")

    dateRangeInput('dateRange',
                   label = 'Date range',
                   start = min_date , end = max_date
    ),
    
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
                        label =  "Empty or Fully-loaded Container:",
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
                        selected = sort(unique(ZL_DF$EVENT_SHIFT_I))
    )


  )
)


body <- dashboardBody(
  
  
    navbarPage(
        theme = shinytheme("cerulean"), 
        "Understanding Prime Mover (PM) Waiting Time in Yard",
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
                            box(withSpinner(plotlyOutput("pareto_2")),title = "Terminal", solidHeader = TRUE,collapsible = TRUE),
                            box(withSpinner(plotlyOutput("pareto_3")),title = "Operation Type", solidHeader = TRUE,collapsible = TRUE),
                            box(withSpinner(plotlyOutput("pareto_4")),title = "Container Length", solidHeader = TRUE,collapsible = TRUE),
                            box(withSpinner(plotlyOutput("pareto_5")),title = "Container Type", solidHeader = TRUE,collapsible = TRUE),
                            box(withSpinner(plotlyOutput("pareto_6")),title = "Equipment Type", solidHeader = TRUE,collapsible = TRUE)
                            )
                 )
        ),
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
        
        tabPanel("Control Chart Analysis",
                 tabsetPanel(
                   tabPanel("XBar & S "),
                   tabPanel("I & MR"),
                   tabPanel("C & U & P")
                 )
                 
                 
                 )
        
        
    )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Title"),
  sidebar,
  body
)


##  server

server <- function(input, output, session) {
    

  makeReactiveBinding("ZL_DF_longwait")
  duration <- c(60)
  
  refreshData <- reactive ({
    
    ZL_DF_longwait  <- subset(ZL_DF,ZL_DF$PM_WAIT_TIME_Q >= duration)%>%
      filter(Terminal %in% input$f_Terminal) %>%
      filter(MOVE_OP_C %in% input$f_MOVE_OP_C) %>%
      filter(LENGTH_Q %in% input$f_LENGTH_Q ) %>%
      filter(EVENT_SHIFT_I %in% input$f_EVENT_SHIFT_I ) %>%
      filter(CNTR_ST_C %in% input$f_CNTR_ST_C ) %>%
      filter(CNTR_TYPE_C %in% input$f_CNTR_TYPE_C ) %>%
      filter(SHIFT_D >= input$dateRange[1] & SHIFT_D <= input$dateRange[2]) 
    
  })

  
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
      
    ZL_DF_longwait  <- refreshData()
    ggplotly(paretochart(ZL_DF_longwait$Terminal,title = FALSE))

    })
    output$pareto_3 <- renderPlotly({
      ZL_DF_longwait  <- refreshData()
      ggplotly(paretochart(ZL_DF_longwait$MOVE_OP_C,title = FALSE))
    })
    output$pareto_4 <- renderPlotly({
      ZL_DF_longwait  <- refreshData()
      ggplotly(paretochart(ZL_DF_longwait$LENGTH_Q,title = FALSE))
    })
    output$pareto_5 <- renderPlotly({
      ZL_DF_longwait  <- refreshData()
      ggplotly(paretochart(ZL_DF_longwait$CNTR_TYPE_C,title = FALSE))
    })
    output$pareto_6 <- renderPlotly({
      ZL_DF_longwait  <- refreshData()
      ggplotly(paretochart(ZL_DF_longwait$EQUIPMENT_TYPE_C,title = FALSE))
    })
    
    
    
    

}



# Run the application 
shinyApp(ui = ui, server = server)
