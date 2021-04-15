library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggstatsplot)
library(ggplot2)
library(readr)
library(parallelPlot)
library(treemap)
library(RColorBrewer)
library(ggthemes)
library(forcats)
library(GGally)
library(plotly)
library(lubridate)
library(d3treeR)
library(knitr)
library(htmlwidgets)
library(htmltools)
library(shinythemes)
library(shinyWidgets)

## loading the data
PM <- read_delim("data/PM_201903.txt", delim = ",")

## filtering the data
PM <- PM %>% 
    filter(EVENT_C == "EQMT" | EVENT_C == "EQOF")

## formatting dates
PM$EVENT_DT <- ymd_hms(PM$EVENT_DT)
PM$day <- date(PM$EVENT_DT)
PM$hour <- hour(PM$EVENT_DT)

## aggregating data
PM_AGG <- PM %>%
    group_by(Terminal, PM_N, EVENT_SHIFT_I, MOVE_OP_C, LENGTH_Q, CNTR_TYPE_C, DG_I, REEFER_I, OVER_SIZE_I, EQUIPMENT_TYPE_C, day, hour) %>%
    summarise_if(is.numeric, sum) %>%
    ungroup()

## cleaning
PM_AGG$day <- as.Date(PM_AGG$day)

PM_AGG[,c(1:11)] <- lapply(PM_AGG[,c(1:11)],as.factor)
PM_AGG$hour <- as.factor(PM_AGG$hour)

PM_AGG$DG <- fct_collapse(PM_AGG$DG_I, 
                          DG = "Y",
                          STD = c("N", "NULL"))
PM_AGG$Reefer <- fct_collapse(PM_AGG$REEFER_I, 
                              REEF = "Y",
                              AMB = "N")
PM_AGG$LENGTH_Q <- fct_collapse(PM_AGG$LENGTH_Q, 
                                "20" = "20",
                                "40" = "40", 
                                "Others" = c("NULL", "00", "45"))
PM_AGG$OVER_SIZE_I <- fct_collapse(PM_AGG$OVER_SIZE_I, 
                                   "Yes" = "Y",
                                   "No" = c("N", "NULL"))


PM_AGG$CNTR_spec <- paste(PM_AGG$LENGTH_Q,PM_AGG$DG,PM_AGG$Reefer, sep = "-")
PM_AGG$CNTR_spec <- as.factor(PM_AGG$CNTR_spec)
PM_AGG$day <- date(PM_AGG$day)


## filtering variables
max_date <- as.Date("31-03-2019")

min_date <- as.Date("01-03-2019")


ui <- tagList(
    navbarPage(
        theme = shinytheme("cerulean"), 
        "Shiny Themes",
        tabPanel("Home",
                 sidebarPanel(
                     dateRangeInput('dateRange',
                                    label = 'Filter operations by date',
                                    start = as.Date('2019-03-01') , end = as.Date('2019-03-31')
                                    ),
                     
                     selectInput(
                         inputId = "column",
                         label = "Select list to filter from variable",
                         choices = list("LENGTH of Container" = "LENGTH_Q",
                                        "Shift" = "EVENT_SHIFT_I",
                                        "DG" = "DG", 
                                        "Reefer" = "Reefer",
                                        "Terminal ID" = "Terminal"),
                         selected = "Terminal"),
                     
                     pickerInput("value", "", choices = NULL, multiple = TRUE, 
                                 options = list(`actions-box` = TRUE)),
                     
                     verbatimTextOutput("selected")
                     
                     ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Introduction",
                                  h4("Hello There"),
                                  uiOutput("selector")
                                  ),
                         tabPanel("Glossary",
                                  h4("Key Definitions")
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
                                         choices = c("LENGTH of Container" = "LENGTH_Q",
                                                     "Shift" = "EVENT_SHIFT_I",
                                                     "DG" = "DG", 
                                                     "Reefer" = "Reefer",
                                                     "Terminal ID" = "Terminal"),
                                         selected = "LENGTH_Q"),
                             selectInput(inputId = "vsize", 
                                         label =  "Choose your independent variable:",
                                         choices = c("Waiting Time" = "PM_WAIT_TIME_Q",
                                                     "Travelling Time" = "PM_TRAVEL_TIME_Q"),
                                         selected = "PM_WAIT_TIME_Q")
                         ),
                         mainPanel(plotOutput("treemap")
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
                                                          "Terminal ID" = "Terminal"),
                                              selected = "LENGTH_Q"),
                                  selectInput(inputId = "bar_fill", 
                                              label =  "Choose your fill variable:",
                                              choices = c("LENGTH of Container" = "LENGTH_Q",
                                                          "Shift" = "EVENT_SHIFT_I",
                                                          "DG" = "DG", 
                                                          "Reefer" = "Reefer",
                                                          "Terminal ID" = "Terminal"),
                                              selected = "Terminal"),
                                  selectInput(inputId = "bar_facet", 
                                              label =  "Choose your grouping variable:",
                                              choices = c("LENGTH of Container" = "~LENGTH_Q",
                                                          "Shift" = "~EVENT_SHIFT_I",
                                                          "DG" = "~DG", 
                                                          "Reefer" = "~Reefer",
                                                          "Terminal ID" = "~Terminal",
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
                                              selected = "free_y")
                                  ),
                              mainPanel(plotlyOutput("bar")
                                        )
                              )
                 ),
        ),
        tabPanel("Confirmatory Data Analysis"),
        tabPanel("Time Series")
        )
)




##  server

server <- function(input, output, session) {
    
    
    observeEvent(input$column, {
        updatePickerInput(session, 
                          "value", 
                          paste("Select", input$column, "value(s)"),
                          choices = sort(unique(PM_AGG[[input$column]]))
                          )
    })
        
        output$selected <- renderText({
            paste0(input$column, ": ", paste(input$value, collapse = ", "))
        })
    

    output$treemap <- renderPlot({
        PM_group <- PM_AGG %>% 
            filter(day >= input$dateRange[1] & day <= input$dateRange[2]) 
        
        PM_group <- filter(PM_AGG, PM_AGG[[input$column]] %in% input$value)
        
        tree <- treemap(PM_group, index = c(input$tree_primary, "DG"), vSize = "PM_WAIT_TIME_Q", palette = "Blues", title = "Treemap for PM Waiting time by CNTR Specifications and Shift", fontsize.labels = c(15,10))
        return(tree)
        
    })
    output$bar <- renderPlotly({  

        PM_group <- PM_AGG %>% 
            filter(day >= input$dateRange[1] & day <= input$dateRange[2]) 
        
        PM_group <- filter(PM_AGG, PM_AGG[[input$column]] %in% input$value)
        
        bar_ID <- ggplot(data = PM_group, aes_string(x = input$bar_var, fill = input$bar_fill))+
            geom_bar(position = input$bar_type, alpha = 0.75, width = 0.5)+ 
            labs(title = paste("Count of ", input$bar_var, "by ", "input$bar_fill", "by ", "input$bar_facet"))+
            theme_few()+
            facet_wrap(as.formula(input$bar_facet), scales = input$bar_scale)+
            scale_fill_brewer(palette = "BuPu")
    
    barplot <- ggplotly(bar_ID)
    return(barplot)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
