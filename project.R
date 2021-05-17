library(shiny)
library(tidyverse)
library(purrr)

list1 = read_csv("List.csv")

ui <- fluidPage(
  navlistPanel(
    id = "tabset",
    "Preparation",
    tabPanel("Setup", 
             titlePanel("Setup"),
             fluidRow(
               column(5,
                 selectInput("provider", label = "Provider Name",
                             choices = list1$Providers)
               ),
               column(5,
                 selectInput("service", label = "Service Name",
                             choices = list1$Services)
               )
             ),
             fluidRow(
               column(5,
                 numericInput("No_modalities", label = "Number of modalities/
                              subspecialties required",
                              value = 1, min = 1, max = 100, step = 1)
               ),
               column(5,
                 selectInput("planned_activity",
                 label = "Does the planned activity constitute a significant 
                          portion of your workload?",
                             choices = list("Yes","No"), selected = "No")
               )
             ),
             fluidRow(
               column(5,
                 selectInput("emergency", label = "Is there a significant amount
                             of same day (eg. emergency) activity?",
                             choices = list("Yes","No"), selected = "No")
               ),
               column(5,
                 selectInput("unit_of_work", label = "Unit of work",
                             choices = list("Patients","Minutes","Points"),
                             selected = "Minutes")
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 textInput("urgency1", label = "Urgency 1", value = "2WW"),
                 textInput("urgency2", label = "Urgency 2", value = "Urgent"),
                 textInput("urgency3", label = "Urgency 3", value = "Routine"),
                 width = 5
               ),
               mainPanel(
                 sliderInput("lower_perc", label = "Lower bound for percentile
                             calculation",
                             value = 65, min = 0, max = 100, step = 1),
                 width = 5
               )
             ),
             fluidRow(
               column(5,
                 uiOutput("modality")
               ),
               column(5,
                 uiOutput("waiting")
               )
             ),
             sidebarLayout(
               sidebarPanel(actionButton("jump_to_demand","Next"),width = 5
               ),
               mainPanel("", width = 5)
             )),
    tabPanel("Demand",
             titlePanel("Demand"),
             sidebarLayout(
               sidebarPanel(
                 dateInput("week_starting", label = "Week Starting",
                           format = "dd-mm-yyyy",
                           value = as.Date("01-04-2016", format="%d-%m-%Y")),
                 h5("Planned Activity?"),
                 verbatimTextOutput("planned_status"),
                 h5("Emergency?"),
                 verbatimTextOutput("emergency_status"),
                 uiOutput("ui_modality_minutes"),
                           width = 2
               ),
               mainPanel("", width = 8)
             )
             , value = "panel2"),
    tabPanel("panel 3", "Panel three contents"),
    widths = c(2, 10)
  )
)



server <- function(input, output, session) {
### Setup ####################################################################
  # dynamic UI for number of modality
  modality_names <- reactive(paste0("Modality ", seq_len(input$No_modalities)))
  waiting_list <- reactive(paste0("Waiting list ",
                                  seq_len(input$No_modalities)))
  modality_minutes <- reactive(paste0("Modality minutes ",
                                      seq_len(input$No_modalities)))
  
  output$modality <- renderUI({
    map(modality_names(), ~ textInput(.x, label = paste(.x," Name"),
                                         value = isolate(input[[.x]])))
  })
  output$waiting <- renderUI({
    map(waiting_list(), ~ numericInput(.x, label = paste(.x, " size"),
                                      value = isolate(input[[.x]])))
  })
  
  # next button logic
  observeEvent(input$jump_to_demand, {
    updateTabsetPanel(session, "tabset",
                      selected = "panel2")
  })

### Demand ###################################################################
  # planned activity
  output$planned_status = renderText(input$planned_activity)
  # emergency activity
  output$emergency_status = renderText(input$emergency)
  # dynamic UI for different modalities to input minutes
  output$ui_modality_minutes <- renderUI({
    if (input$unit_of_work == "Minutes") {
      map2(modality_minutes(), map_chr(modality_names(),~input[[.x]]%||% ""),
           ~ numericInput(.x, label = .y,value = isolate(input[[.x]])))
    } else {
      ""
    }
  })
  
}

shinyApp(ui, server)
























































