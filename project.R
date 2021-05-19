library(shiny)
library(tidyverse)
library(purrr)
library(tools)
library(vroom)
library(reactable)
library(reactlog)

reactlog::reactlog_enable()
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
               mainPanel(
                 fluidRow(
                   column(6,
                          titlePanel("Template")
                   ),
                   column(2,
                          downloadButton("download1", "Download Template")
                   )
                 ),
                 fluidRow(
                   reactableOutput("template")),
                 fluidRow(
                   column(4,
                          titlePanel("Import")
                   ),
                   column(4,
                          fileInput("file_import",
                                    "Import Data", accept = c(".csv", ".tsv"))
                   )
                 ),
                 fluidRow(
                   reactableOutput("imported_data")),
                   width = 8
             ))
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
    # if the unit of work is minutes, then give users options to input
    if (input$unit_of_work == "Minutes") {
      map2(modality_minutes(), map_chr(modality_names(),~input[[.x]]%||% ""),
           ~ numericInput(.x, label = paste(.y," minutes"),value = isolate(input[[.x]])))
    } else {
      ""
    }
  })
  
  # build the data frame
  # reactivity
  date_seq1 = reactive({ymd(as.Date(input$week_starting,
                                    format="%d-%m-%Y"))})
  date_seq2 = reactive({ymd(as.Date(input$week_starting,
                                    format="%d-%m-%Y")%m+% years(2))})
  df1 = reactive({tibble(Year=strftime(seq(date_seq1(),
                                           date_seq2(),
                                           by = '1 week'), format = "%G"),
                         Week=strftime(seq(date_seq1(),
                                           date_seq2(),
                                           by = '1 week'), format = "%V"),
                         Date=seq(date_seq1(),
                                  date_seq2(),
                                  by = '1 week'))})
  
  # add a column of emergency if emergency is selected as Yes
  df2 = reactive({add_column(df1(),Emergency="")})
  
  output$template = renderReactable({
    if(input$emergency=="No"){
      reactable(df1())
    } else {reactable(df2())}
  })
  
  # if No_modality>1, add more columns of urgency to the data frame
  columnsToAdd = reactive({
    c(outer(c(input$urgency1,input$urgency2,input$urgency3),
            seq_len(input$No_modalities), paste0))
  })
  
    df3 = reactive({
      add_column(if(input$emergency=="No"){df1()} else {df2()}, 
                 !!!set_names(as.list(rep("",length(columnsToAdd()))),
                                                  nm=columnsToAdd()))
      })
    
    output$template = renderReactable({
      reactable(df3())
    })

  
  # download the data frame
  output$download1 <- downloadHandler(
    filename = function(){"user_template.csv"}, 
    content = function(file){
      write.csv(df3(), file,row.names = FALSE)
    }
  )
  
  # import the data
  df4 <- reactive({
    req(input$file_import)
    
    ext <- tools::file_ext(input$file_import$name)
    switch(ext,
           csv = vroom::vroom(input$file_import$datapath, delim = ","),
           tsv = vroom::vroom(input$file_import$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  output$imported_data <- renderReactable({
    reactable(df4())
  })
  
}

shinyApp(ui, server)
























































