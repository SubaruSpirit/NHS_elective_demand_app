library(shiny)
library(tidyverse)
library(purrr)
library(tools)
library(vroom)
library(reactable)
library(reactlog)
library(Rspc)
library(plotly)

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
                      # only patients and minutes so far, points removed
                 selectInput("unit_of_work", label = "Unit of work",
                             choices = list("Patients","Minutes"),
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
                 fluidRow(
                   sidebarLayout(
                     sidebarPanel(actionButton("jump_to_spc","Next"),width = 5
                     ),
                     mainPanel("", width = 5)
                   )
                 ),
                   width = 8
             ))
             , value = "panel2"),
    tabPanel("SPC Chart",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "select_season", label = "Display type",
                   choices = c("1 season", "1+season")
                 ),
                 dateRangeInput(
                   "baseline_date", label = "Baseline selection",
                   format = "dd-mm-yyyy"
                 ),
                 h5("Baseline period"),
                 verbatimTextOutput(
                   "baseline_period"
                 )
                 ,width = 3
               ),
               mainPanel(
                 plotlyOutput(
                       "plot1"
                     )
                 , width = 7
               )
             )
             ,
             value = "panel3"),
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
           ~ numericInput(.x, label = paste(.y," minutes"),
                          value = 0))
    } else {
      ""
    }
  })
  
  # build the data frame
  # reactivity
  date_seq1 = reactive({ymd(as.Date(input$week_starting,
                                    format="%d-%m-%Y"))})
  date_seq2 = reactive({ymd(as.Date(input$week_starting,
                                    format="%d-%m-%Y")%m+% years(3))})
  df1 = reactive({tibble(Year=strftime(seq(date_seq1(),
                                           date_seq2(),
                                           by = '1 week'), format = "%G"),
                         Week=strftime(seq(date_seq1(),
                                           date_seq2(),
                                           by = '1 week'), format = "%V"),
                         Date=seq(date_seq1(),
                                  date_seq2(),
                                  by = '1 week'))})
  
  # if No_modality>1, add more columns of urgency to the data frame
  columnsToAdd = reactive({
    # columnsToAdd can be different based on emergency
    if(input$emergency=="No"){
      c(outer(c(input$urgency1,input$urgency2,input$urgency3),
              seq_len(input$No_modalities), paste0))
    } else {
      c(outer(c(input$urgency1,input$urgency2,input$urgency3, "Emergency"),
              seq_len(input$No_modalities), paste0))
    }
    
  })
  
    df3 = reactive({
      add_column(df1(), 
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
           csv = vroom::vroom(input$file_import$datapath, delim = ",",
                              col_types = c(
                                list(Date = col_date(format = "%d/%m/%Y")),
                                set_names(
                                  rep("d",length(columnsToAdd())) ,
                                  columnsToAdd())
                              )
                              ),
           tsv = vroom::vroom(input$file_import$datapath, delim = "\t",
                              col_types = c(
                                list(Date = col_date(format = "%d/%m/%Y")),
                                set_names(
                                  rep("d",length(columnsToAdd())) ,
                                  columnsToAdd())
                              )
                              ),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  # add total patients column
  df5 = reactive({
    df4() %>%
      replace(is.na(.), 0) %>%
      rowwise() %>%mutate(Total_patients = sum(c_across(columnsToAdd())))
  })
  
  # add total minutes column based on minutes
  df6 = reactive({if(input$unit_of_work == "Minutes"){
    add_column(df5(), Total_minutes = sapply(
      seq(4
          # no need for if statement for whether emergency or not as 
          # columnsToAdd is already conditional based on emergency
        , (ncol(df5())-1), length(columnsToAdd())/(input$No_modalities)), function(i){ 
        rowSums(df5()[, i + 0:( 
          (length(columnsToAdd())/(input$No_modalities)-1)
          )])
      }) %*% sapply(modality_minutes(), function(i){ input[[ i ]] } ))
  } else { df5() }
  })

  output$imported_data <- renderReactable({
    reactable(df6())
  })
  
  # next button logic
  observeEvent(input$jump_to_spc, {
    updateTabsetPanel(session, "tabset",
                      selected = "panel3")
  })
  
### SPC chart ###############################################################
  # update the data range when click the next button
  observeEvent(input$jump_to_spc, {
    updateDateRangeInput(
      session, "baseline_date",
      # start is 52 weeks before the last week data, can CRASH if data is 
      # less than 52 weeks
      start = if(nrow(df6())>52){
        df6()[[nrow(df6())-52,"Date"]]
      } else {df6()[[nrow(df6())-1,"Date"]]},
        
      end = last(df6()[["Date"]])
    )
  })
  
  output$baseline_period = renderText({
    paste(
      as.numeric(
        difftime(input$baseline_date[2], input$baseline_date[1],
                 units = "weeks")
      ),"weeks")
  })
  
  output$plot1 = renderPlotly({
    # filter the date as the input in the dateRangeInput, df7 is the baseline
    df7 = dplyr::filter(
      df6(), between(Date, input$baseline_date[1],
                            input$baseline_date[2])
    )
    
    # generate mean, ucl, lcl, etc for the baseline data
    Mean = if(input$unit_of_work == "Minutes"){
      mean(df7$Total_minutes)
    } else {mean(df7$Total_patients)}
    
    UCL = if(input$unit_of_work == "Minutes"){
      CalculateLimits(x = df7$Total_minutes)$ucl
    } else {CalculateLimits(x = df7$Total_patients)$ucl}
    
    LCL = if(input$unit_of_work == "Minutes"){
      CalculateLimits(x = df7$Total_minutes)$lcl
    } else {CalculateLimits(x = df7$Total_patients)$lcl}

    UP = if(input$unit_of_work == "Minutes"){
      quantile(df7$Total_minutes, probs = 0.85)
    } else {quantile(df7$Total_patients, probs = 0.85)}
    
    LP = if(input$unit_of_work == "Minutes"){
      quantile(df7$Total_minutes, probs = (input$lower_perc)/100)
    } else {quantile(df7$Total_patients, probs = (input$lower_perc)/100)}
    
    
    # add nelson rules to entire dataset based on baseline data above
    df8 = df6()
    df9 = add_column(df8, EvaluateRules(
      x = if(input$unit_of_work == "Minutes"){
        c(df8$Total_minutes)
      } else {c(df8$Total_patients)}, type = 'i', whichRules = c(1,5,3,2),
      lcl = LCL, cl= Mean, ucl = UCL
      ))
    
    # rectangle
    rect_range = tibble(xmin=as.Date(input$baseline_date[1]),
                        xmax=as.Date(input$baseline_date[2]),
                        ymin= if(input$unit_of_work == "Minutes"){
                          min(df8$Total_minutes)*0.95
                        } else {min(df8$Total_patients)*0.95},
                        ymax= if(input$unit_of_work == "Minutes"){
                          max(df8$Total_minutes)*1.05
                        } else {max(df8$Total_patients)*1.05})
    
    # ggplot
    if(input$unit_of_work == "Minutes"){
      ggplot(data=df6())+
        geom_rect(data=rect_range, aes(xmin=xmin, xmax=xmax,
                                       ymin=ymin, ymax=ymax),
                  color="grey20",
                  alpha=0.2,
                  inherit.aes = FALSE)+
        geom_line(aes(x=Date, y=Total_minutes))+
        geom_hline(yintercept = LP, lty=2, col="purple")+
        geom_hline(yintercept = UP, lty=2, col="red")+
        geom_hline(yintercept = UCL, lty=3, col="blue")+
        geom_hline(yintercept = LCL, lty=3, col="orange")+
        geom_hline(yintercept = Mean, lty=3, col="brown")+
        geom_point(data=filter(df9, Rule1=="1"), aes(x=Date,y=Total_minutes),
                   color="red", shape=15)+
        geom_point(data=filter(df9, Rule5=="1"), aes(x=Date,y=Total_minutes),
                   color="green", shape=15)+
        geom_point(data=filter(df9, Rule3=="1"), aes(x=Date,y=Total_minutes),
                   color="purple", shape=15)+
        geom_point(data=filter(df9, Rule2=="1"), aes(x=Date,y=Total_minutes),
                   color="orange", shape=15)+
        theme_bw()
    } else {
      ggplot(data=df6())+
        geom_rect(data=rect_range, aes(xmin=xmin, xmax=xmax,
                                       ymin=ymin, ymax=ymax),
                  color="grey20",
                  alpha=0.2,
                  inherit.aes = FALSE)+
        geom_line(aes(x=Date, y=Total_patients))+
        geom_hline(yintercept = LP, lty=2, col="purple")+
        geom_hline(yintercept = UP, lty=2, col="red")+
        geom_hline(yintercept = UCL, lty=3, col="blue")+
        geom_hline(yintercept = LCL, lty=3, col="orange")+
        geom_hline(yintercept = Mean, lty=3, col="brown")+
        geom_point(data=filter(df9, Rule1=="1"), aes(x=Date,y=Total_patients),
                   color="red", shape=15)+
        geom_point(data=filter(df9, Rule5=="1"), aes(x=Date,y=Total_patients),
                   color="green", shape=15)+
        geom_point(data=filter(df9, Rule3=="1"), aes(x=Date,y=Total_patients),
                   color="purple", shape=15)+
        geom_point(data=filter(df9, Rule2=="1"), aes(x=Date,y=Total_patients),
                   color="orange", shape=15)+
        theme_bw()
    }
      
  })
  # above are all plotly

  
  
}

shinyApp(ui, server)
























































