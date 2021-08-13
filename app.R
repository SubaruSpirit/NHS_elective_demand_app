library(shiny)
library(tidyverse)
library(purrr)
library(tools)
library(vroom)
library(reactable)
library(reactlog)
library(Rspc)
library(plotly)
library(lubridate)
library(excelR)
library(shinydashboard)
library(DiagrammeR)
library(shinyjs)
library(glue)
library(shinyWidgets)
library(shinyalert)
library(prophet)

reactlog::reactlog_enable()
list1 = read_csv("List.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Core Model"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabset",
      menuItem("Setup", tabName = "Setup", icon = icon("play-circle")),
      menuItem("Demand", tabName = "Demand", icon = icon("user")),
      menuItem("Statistics Chart", tabName = "SPC", icon = icon("dashboard")),
      menuItem("Capacity", tabName = "Capacity", icon = icon("bed")),
      menuItem("Capacity Summary", tabName = "Capacity_Summary",
               icon = icon("zoom-in", lib = "glyphicon")),
      menuItem("Parameters", tabName = "Parameters",
               icon = icon("road", lib = "glyphicon")),
      menuItem("Pathway", tabName = "Pathway",
               icon = icon("calendar", lib = "glyphicon")),
      menuItem("Summary", tabName = "Summary", icon = icon("dashboard"))
    ),
    collapsed = F #T
  ),
  dashboardBody(
    tabItems(
      
      # Setup
      tabItem(tabName = "Setup",
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
                                   selected = "Patients")
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
              )
      ),
      
      # Demand
      tabItem(tabName = "Demand",
              useShinyalert(),
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
                  actionButton("jump_to_spc","Next"),
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
      ),
      
      # SPC Chart
      tabItem(tabName = "SPC",
              titlePanel("Statistics Chart"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(
                    "select_season", label = "Baseline Seasons",
                    choices = c("1","2","3","4"), selected = "1"
                  ),
                  h5("Baseline period"),
                  verbatimTextOutput(
                    "baseline_period"
                  ),
                  selectInput("baseline_start", label = "Season 1 begins",
                              choices = ""),
                  selectInput("baseline_end", label = "Season 1 ends",
                              choices = ""),
                  actionButton("jump_to_capacity","Next")
                  ,width = 3
                ),
                mainPanel(
                  plotlyOutput("plot1"),
                  htmlOutput("nelson"),
                  plotlyOutput("prophet_plot"),
                  plotOutput("trend_plot"),
                  dataTableOutput("testing99")
                  , width = 7
                )
              )
      ),
      
      # Capacity
      tabItem(tabName = "Capacity",
              titlePanel("Capacity"),
              # useShinujs package for disabling elements
              useShinyjs(),
              sidebarLayout(
                sidebarPanel(
                  selectInput("capacity_type",
                              label = "Basic Or Planning",
                              choices = list("Basic","Planning"), selected = "Basic"),
                  selectInput("core_customise",
                              label = "Customise Core Capacity?",
                              choices = list("Yes","No"), selected = "No"),
                  selectInput("adhoc_customise",
                              label = "Customise Adhoc Capacity?",
                              choices = list("Yes","No"), selected = "No"),
                  dateInput("capacity_start", label = "Start Date",
                            format = "dd-mm-yyyy",
                            value = as.Date("01-05-2019", format="%d-%m-%Y")),
                  dateInput("capacity_end", label = "End Date",
                            format = "dd-mm-yyyy",
                            value = as.Date("30-04-2020", format="%d-%m-%Y")),
                  disabled(selectInput("capacity_unit",
                              label = "Units",
                              choices = c("Patients", "Minutes"), selected = "Patients")),
                  actionButton("jump_to_capacity_summary","Next"),
                  width = 2
                ),
                mainPanel(
                  fluidRow(
                    column(4,
                           titlePanel("Core Capacity Template")
                    ),
                    column(4,
                           downloadButton("download2", "Download Core Capacity Template")
                    )
                  ),
                  fluidRow(
                    reactableOutput("template2")),
                  fluidRow(
                    column(4,
                           titlePanel("Ad-hoc Capacity Template")
                    ),
                    column(4,
                           downloadButton("download3", "Download Ad-hoc Template")
                    )
                  ),
                  fluidRow(
                    reactableOutput("template3")),
                  fluidRow(
                    column(4,
                           titlePanel("Core Capacity Import")
                    ),
                    column(4,
                           fileInput("file_import2",
                                     "Import Core Capacity", accept = c(".csv", ".tsv"))
                    )
                  ),
                  fluidRow(
                    reactableOutput("core_import1")),
                  fluidRow(
                    column(4,
                           titlePanel("Adhoc Capacity Import")
                    ),
                    column(4,
                           fileInput("file_import3",
                                     "Import Adhoc Capacity", accept = c(".csv", ".tsv"))
                    )
                  ),
                  fluidRow(
                    reactableOutput("adhoc_import1"),
                    textOutput("capacity_testing"))
                  , width = 8
                ))
      ),
      
      # Capacity Summary
      tabItem(tabName = "Capacity_Summary",
              titlePanel("Capacity Summary"),
              sidebarLayout(
                sidebarPanel(
                  "",
                  width = 2
                ),
                mainPanel(
                  plotlyOutput("plot2"),
                  dataTableOutput("capacity_summary_test"),
                  width = 8
                ))
      ),
      
      # Parameters
      tabItem(tabName = "Parameters",
              useShinyjs(),
              titlePanel("Parameters"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("parameter_baseline_period",
                              label = "Baseline period (wks)",
                              choices = seq(20,52),
                              selected = 52),
                  disabled(selectInput("parameter_units",
                              label = "Units",
                              choices = c("Patients", "Minutes"),
                              selected = "Patients")),
                  numericInputIcon(
                    inputId = "rott",
                    label = "2. ROTT",
                    value = 1.1,
                    step=0.1,
                    icon = list(NULL, icon("percent"))),
                  numericInputIcon(
                    inputId = "nasr",
                    label = "3. NASR: Discharged",
                    value = 11.3,
                    step=0.1,
                    icon = list(NULL, icon("percent"))),
                  numericInputIcon(
                    inputId = "nasl",
                    label = "5. NASL",
                    value = 10,
                    step=0.1,
                    icon = list(NULL, icon("percent"))),
                  numericInputIcon(
                    inputId = "nasl_discharged",
                    label = "6. NASL: Discharged",
                    value = 20,
                    step=0.1,
                    icon = list(NULL, icon("percent"))),
                  disabled(numericInputIcon(
                    inputId = "nasl_rebooked",
                    label = "7. NASL: Rebooked",
                    value = 80,
                    step=0.1,
                    icon = list(NULL, icon("percent")))),
                  width = 2
                ),
                mainPanel(
                  grVizOutput("parameter_plot"),
                  numericInput("real_attendances",
                               label = "How many attendances did you actually record
                               over the sample period?", min = 0,
                               max = 10000000, step=1, value = NA),
                  disabled(numericInput("estimated_demand",
                               label = "Based on this attendance level, your estimated
                               demand is", min = 0,
                               max = 10000000, step=1, value = NA)),
                  width = 8
                ))
      ),
      
      # Pathway
      tabItem(tabName = "Pathway",
              titlePanel("Pathway"),
              tags$head(
                tags$style(HTML("
      /* this will affect only the pre elements under the class myclass */
      .myclass1 pre {
        color: black;
        background-color: #FF4848;
        font-weight: bolder;
      }
      /* this will affect only the pre elements under the class myclass */
      .myclass2 pre {
        color: black;
        background-color: #FFB740;
        font-weight: bolder;
      }
      /* this will affect only the pre elements under the class myclass */
      .myclass3 pre {
        color: black;
        background-color: #64C9CF;
        font-weight: bolder;
      }                      
                                "
                                ))
              ),
              fluidRow(
                column(8,
                       ""
                ),

                column(2,
                       h4("Demand Split")
                )
              ),
              fluidRow(
                column(2,
                       div(class = "myclass1",
                           verbatimTextOutput("pathway1")
                       )
                ),
                column(2,
                       selectInput("pathway1_unit", "Unit", choices = c("Days","Weeks"),
                                   selected = "Weeks")
                ),
                column(1,
                       numericInput("pathway1_start", label = "Start", min = 0,
                                    max = 100, step=1, value = 0)
                ),
                column(1,
                       numericInput("pathway1_seenby", label = "Seen by", min = 0,
                                    max = 100, step=1, value = 0)
                ),
                column(2,
                       numericInput("pathway1_reb", label = "Rebookings seen in", min = 0,
                                    max = 100, step=1, value = 0)
                ),
                column(1,
                       div(class = "myclass1",
                           verbatimTextOutput("pd1")
                       )
                ),
                column(1,
                       uiOutput("percent1")
                )
                
              ),
              fluidRow(
                column(2,
                       div(class = "myclass2",
                           verbatimTextOutput("pathway2")
                       )
                ),
                column(2,
                       selectInput("pathway2_unit", "Unit", choices = c("Days","Weeks"),
                                   selected = "Weeks")
                ),
                column(1,
                       numericInput("pathway2_start", label = "Start", min = 0,
                                    max = 100, step=1, value = 0)
                ),
                column(1,
                       numericInput("pathway2_seenby", label = "Seen by", min = 0,
                                    max = 100, step=1, value = 0)
                ),
                column(2,
                       numericInput("pathway2_reb", label = "Rebookings seen in", min = 0,
                                    max = 100, step=1, value = 0)
                ),
                column(1,
                       div(class = "myclass2",
                           verbatimTextOutput("pd2")
                       )
                ),
                column(1,
                       uiOutput("percent2")
                )
              ),
              fluidRow(
                column(2,
                       div(class = "myclass3",
                           verbatimTextOutput("pathway3")
                       )
                ),
                column(2,
                       selectInput("pathway3_unit", "Unit", choices = c("Days","Weeks"),
                                   selected = "Weeks")
                ),
                column(1,
                       numericInput("pathway3_start", label = "Start", min = 0,
                                    max = 100, step=1, value = 0)
                ),
                column(1,
                       numericInput("pathway3_seenby", label = "Seen by", min = 0,
                                    max = 100, step=1, value = 0)
                ),
                column(2,
                       numericInput("pathway3_reb", label = "Rebookings seen in", min = 0,
                                    max = 100, step=1, value = 0)
                ),
                column(1,
                       div(class = "myclass3",
                           verbatimTextOutput("pd3")
                       )
                ),
                column(1,
                       uiOutput("percent3")
                )
              ),
              fluidRow(
                column(10,
                       plotOutput("pathway_plot"),
                       dataTableOutput("test_pathway"),
                       textOutput("text_pathway")
                )
              )
              
      ),
      
      # Summary
      tabItem(tabName = "Summary",
              titlePanel("Summary"),
              tags$head(
                tags$style(HTML("
      /* this will affect only the pre elements under the class myclass */
      .myclass8 pre {
        color: #FA8072;
        background-color: #002366;
        font-weight: bolder;
      }
      /* this will affect only the pre elements under the class myclass */
      .myclass9 pre {
        color: #FA8072;
        background-color: #002366;
        font-weight: bolder;
      }                  
                                "
                ))
              ),
              fluidRow(column(10,
                              div(class = "myclass8",
                                  verbatimTextOutput("summary1")
                              )
              )),
              sidebarLayout(
                sidebarPanel(
                  disabled(selectInput("summary_units", "Units",
                                       choices = c("Patients", "Minutes"),
                              selected = "Patients")),
                  selectInput("summary_option", "Use baseline or forecasted demand?",
                              choices = c("Baseline","Forecast"),
                              selected = "Baseline"),
                  selectInput("detail", "Show Detail", choices = c("Yes","No"),
                              selected = "Yes"),
                  dataTableOutput("summary_table1"),
                  dataTableOutput("summary_table2"),
                  dataTableOutput("summary_table3"),
                  width = 4
                ),
                mainPanel(
                  plotlyOutput("summary_plot1"),
                  plotlyOutput("summary_plot2"),
                  textOutput("summary_text"),
                  width = 6
                )),
              fluidRow(column(10,
                              div(class = "myclass9",
                                  verbatimTextOutput("summary2")
                              )
              )),
              sidebarLayout(
                sidebarPanel(
                 h5("Current Waiting list"),
                 verbatimTextOutput("current_waiting"),
                 h5("Sustainable Waiting list"),
                 verbatimTextOutput("sustainable_waiting"),
                 h5("Clearance"),
                 verbatimTextOutput("clearance"),
                 numericInput("clear_waiting",
                              label = "I would like to clear backlog in weeks",
                              value=0),
                 h5("This will require additional"),
                 verbatimTextOutput("additional_capacity"),
                  width = 4
                ),
                mainPanel(
                  plotlyOutput("sustainable_plot"),
                  dataTableOutput("summary_testing10"),
                  width = 6
                ))
      )
    )
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
    updateTabItems(session, "tabset",
                   selected = "Demand")
  })
  
  ### Demand ###################################################################
  # alert if it's minutes and users haven't input minutes
  observeEvent({
    input$tabset
    }, {
    if(input$unit_of_work == "Minutes" & 
       (0 %in% as.numeric(map_chr(modality_minutes(),~input[[.x]]%||% "")))
       ){
      shinyalert("Alert!", "Go back to Demand tab and input minutes", type = "error")
    } else {
      
    }
  })
  
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
    filename = function(){"demand_template.csv"}, 
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
  
  ###############################################################################    
  # data frame when emergency is selected yes, this is for plotting SPC
  columnsToAddE = reactive({
    c(outer(c(input$urgency1,input$urgency2,input$urgency3),
            seq_len(input$No_modalities), paste0))
  })
  
  dfE1 = reactive({
    dplyr::select(df4(), -dplyr::contains("Emergency"))
  })
  
  dfE2 = reactive({
    dfE1() %>%
      replace(is.na(.), 0) %>%
      rowwise() %>%mutate(Total_patients = sum(c_across(columnsToAddE())))
  })
  
  dfE3 = reactive({if(input$unit_of_work == "Minutes"){
    add_column(dfE2(), Total_minutes = sapply(
      seq(4,
          (ncol(dfE2())-1), length(columnsToAddE())/(input$No_modalities)), function(i){ 
            rowSums(dfE2()[, i + 0:( 
              (length(columnsToAddE())/(input$No_modalities)-1)
            )])
          }) %*% sapply(modality_minutes(), function(i){ input[[ i ]] } ))
  } else { dfE2() }
  })
  ###############################################################################  
  
  
  # show uploaded dataframe with total patients or total minutes
  output$imported_data <- renderReactable({
    reactable(df6())
  })
  
  # next button logic
  observeEvent(input$jump_to_spc, {
    if(input$unit_of_work == "Minutes" & 
       (0 %in% as.numeric(map_chr(modality_minutes(),~input[[.x]]%||% "")))
    ){
      shinyalert("Alert!", "Go back to Demand tab and input minutes", type = "error")
    } else {
      updateTabItems(session, "tabset",
                     selected = "SPC")
    }
    
  })
  
  ### SPC chart ###############################################################
  # baseline start date
  observeEvent(input$file_import, {
    updateSelectInput(
      session, "baseline_start", label = "Season 1 begins",
      # start is 52 weeks before the last week data, can CRASH if data is 
      # less than 52 weeks
      choices = format(as.Date(df6()[["Date"]], format="%Y-%m-%d"),"%d-%m-%Y"),
      selected = if(nrow(df6())>52){
        format(as.Date(df6()[[nrow(df6())-52,"Date"]], format="%Y-%m-%d"),"%d-%m-%Y")
      } else {format(as.Date(df6()[[nrow(df6())-1,"Date"]], format="%Y-%m-%d"),"%d-%m-%Y")}
    )
  })
  
  # baseline end date
  observeEvent(input$file_import, {
    updateSelectInput(
      session, "baseline_end", label = "Season 1 ends",
      # start is 52 weeks before the last week data, can CRASH if data is 
      # less than 52 weeks
      choices = format(as.Date(df6()[["Date"]], format="%Y-%m-%d"),"%d-%m-%Y"),
      selected = format(as.Date(last(df6()[["Date"]]), format="%Y-%m-%d"),"%d-%m-%Y")
    )
  })
  
  # baseline period length
  observeEvent(input$file_import, {
  output$baseline_period = renderText({
    paste(
      as.numeric(
        difftime(as.Date(input$baseline_end, format="%d-%m-%Y"),
                 as.Date(input$baseline_start, format="%d-%m-%Y"),
                 units = "weeks")
      ),"weeks")
  })
  })
  
  # plot the SPC chart
  output$plot1 = renderPlotly({   
    # filter the date as the input in the dateRangeInput, df7 is the baseline
    df7 = head(dplyr::filter( if(input$emergency=="No"){df6()}else{dfE3()},
                         between(Date, as.Date(input$baseline_start, format="%d-%m-%Y"),
                                 as.Date(input$baseline_end, format="%d-%m-%Y"))
    ),-1)
    
    # for testing//////////////////////////////////////////////////
    #output$testing99 = renderDataTable({df7})
    
    # generate mean, ucl, lcl, etc for the baseline data
    Mean = if(input$unit_of_work == "Minutes"){
      round(mean(df7$Total_minutes))
    } else {round(mean(df7$Total_patients))}
    
    UCL = if(input$unit_of_work == "Minutes"){
      round(CalculateLimits(x = df7$Total_minutes)$ucl)
    } else {round(CalculateLimits(x = df7$Total_patients)$ucl)}
    
    LCL = if(input$unit_of_work == "Minutes"){
      round(CalculateLimits(x = df7$Total_minutes)$lcl)
    } else {round(CalculateLimits(x = df7$Total_patients)$lcl)}
    
    UP = if(input$unit_of_work == "Minutes"){
      round(quantile(df7$Total_minutes, probs = 0.85))
    } else {round(quantile(df7$Total_patients, probs = 0.85))}
    
    LP = if(input$unit_of_work == "Minutes"){
      round(quantile(df7$Total_minutes, probs = (input$lower_perc)/100))
    } else {round(quantile(df7$Total_patients, probs = (input$lower_perc)/100))}
    
    
    # add nelson rules to entire dataset based on baseline data above
    df8 = if(input$emergency=="No"){df6()}else{dfE3()}
    df9 = add_column(df8, EvaluateRules(
      x = if(input$unit_of_work == "Minutes"){
        c(df8$Total_minutes)
      } else {c(df8$Total_patients)}, type = 'i', whichRules = c(1,5,3,2),
      lcl = LCL, cl= Mean, ucl = UCL
    ))
    
    
    # rectangle
    rect_range = tibble(xmin=as.Date(input$baseline_start, format="%d-%m-%Y"),
                        xmax=as.Date(input$baseline_end, format="%d-%m-%Y"),
                        ymin= if(input$unit_of_work == "Minutes"){
                          min(df8$Total_minutes)*0.9
                        } else {min(df8$Total_patients)*0.9},
                        ymax= if(input$unit_of_work == "Minutes"){
                          max(df8$Total_minutes)*1.1
                        } else {max(df8$Total_patients)*1.1})
    
    colours1 = c("UCL"="blue", "UP"="red", "LP"="purple", "Mean"="brown",
                 "LCL"="orange", "Rule4"="orange", "Rule1"="brown",
                 "Rule2"="green", "Rule3"="purple")
    # ggplot
    if(input$unit_of_work == "Minutes"){
      ggplot(data=df8)+
        geom_rect(data=rect_range, aes(xmin=xmin, xmax=xmax,
                                       ymin=ymin, ymax=ymax),
                  color="grey20",
                  alpha=0.2,
                  inherit.aes = FALSE)+
        geom_line(aes(x=Date, y=Total_minutes))+
        geom_hline(aes(col="UCL", yintercept = UCL), lty=3)+
        geom_hline(aes(col="UP", yintercept = UP), lty=2)+
        geom_hline(aes(col="LP", yintercept = LP), lty=2)+
        geom_hline(aes(col="Mean", yintercept = Mean), lty=4)+
        geom_hline(aes(col="LCL", yintercept = LCL), lty=3)+
        geom_point(data=add_row(filter(df9, Rule1=="1"),Date=df8[["Date"]][1]),
                   aes(x=Date,y=Total_minutes, col="Rule1"), shape=15)+
        geom_point(data=add_row(filter(df9, Rule5=="1"),Date=df8[["Date"]][1]),
                   aes(x=Date,y=Total_minutes, col="Rule2"), shape=15)+
        geom_point(data=add_row(filter(df9, Rule3=="1"),Date=df8[["Date"]][1]),
                   aes(x=Date,y=Total_minutes, col="Rule3"), shape=15)+
        geom_point(data=add_row(filter(df9, Rule2=="1"),Date=df8[["Date"]][1]),
                   aes(x=Date,y=Total_minutes, col="Rule4"), shape=15)+
        theme_bw()+
        labs(color="Legend", y="Total Minutes",
             title = "Demand-SPC Chart (Minutes)")+
        scale_color_manual(values = colours1)
    } else {
      ggplot(df8)+
        geom_rect(data=rect_range, aes(xmin=xmin, xmax=xmax,
                                       ymin=ymin, ymax=ymax),
                  color="grey20",
                  alpha=0.2,
                  inherit.aes = FALSE)+
        geom_line(aes(x=Date, y=Total_patients))+
        geom_hline(aes(col="UCL", yintercept = UCL), lty=3)+
        geom_hline(aes(col="UP", yintercept = UP), lty=2)+
        geom_hline(aes(col="LP", yintercept = LP), lty=2)+
        geom_hline(aes(col="Mean", yintercept = Mean), lty=4)+
        geom_hline(aes(col="LCL", yintercept = LCL), lty=3)+
        geom_point(data=add_row(filter(df9, Rule1=="1"),Date=df8[["Date"]][1]),
                   aes(x=Date,y=Total_patients, col="Rule1"), shape=15)+
        geom_point(data=add_row(filter(df9, Rule5=="1"),Date=df8[["Date"]][1]),
                   aes(x=Date,y=Total_patients, col="Rule2"), shape=15)+
        geom_point(data=add_row(filter(df9, Rule3=="1"),Date=df8[["Date"]][1]),
                   aes(x=Date,y=Total_patients, col="Rule3"), shape=15)+
        geom_point(data=add_row(filter(df9, Rule2=="1"),Date=df8[["Date"]][1]),
                   aes(x=Date,y=Total_patients, col="Rule4"), shape=15)+
        theme_bw()+
        labs(color="Legend", y="Total Minutes",
             title = "Demand-SPC Chart (Patients)")+
        scale_color_manual(values = colours1)
    }
    
  })
  ### above are all plotly1 ##################################################
  
  output$prophet_plot = renderPlotly({
    if(input$unit_of_work == "Patients"){
      df7 = select(df6(), Date, Total_patients)
      colnames(df7) = c("ds","y")
      
      m = prophet(df7, seasonality.mode = 'multiplicative', yearly.seasonality = T)
      future <- make_future_dataframe(m, periods = 52, freq = 'week')
      fcst <- predict(m, future)
      
      p = ggplot(fcst) +
        geom_ribbon(aes(x=as.Date(ds), y=yhat,ymin=yhat_lower, ymax=yhat_upper),
                    fill="#5089C6", alpha=0.3)+
        geom_line(aes(x=as.Date(ds), y=yhat,
                      text=paste(
                        "Prediction",
                        "\nPatients:", round(yhat),
                        "\nDate:", ds
                      ), group=1),
                  col="#001E6C")+
        geom_point(data=df7, aes(x=ds, y=y), col="#FFAA4C", size=0.5)+
        labs(x="Date", y="Patients",
             title = "Fitted Demand and Predictions for the next 52 weeks")
      ggplotly(p, tooltip = "text")
    } else {
      df7 = select(df6(), Date, Total_minutes)
      colnames(df7) = c("ds","y")
      
      m = prophet(df7, seasonality.mode = 'multiplicative', yearly.seasonality = T)
      future <- make_future_dataframe(m, periods = 52, freq = 'week')
      fcst <- predict(m, future)
      
      p = ggplot(fcst) +
        geom_ribbon(aes(x=as.Date(ds), y=yhat,ymin=yhat_lower, ymax=yhat_upper),
                    fill="#5089C6", alpha=0.3)+
        geom_line(aes(x=as.Date(ds), y=yhat,
                      text=paste(
                        "Prediction",
                        "\nMinutes:", round(yhat),
                        "\nDate:", ds
                      ), group=1),
                  col="#001E6C")+
        geom_point(data=df7, aes(x=ds, y=y), col="#FFAA4C", size=0.5)+
        labs(x="Date", y="Minutes",
             title = "Fitted Demand and Predictions for the next 52 weeks")
      ggplotly(p, tooltip = "text")
    }
    
  })
  
  output$trend_plot = renderPlot({
    if(input$unit_of_work == "Patients"){
      df7 = select(df6(), Date, Total_patients)
      colnames(df7) = c("ds","y")
      
      m = prophet(df7, seasonality.mode = 'multiplicative', yearly.seasonality = T)
      future <- make_future_dataframe(m, periods = 52, freq = 'week')
      fcst <- predict(m, future)
      prophet_plot_components(m, fcst)
    } else {
      df7 = select(df6(), Date, Total_minutes)
      colnames(df7) = c("ds","y")
      
      m = prophet(df7, seasonality.mode = 'multiplicative', yearly.seasonality = T)
      future <- make_future_dataframe(m, periods = 52, freq = 'week')
      fcst <- predict(m, future)
      prophet_plot_components(m, fcst)
    }
  })
  
  # Explanation of Nelson Rules
  output$nelson = renderUI({
    str1 = "Rule1: A data point is more than 3 sigma from the mean"
    str2 = "Rule2: 2 out of 3 points are more than 2 signma from the mean,
    with all 3 on the same side of the mean"
    str3 = "Rule3: 6 consecutively rising or falling points"
    str4 = "Rule4: a run of 9 or more points on the same side of the mean"
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  })
  
  
  # next button
  observeEvent(input$jump_to_capacity, {
    updateTabItems(session, "tabset",
                   selected = "Capacity")
  })
  
  ### Capacity #################################################################
  # unit of patients or minutes
  observeEvent(input$unit_of_work,{
    updateSelectInput(
      session,
      "capacity_unit",
      selected = input$unit_of_work)
  })
  
  
  # get the capacity start date from the last date in the demand
  observeEvent(input$file_import,{
    updateDateInput(
      session, "capacity_start",
      value = last(df6()[["Date"]])
    )
  })
  
  capacity_startDate = reactive({ymd(as.Date(input$capacity_start,
                                             format="%d-%m-%Y"))})
  capacity_endDate = reactive({ymd(as.Date(input$capacity_start,
                                           format="%d-%m-%Y")%m+% years(1))})
  
  # dataframe of 53 weeks of user selected date
  cdf1 = reactive({tibble(Year=strftime(seq(capacity_startDate(),
                                            capacity_endDate(),
                                            by = '1 week'), format = "%G"),
                          Week=strftime(seq(capacity_startDate(),
                                            capacity_endDate(),
                                            by = '1 week'), format = "%V"),
                          Date=seq(capacity_startDate(),
                                   capacity_endDate(),
                                   by = '1 week'))})
  
  # dataframe of 52 weeks of starting on monday
  cdf2 = reactive({
    head(add_column(cdf1(), Date_Monday=floor_date(cdf1()[["Date"]], "weeks",
                                                   week_start = 1)),-1)
  })
  
  capacity_time = reactive({
    cdf2()[["Date_Monday"]]
  })
  
  # update the capacity end date every time the capacity start updates
  observeEvent(input$capacity_start, {
    updateDateInput(
      session, "capacity_end",
      
      value = last(cdf1()[["Date"]])
    )
    disable("capacity_end")
  })
  
  
  # capacity template
  # core capacity
  core_data = reactive({
    if(input$core_customise == "No"){
      
      if(input$capacity_unit=="Patients"){
        tibble(Session_Name = rep("",1),
               Description1="", Description2="",
               Weeks_per_year="",
               Patients_per_clinic="")
      } else {
        tibble(Session_Name = rep("",1),
               Description1="", Description2="",
               Weeks_per_year="",
               Minutes_per_clinic="")
      }
      
    } else {
      add_column(
        if(input$capacity_unit=="Patients"){
          tibble(Session_Name = rep("",1),
                 Description1="", Description2="",
                 Weeks_per_year="",
                 Patients_per_clinic="")
        } else {
          tibble(Session_Name = rep("",1),
                 Description1="", Description2="",
                 Weeks_per_year="",
                 Minutes_per_clinic="")
        },
        
        !!!set_names(as.list(rep("",length(capacity_time()))),
                     nm=capacity_time()))
    }
  })
  
  output$template2 <- renderReactable({
    reactable(core_data())
  })
  
  output$download2 <- downloadHandler(
    filename = function(){"core_capacity.csv"}, 
    content = function(file){
      write.csv(core_data(), file,row.names = FALSE)
    }
  )
  
  
  core_import <- reactive({
    req(input$file_import2)
    
    ext <- tools::file_ext(input$file_import2$name)
    
    if(input$capacity_unit=="Patients"){
      switch(ext,
             csv = vroom::vroom(input$file_import2$datapath, delim = ",",
                                col_types = c(Session_Name = "c",
                                              Description1="c", Description2="c",
                                              Weeks_per_year="d",
                                              Patients_per_clinic="d")
             ),
             tsv = vroom::vroom(input$file_import2$datapath, delim = "\t",
                                col_types = c(Session_Name = "c",
                                              Description1="c", Description2="c",
                                              Weeks_per_year="d",
                                              Patients_per_clinic="d")
             ),
             validate("Invalid file; Please upload a .csv or .tsv file")
      )
    } else { 
      switch(ext,
             csv = vroom::vroom(input$file_import2$datapath, delim = ",",
                                col_types = c(Session_Name = "c",
                                              Description1="c", Description2="c",
                                              Weeks_per_year="d",
                                              Minutes_per_clinic="d")
             ),
             tsv = vroom::vroom(input$file_import2$datapath, delim = "\t",
                                col_types = c(Session_Name = "c",
                                              Description1="c", Description2="c",
                                              Weeks_per_year="d",
                                              Minutes_per_clinic="d")
             ),
             validate("Invalid file; Please upload a .csv or .tsv file")
      )
    }
    
  })
  
  core_import2 = reactive({
    if(input$capacity_unit=="Patients"){
      add_column(core_import(),
        Total_patients = core_import()[["Weeks_per_year"]] * 
                   core_import()[["Patients_per_clinic"]],
                 .after = "Patients_per_clinic")
    } else {
      add_column(core_import(),
        Total_minutes = core_import()[["Weeks_per_year"]] * 
                   core_import()[["Minutes_per_clinic"]],
                 .after = "Minutes_per_clinic")
    }
    
  })
  
  core_import3 = reactive({
    if(input$capacity_unit=="Patients") {
      add_column(core_import2(),
                 Average_per_week = round(core_import2()[["Total_patients"]]/52),
                 .after = "Total_patients")
    } else {
      add_column(core_import2(),
                 Average_per_week = round(core_import2()[["Total_minutes"]]/52),
                 .after = "Total_minutes")
    }
  })

  
  
  # after import
  observeEvent(input$file_import2, {
    output$core_import1 <- renderReactable({
      reactable(core_import3())
    })
  })
  
  
  
  # adhoc capacity
  adhoc_data = reactive({
    if(input$adhoc_customise == "No"){
      
      if(input$capacity_unit=="Patients"){
        tibble(Session_Name = rep("",1),
               Description1="", Description2="",
               Weeks_per_year="",
               Patients_per_clinic="")
      } else {
        tibble(Session_Name = rep("",1),
               Description1="", Description2="",
               Weeks_per_year="",
               Minutes_per_clinic="")
      }
      
    } else {
      add_column(
        if(input$capacity_unit=="Patients"){
          tibble(Session_Name = rep("",1),
                 Description1="", Description2="",
                 Weeks_per_year="",
                 Patients_per_clinic="")
        } else {
          tibble(Session_Name = rep("",1),
                 Description1="", Description2="",
                 Weeks_per_year="",
                 Minutes_per_clinic="")
        },
        
        !!!set_names(as.list(rep("",length(capacity_time()))),
                     nm=capacity_time()))
    }
  })
  
  output$template3 <- renderReactable({
    reactable(adhoc_data())
  })
  
  output$download3 <- downloadHandler(
    filename = function(){"adhoc_capacity.csv"}, 
    content = function(file){
      write.csv(adhoc_data(), file,row.names = FALSE)
    }
  )
  
  
  adhoc_import <- reactive({
    req(input$file_import3)
    
    ext <- tools::file_ext(input$file_import3$name)
    
    if(input$capacity_unit=="Patients"){
      switch(ext,
             csv = vroom::vroom(input$file_import3$datapath, delim = ",",
                                col_types = c(Session_Name = "c",
                                              Description1="c", Description2="c",
                                              Weeks_per_year="d",
                                              Patients_per_clinic="d")
             ),
             tsv = vroom::vroom(input$file_import3$datapath, delim = "\t",
                                col_types = c(Session_Name = "c",
                                              Description1="c", Description2="c",
                                              Weeks_per_year="d",
                                              Patients_per_clinic="d")
             ),
             validate("Invalid file; Please upload a .csv or .tsv file")
      )
    } else { 
      switch(ext,
             csv = vroom::vroom(input$file_import3$datapath, delim = ",",
                                col_types = c(Session_Name = "c",
                                              Description1="c", Description2="c",
                                              Weeks_per_year="d",
                                              Minutes_per_clinic="d")
             ),
             tsv = vroom::vroom(input$file_import3$datapath, delim = "\t",
                                col_types = c(Session_Name = "c",
                                              Description1="c", Description2="c",
                                              Weeks_per_year="d",
                                              Minutes_per_clinic="d")
             ),
             validate("Invalid file; Please upload a .csv or .tsv file")
      )
    }
    
  })
  
  adhoc_import2 = reactive({
    if(input$capacity_unit=="Patients"){
      add_column(adhoc_import(),
                 Total_patients = adhoc_import()[["Weeks_per_year"]] * 
                   adhoc_import()[["Patients_per_clinic"]],
                 .after = "Patients_per_clinic")
    } else {
      add_column(adhoc_import(),
                 Total_minutes = adhoc_import()[["Weeks_per_year"]] * 
                   adhoc_import()[["Minutes_per_clinic"]],
                 .after = "Minutes_per_clinic")
    }
    
  })
  
  adhoc_import3 = reactive({
    if(input$capacity_unit=="Patients") {
      add_column(adhoc_import2(),
                 Average_per_week = round(adhoc_import2()[["Total_patients"]]/52),
                 .after = "Total_patients")
    } else {
      add_column(adhoc_import2(),
                 Average_per_week = round(adhoc_import2()[["Total_minutes"]]/52),
                 .after = "Total_minutes")
    }
    
  })
  
  
  # after import
  observeEvent(input$file_import3, {
    output$adhoc_import1 <- renderReactable({
      reactable(adhoc_import3())
    })
  })
  
  
  # next button
  observeEvent(input$jump_to_capacity_summary, {
    updateTabItems(session, "tabset",
                   selected = "Capacity_Summary")
  })
  
  
  
  ### Capacity Summary #########################################################
  cdf3 = reactive({
    if(input$capacity_unit=="Patients"){
      add_column(cdf2(), total_core = sum(core_import3()[["Total_patients"]]),
                 total_adhoc = sum(adhoc_import3()[["Total_patients"]]))
    } else {
      add_column(cdf2(), total_core = sum(core_import3()[["Total_minutes"]]),
                 total_adhoc = sum(adhoc_import3()[["Total_minutes"]]))
    }
    
  })
  
  cdf4 = reactive({
    add_column(cdf3(), total = cdf3()[["total_core"]] + cdf3()[["total_adhoc"]])
  })
  
  cdf5 = reactive({
    if(input$capacity_unit=="Patients"){
      cdf4() %>%
        add_column(`Core Capacity (Patient)` = round(cdf4()[["total_core"]]/52)) %>%
        add_column(`Total Capacity (Patient)` = round(cdf4()[["total"]]/52))
    } else {
      cdf4() %>%
        add_column(`Core Capacity (Minutes)` = round(cdf4()[["total_core"]]/52)) %>%
        add_column(`Total Capacity (Minutes)` = round(cdf4()[["total"]]/52))
    }
    
  })
  
  colours2 = c("Core Capacity"="#185ADB", "Ad-hoc"="#FFC947")
  
  output$plot2 = renderPlotly({
    
    if (input$capacity_unit=="Patients") {
      ggplotly(
      ggplot(cdf5(),aes(x=Date_Monday, y=`Core Capacity (Patient)`))+
        geom_ribbon(aes(ymin=`Core Capacity (Patient)`, ymax=`Total Capacity (Patient)`,
                        fill="Ad-hoc",
                        text=paste(
                          "Adhoc Capacity:", cdf5()[["Total Capacity (Patient)"]]-
                            cdf5()[["Core Capacity (Patient)"]]
                        )))+
        geom_ribbon(aes(ymin=0, ymax=`Core Capacity (Patient)`, fill="Core Capacity",
                        text=paste(
                          "Core Capacity:",
                            cdf5()[["Core Capacity (Patient)"]]
                        )))+
        ylim(0,round(cdf5()[["total"]][1]/52)+2)+
        scale_x_date(date_breaks = "1 week", date_labels = "%d-%m-%Y")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(fill="Legend", y="Capacity (Patients)",
             title = "Capacity Summary (Patients)")+
        scale_fill_manual(values = colours2),
      tooltip = "text"
      )
    } else {
      ggplotly(
      ggplot(cdf5(),aes(x=Date_Monday, y=`Core Capacity (Minutes)`))+
        geom_ribbon(aes(ymin=`Core Capacity (Minutes)`,
                        ymax=`Total Capacity (Minutes)`,
                        fill="Ad-hoc",
                        text=paste(
                          "Adhoc Capacity:", cdf5()[["Total Capacity (Minutes)"]]-
                            cdf5()[["Core Capacity (Minutes)"]]
                        )))+
        geom_ribbon(aes(ymin=0, ymax=`Core Capacity (Minutes)`,
                        fill="Core Capacity",
                        text=paste(
                          "Core Capacity:",
                            cdf5()[["Core Capacity (Minutes)"]]
                        )))+
        ylim(0,round(cdf5()[["total"]][1]/52)+2)+
        scale_x_date(date_breaks = "1 week", date_labels = "%d-%m-%Y")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(fill="Legend", y="Capacity (Minutes)",
             title = "Capacity Summary (Minutes)")+
        scale_fill_manual(values = colours2),
      tooltip = "text"
      )
    }
    
  })
  
  
  ### Parameters ###########################################################
  observeEvent(input$unit_of_work,{
    updateSelectInput(session,
                      "parameter_units",
                      selected = input$unit_of_work)
  })
  
  observeEvent(input$nasl_discharged, {
    updateNumericInputIcon(
      session,
      "nasl_rebooked",
      value = 100 - input$nasl_discharged
    )
  })
  
  output$parameter_plot <- renderGrViz({
    
    # filter to the baseline date
    par1 = dplyr::filter( if(input$emergency=="No"){df6()}else{dfE3()},
                         between(Date, as.Date(input$baseline_start, format="%d-%m-%Y"),
                                 as.Date(input$baseline_end, format="%d-%m-%Y"))
    )
    
    output$parameter_table = renderDataTable({
      par1
    })
   
    # Define some sample data
    data = if(input$parameter_units=="Patients"){
      tibble(a = sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"]),
             b = floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                       * (input$nasr/100)),
             c = round(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                       * (input$rott/100)),
             d = sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"]) - 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                     * (input$nasr/100)) - 
               round(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                     * (input$rott/100)) + 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                     * (input$nasl/100)*(input$nasl_rebooked/100)),
             e = floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                       * (input$nasl/100)),
             f = sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"]) - 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                     * (input$nasr/100)) - 
               round(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                     * (input$rott/100)) + 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                     * (input$nasl/100)*(input$nasl_rebooked/100)) - 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                     * (input$nasl/100)),
             g = floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                       * (input$nasl/100)*(input$nasl_rebooked/100)),
             h = round(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_patients"])
                       * (input$nasl/100)*(input$nasl_discharged/100)))
    } else {
      tibble(a = sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"]),
             b = floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                       * (input$nasr/100)),
             c = round(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                       * (input$rott/100)),
             d = sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"]) - 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                     * (input$nasr/100)) - 
               round(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                     * (input$rott/100)) + 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                     * (input$nasl/100)*(input$nasl_rebooked/100)),
             e = floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                       * (input$nasl/100)),
             f = sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"]) - 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                     * (input$nasr/100)) - 
               round(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                     * (input$rott/100)) + 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                     * (input$nasl/100)*(input$nasl_rebooked/100)) - 
               floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                     * (input$nasl/100)),
             g = floor(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                       * (input$nasl/100)*(input$nasl_rebooked/100)),
             h = round(sum(par1[(nrow(par1)-as.numeric(input$parameter_baseline_period)):(nrow(par1)-1),"Total_minutes"])
                       * (input$nasl/100)*(input$nasl_discharged/100)))
    }
      
    # estimated demand is:
    observeEvent(input$real_attendances, {
      updateNumericInput(
        session,
        "estimated_demand",
        value = round(input$real_attendances / (
          1-(input$rott/100)-(input$nasr/100)+(input$nasl/100) * 
            (input$nasl_rebooked/100) - (input$nasl/100)
        ))
          
      )
    })
    
    
    DiagrammeR::grViz(glue::glue(
      .open = "{{",
      .close = "}}", 
      
      "digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = rectangle, style = filled]

node [fillcolor = lightblue]
a [label = '@@1']

node [fillcolor = Beige]
b [label = '@@2']
c [label = '@@3']
d [label = '@@4']

node [fillcolor = mediumpurple1]
e [label = '@@5']
f [label = '@@6']
g [label = '@@7']

node [fillcolor = darkseagreen1]
h [label = '@@8']
i [label = '@@9']

  subgraph cluster1 {
    node [fixedsize = true, width = 3]
    b -> {c d}
  }
  
  subgraph cluster2 {
    node [fixedsize = true, width = 3]
    e -> f -> g -> e
  }
  
  subgraph cluster3 {
    node [fixedsize = true, width = 3]
    h
    i
  }

a -> b 
a -> e
f -> i
e -> h

}

[1]: '1. Est.Demand (n = {{data$a}})'
[2]: 'Patient leaves pathway'
[3]: '3. NASR: Discharged (n = {{data$b}})'
[4]: '2. ROTT (n = {{data$c}})'
[5]: '4. Clinical Slots Used (n = {{data$d}})'
[6]: '5. NASL (n = {{data$e}})'
[7]: '7. NASL: Rebooked (n = {{data$g}})'
[8]: '8. Expected Attendances (n = {{data$f}})'
[9]: '6. NASL: Discharged (n = {{data$h}})'

"))
    
  })
  
  
  
  ### Pathway ##############################################################
  output$pathway1 = renderText({input$urgency1})
  output$pathway2 = renderText({input$urgency2})
  output$pathway3 = renderText({input$urgency3})
  
  # percentage symbol
  output$percent1 <- renderText({ 
    as.character(icon("percent"))
  })
  output$percent2 <- renderText({ 
    as.character(icon("percent"))
  })
  output$percent3 <- renderText({ 
    as.character(icon("percent"))
  })
  
  pathway_df = reactive({
    tibble(type = c(input$urgency3,input$urgency2,input$urgency1),
           xmin = c(
             if(input$pathway3_unit=="Weeks"){
               input$pathway3_start
             } else {input$pathway3_start/7},
             if(input$pathway2_unit=="Weeks"){
               input$pathway2_start
             } else {input$pathway2_start/7},
             if(input$pathway1_unit=="Weeks"){
               input$pathway1_start
             } else {input$pathway1_start/7}),
           xmax = c(
             if(input$pathway3_unit=="Weeks"){
               input$pathway3_seenby
             } else {input$pathway3_seenby/7},
             if(input$pathway2_unit=="Weeks"){
               input$pathway2_seenby
             } else {input$pathway2_seenby/7},
             if(input$pathway1_unit=="Weeks"){
               input$pathway1_seenby
             } else {input$pathway1_seenby/7}),
           ymin = c(0.1,0.4,0.7),
           ymax = c(0.3,0.6,0.9))
  })

  output$pathway_plot = renderPlot({
    ggplot(pathway_df()) + 
      geom_rect(stat = "identity", aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                       ymax = ymax), fill="cornflowerblue") +
      geom_rect(data = filter(pathway_df(), type==input$urgency1)[,-1] %>%
                  mutate(xmin=xmax, xmax=xmax+
                           if(input$pathway1_unit=="Weeks"){
                             input$pathway1_reb
                           } else {input$pathway1_reb/7}),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill="darkseagreen3")+
      geom_rect(data = filter(pathway_df(), type==input$urgency2)[,-1] %>%
                  mutate(xmin=xmax, xmax=xmax+
                           if(input$pathway2_unit=="Weeks"){
                             input$pathway2_reb
                           } else {input$pathway2_reb/7}),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill="darkseagreen3")+
      geom_rect(data = filter(pathway_df(), type==input$urgency3)[,-1] %>%
                  mutate(xmin=xmax, xmax=xmax+
                           if(input$pathway3_unit=="Weeks"){
                             input$pathway3_reb
                           } else {input$pathway3_reb/7}),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill="darkseagreen3")+
      scale_y_continuous(breaks = c(0.2,0.5, 0.8),
                         labels = c(input$urgency3,input$urgency2,input$urgency1),
                         limits = c(0,1))+
      scale_x_continuous(breaks = seq(0,100))+
      theme_bw()+
      theme(legend.position = "none")+
      labs(x="Weeks waiting", title = "Waits for pathways (wks)")
    })
  
  
  # baseline period data refer to SPC section as it occurs first
  df7 = reactive({
    head(dplyr::filter( if(input$emergency=="No"){df6()}else{dfE3()},
                        between(Date, as.Date(input$baseline_start, format="%d-%m-%Y"),
                                as.Date(input$baseline_end, format="%d-%m-%Y"))
    ),-1)
  })
  
  # split demand for all urgencies
  output$pd1 = renderText({
    if(input$unit_of_work == "Patients"){
      round(
        (sum(df7()[,match(c(outer(input$urgency1,
                                  seq_len(input$No_modalities), paste0)),
                          colnames(df7()))]) /
           sum(df7()[["Total_patients"]])) *100
      )
    } else {
      round(
        (as.numeric(sapply(df7()[,match(c(outer(input$urgency1,
                                                seq_len(input$No_modalities),
                                                paste0)), colnames(df7()))],
                           sum) %*% as.numeric(map_chr(modality_minutes(),
                                                       ~input[[.x]]%||% ""))) /
           sum(df7()[["Total_minutes"]])) *100
      )
    }
  })
  
  output$pd2 = renderText({
    if(input$unit_of_work == "Patients"){
      round(
        (sum(df7()[,match(c(outer(input$urgency2,
                                  seq_len(input$No_modalities), paste0)),
                          colnames(df7()))]) /
           sum(df7()[["Total_patients"]])) *100
      )
    } else {
      round(
        (as.numeric(sapply(df7()[,match(c(outer(input$urgency2,
                                                seq_len(input$No_modalities),
                                                paste0)), colnames(df7()))],
                           sum) %*% as.numeric(map_chr(modality_minutes(),
                                                       ~input[[.x]]%||% ""))) /
           sum(df7()[["Total_minutes"]])) *100
      )
    }
  })
  
  output$pd3 = renderText({
    if(input$unit_of_work == "Patients"){
      round(
        (sum(df7()[,match(c(outer(input$urgency3,
                                  seq_len(input$No_modalities), paste0)),
                          colnames(df7()))]) /
           sum(df7()[["Total_patients"]])) *100
      )
    } else {
      round(
        (as.numeric(sapply(df7()[,match(c(outer(input$urgency3,
                                                seq_len(input$No_modalities),
                                                paste0)), colnames(df7()))],
                           sum) %*% as.numeric(map_chr(modality_minutes(),
                                                       ~input[[.x]]%||% ""))) /
           sum(df7()[["Total_minutes"]])) *100
      )
    }
  })
  
  

  
  
  ### Summary ###############################################################
  output$summary1 = renderText({
    "Required capacity vs available capacity (weekly)"
  })
  
  output$summary2 = renderText({
    "Current waiting list vs Estimated sustainable waiting list"
  })
  
  # minutes option only available when minutes is selected
  observeEvent(input$unit_of_work,{
      updateSelectInput(
        session,
        "summary_units",
        selected = input$unit_of_work
      )
  })
  
  # test\\\\\\
  #output$summary_text = renderPrint({
  #})
  
  
  
  
  
  output$summary_table1 = renderDataTable({
    if(input$detail=="Yes"){
      if (input$summary_units=="Patients") {
        if (input$summary_option == "Baseline"){
          tibble(`Required Capacity`= c("85th Percentile","65th Percentile", "Average"),
                 `Per Week` = c(
                   round(
                     as.numeric(quantile(df7()[["Total_patients"]], 0.85)) *
                       (1-input$rott/100 - input$nasr/100 +
                          (input$nasl_rebooked/100) * (input$nasl/100))
                   ),
                   round(
                     as.numeric(quantile(df7()[["Total_patients"]], input$lower_perc/100)) *
                       (1-input$rott/100 - input$nasr/100 +
                          (input$nasl_rebooked/100) * (input$nasl/100))
                   ),
                   round(as.numeric(mean(df7()[["Total_patients"]])))
                 ),
                 Year = c("-","-",round(as.numeric(mean(df7()[["Total_patients"]]))*52)))
        } else {
          # forecast
          df_summary = select(df6(), Date, Total_patients)
          colnames(df_summary) = c("ds","y")
          
          m = prophet(df_summary, seasonality.mode = 'multiplicative', yearly.seasonality = T)
          future <- make_future_dataframe(m,
              periods = as.numeric(round(difftime(strptime(as.Date(input$capacity_start,format="%d-%m-%Y"),
                                                           format = "%Y-%m-%d"),
                                                  strptime(last(df_summary$ds),
                                                           format = "%Y-%m-%d"),
                                                  units="weeks"))+52),
              freq = 'week')
          fcst <- predict(m, future)
          
          tibble(`Required Capacity`= c("85th Percentile","65th Percentile", "Average"),
                 `Per Week` = c(
                   round(
                     as.numeric(quantile(tail(fcst,52)[["yhat"]], 0.85)) *
                       (1-input$rott/100 - input$nasr/100 +
                          (input$nasl_rebooked/100) * (input$nasl/100))
                   )
                   ,
                   round(
                     as.numeric(quantile(tail(fcst,52)[["yhat"]], input$lower_perc/100)) *
                       (1-input$rott/100 - input$nasr/100 +
                          (input$nasl_rebooked/100) * (input$nasl/100))
                   ),
                   round(as.numeric(mean(tail(fcst,52)[["yhat"]])))
                 ),
                 Year = c("-","-",round(as.numeric(mean(tail(fcst,52)[["yhat"]]))*52)))
        }

      } else {
        if (input$summary_option == "Baseline"){
          tibble(`Required Capacity`= c("85th Percentile","65th Percentile", "Average"),
                 `Per Week` = c(
                   round(
                     as.numeric(quantile(df7()[["Total_minutes"]], 0.85)) *
                       (1-input$rott/100 - input$nasr/100 +
                          (input$nasl_rebooked/100) * (input$nasl/100))
                   ),
                   round(
                     as.numeric(quantile(df7()[["Total_minutes"]], input$lower_perc/100)) *
                       (1-input$rott/100 - input$nasr/100 +
                          (input$nasl_rebooked/100) * (input$nasl/100))
                   ),
                   round(as.numeric(mean(df7()[["Total_minutes"]])))
                 ),
                 Year = c("-","-",round(as.numeric(mean(df7()[["Total_minutes"]]))*52)))
        } else {
          # forecast
          df_summary = select(df6(), Date, Total_minutes)
          colnames(df_summary) = c("ds","y")
          
          m = prophet(df_summary, seasonality.mode = 'multiplicative', yearly.seasonality = T)
          future <- make_future_dataframe(m,
                                          periods = as.numeric(round(difftime(strptime(as.Date(input$capacity_start,format="%d-%m-%Y"),
                                                                                       format = "%Y-%m-%d"),
                                                                              strptime(last(df_summary$ds),
                                                                                       format = "%Y-%m-%d"),
                                                                              units="weeks"))+52),
                                          freq = 'week')
          fcst <- predict(m, future)
          
          tibble(`Required Capacity`= c("85th Percentile","65th Percentile", "Average"),
                 `Per Week` = c(
                   round(
                     as.numeric(quantile(tail(fcst,52)[["yhat"]], 0.85)) *
                       (1-input$rott/100 - input$nasr/100 +
                          (input$nasl_rebooked/100) * (input$nasl/100))
                   ),
                   round(
                     as.numeric(quantile(tail(fcst,52)[["yhat"]], input$lower_perc/100)) *
                       (1-input$rott/100 - input$nasr/100 +
                          (input$nasl_rebooked/100) * (input$nasl/100))
                   ),
                   round(as.numeric(mean(tail(fcst,52)[["yhat"]])))
                 ),
                 Year = c("-","-",round(as.numeric(mean(tail(fcst,52)[["yhat"]]))*52)))
        }
        
      }

    } else {}
  },
  options = list(ordering=F, searching=F, lengthChange=F, paging=F, info=F))
  

  output$summary_table2 = renderDataTable({
    if(input$detail=="Yes"){
      if(input$summary_units=="Patients"){
        tibble(`Available Capacity`= c("Core","Ad-hoc"),
               `Per Week` = c(
                 round(sum(core_import3()[["Total_patients"]])/52),
                 round(sum(adhoc_import3()[["Total_patients"]])/52)
               ),
               Year = c(
                 sum(core_import3()[["Total_patients"]]),
                 sum(adhoc_import3()[["Total_patients"]])
               ))
      } else {
        tibble(`Available Capacity`= c("Core","Ad-hoc"),
               `Per Week` = c(
                 round(sum(core_import3()[["Total_minutes"]])/52),
                 round(sum(adhoc_import3()[["Total_minutes"]])/52)
               ),
               Year = c(
                 round(sum(core_import3()[["Total_minutes"]])),
                 round(sum(adhoc_import3()[["Total_minutes"]]))
               ))
      }

    } else {}
  },
  options = list(ordering=F, searching=F, lengthChange=F, paging=F, info=F))
  
  output$summary_table3 = renderDataTable({
    if(input$detail=="Yes"){
      tibble(`Reserved Capacity`= c("Reserved for non-elective"),
             `Per Week` = c(
               0
             ),
             Year = c(0))
    } else {}
  },
  options = list(ordering=F, searching=F, lengthChange=F, paging=F, info=F))
  
  # 1st plot in summary
  output$summary_plot1 = renderPlotly({
    if(input$summary_units=="Patients"){
      if (input$summary_option == "Baseline"){
        p = ggplot() +
          geom_rect(aes(xmin=1, xmax=5,
                        ymin=round(
                          as.numeric(quantile(df7()[["Total_patients"]], input$lower_perc/100)) *
                            (1-input$rott/100 - input$nasr/100 +
                               (input$nasl_rebooked/100) * (input$nasl/100))
                        ),
                        ymax=round(
                          as.numeric(quantile(df7()[["Total_patients"]], 0.85)) *
                            (1-input$rott/100 - input$nasr/100 +
                               (input$nasl_rebooked/100) * (input$nasl/100))
                        ),
                        text=paste(
                          "Required Capacity: From",round(
                            as.numeric(quantile(df7()[["Total_patients"]], input$lower_perc/100)) *
                              (1-input$rott/100 - input$nasr/100 +
                                 (input$nasl_rebooked/100) * (input$nasl/100))
                          ),
                          "to",round(
                            as.numeric(quantile(df7()[["Total_patients"]], 0.85)) *
                              (1-input$rott/100 - input$nasr/100 +
                                 (input$nasl_rebooked/100) * (input$nasl/100))
                          )
                        )), fill="#035397", color="#035397")+
          ylim(0,max(round(
            as.numeric(quantile(df7()[["Total_patients"]], 0.85)) *
              (1-input$rott/100 - input$nasr/100 +
                 (input$nasl_rebooked/100) * (input$nasl/100))
          ),round(sum(adhoc_import3()[["Total_patients"]])/52)+
            round(sum(core_import3()[["Total_patients"]])/52))*1.2)+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          geom_point(aes(x=3, y=round(sum(core_import3()[["Total_patients"]])/52),
                         text=paste(
                           "Available Core Capacity:",
                           round(sum(core_import3()[["Total_patients"]])/52)
                         )),
                     shape=4, col="#FFAA4C", cex=4, stroke=0.6)+
          geom_point(aes(x=3,
                         y=round(sum(adhoc_import3()[["Total_patients"]])/52)+
                           round(sum(core_import3()[["Total_patients"]])/52),
                         text=paste(
                           "Available Ad-hoc Capacity:",
                           round(sum(adhoc_import3()[["Total_patients"]])/52)
                         )),
                     shape=8, col="#FFAA4C", cex=4, stroke=0.6)+
          labs(y="Patients",
               title = "Required vs Available Capacity (Patients)")
        
        ggplotly(p, tooltip = "text")
      } else {
        # forecast
        df_summary = select(df6(), Date, Total_patients)
        colnames(df_summary) = c("ds","y")
        
        m = prophet(df_summary, seasonality.mode = 'multiplicative', yearly.seasonality = T)
        future <- make_future_dataframe(m,
                                        periods = as.numeric(round(difftime(strptime(as.Date(input$capacity_start,format="%d-%m-%Y"),
                                                                                     format = "%Y-%m-%d"),
                                                                            strptime(last(df_summary$ds),
                                                                                     format = "%Y-%m-%d"),
                                                                            units="weeks"))+52),
                                        freq = 'week')
        fcst <- predict(m, future)
        
        p = ggplot() +
          geom_rect(aes(xmin=1, xmax=5,
                        ymin=round(
                          as.numeric(quantile(tail(fcst,52)[["yhat"]], input$lower_perc/100)) *
                            (1-input$rott/100 - input$nasr/100 +
                               (input$nasl_rebooked/100) * (input$nasl/100))
                        ),
                        ymax=round(
                          as.numeric(quantile(tail(fcst,52)[["yhat"]], 0.85)) *
                            (1-input$rott/100 - input$nasr/100 +
                               (input$nasl_rebooked/100) * (input$nasl/100))
                        ),
                        text=paste(
                          "Required Capacity: From",round(
                            as.numeric(quantile(tail(fcst,52)[["yhat"]], input$lower_perc/100)) *
                              (1-input$rott/100 - input$nasr/100 +
                                 (input$nasl_rebooked/100) * (input$nasl/100))
                          ),
                          "to",round(
                            as.numeric(quantile(tail(fcst,52)[["yhat"]], 0.85)) *
                              (1-input$rott/100 - input$nasr/100 +
                                 (input$nasl_rebooked/100) * (input$nasl/100))
                          )
                        )), fill="#035397", color="#035397")+
          ylim(0,max(round(
            as.numeric(quantile(tail(fcst,52)[["yhat"]], 0.85)) *
              (1-input$rott/100 - input$nasr/100 +
                 (input$nasl_rebooked/100) * (input$nasl/100))
          ),round(sum(adhoc_import3()[["Total_patients"]])/52)+
            round(sum(core_import3()[["Total_patients"]])/52))*1.2)+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          geom_point(aes(x=3, y=round(sum(core_import3()[["Total_patients"]])/52),
                         text=paste(
                           "Available Core Capacity:",
                           round(sum(core_import3()[["Total_patients"]])/52)
                         )),
                     shape=4, col="#FFAA4C", cex=4, stroke=0.6)+
          geom_point(aes(x=3,
                         y=round(sum(adhoc_import3()[["Total_patients"]])/52)+
                           round(sum(core_import3()[["Total_patients"]])/52),
                         text=paste(
                           "Available Ad-hoc Capacity:",
                           round(sum(adhoc_import3()[["Total_patients"]])/52)
                         )),
                     shape=8, col="#FFAA4C", cex=4, stroke=0.6)+
          labs(y="Patients",
               title = "Required vs Available Capacity (Patients)")
        
        ggplotly(p, tooltip = "text")
      }

    }else{
      if (input$summary_option == "Baseline"){
        p = ggplot() +
          geom_rect(aes(xmin=1, xmax=5,
                        ymin=round(
                          as.numeric(quantile(df7()[["Total_minutes"]], input$lower_perc/100)) *
                            (1-input$rott/100 - input$nasr/100 +
                               (input$nasl_rebooked/100) * (input$nasl/100))
                        ),
                        ymax=round(
                          as.numeric(quantile(df7()[["Total_minutes"]], 0.85)) *
                            (1-input$rott/100 - input$nasr/100 +
                               (input$nasl_rebooked/100) * (input$nasl/100))
                        ),
                        text=paste(
                          "Required Capacity: From",round(
                            as.numeric(quantile(df7()[["Total_minutes"]], input$lower_perc/100)) *
                              (1-input$rott/100 - input$nasr/100 +
                                 (input$nasl_rebooked/100) * (input$nasl/100))
                          ),
                          "to",round(
                            as.numeric(quantile(df7()[["Total_minutes"]], 0.85)) *
                              (1-input$rott/100 - input$nasr/100 +
                                 (input$nasl_rebooked/100) * (input$nasl/100))
                          )
                        )), fill="#035397", color="#035397")+
          ylim(0,max(round(
            as.numeric(quantile(df7()[["Total_minutes"]], 0.85)) *
              (1-input$rott/100 - input$nasr/100 +
                 (input$nasl_rebooked/100) * (input$nasl/100))
          ),round(sum(adhoc_import3()[["Total_minutes"]])/52)+
            round(sum(core_import3()[["Total_minutes"]])/52))*1.2)+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          geom_point(aes(x=3, y=round(sum(core_import3()[["Total_minutes"]])/52),
                         text=paste(
                           "Available Core Capacity:",
                           round(sum(core_import3()[["Total_minutes"]])/52)
                         )),
                     shape=4, col="#FFAA4C", cex=4, stroke=0.6)+
          geom_point(aes(x=3,
                         y=round(sum(adhoc_import3()[["Total_minutes"]])/52)+
                           round(sum(core_import3()[["Total_minutes"]])/52),
                         text=paste(
                           "Available Ad-hoc Capacity:",
                           round(sum(adhoc_import3()[["Total_minutes"]])/52)
                         )),
                     shape=8, col="#FFAA4C", cex=4, stroke=0.6)+
          labs(y="Minutes",
               title = "Required vs Available Capacity (Minutes)")
        
        ggplotly(p, tooltip = "text")
      } else {
        # forecast
        df_summary = select(df6(), Date, Total_minutes)
        colnames(df_summary) = c("ds","y")
        
        m = prophet(df_summary, seasonality.mode = 'multiplicative', yearly.seasonality = T)
        future <- make_future_dataframe(m,
                                        periods = as.numeric(round(difftime(strptime(as.Date(input$capacity_start,format="%d-%m-%Y"),
                                                                                     format = "%Y-%m-%d"),
                                                                            strptime(last(df_summary$ds),
                                                                                     format = "%Y-%m-%d"),
                                                                            units="weeks"))+52),
                                        freq = 'week')
        fcst <- predict(m, future)
        
        p = ggplot() +
          geom_rect(aes(xmin=1, xmax=5,
                        ymin=round(
                          as.numeric(quantile(tail(fcst,52)[["yhat"]], input$lower_perc/100)) *
                            (1-input$rott/100 - input$nasr/100 +
                               (input$nasl_rebooked/100) * (input$nasl/100))
                        ),
                        ymax=round(
                          as.numeric(quantile(tail(fcst,52)[["yhat"]], 0.85)) *
                            (1-input$rott/100 - input$nasr/100 +
                               (input$nasl_rebooked/100) * (input$nasl/100))
                        ),
                        text=paste(
                          "Required Capacity: From",round(
                            as.numeric(quantile(tail(fcst,52)[["yhat"]], input$lower_perc/100)) *
                              (1-input$rott/100 - input$nasr/100 +
                                 (input$nasl_rebooked/100) * (input$nasl/100))
                          ),
                          "to",round(
                            as.numeric(quantile(tail(fcst,52)[["yhat"]], 0.85)) *
                              (1-input$rott/100 - input$nasr/100 +
                                 (input$nasl_rebooked/100) * (input$nasl/100))
                          )
                        )), fill="#035397", color="#035397")+
          ylim(0,max(round(
            as.numeric(quantile(tail(fcst,52)[["yhat"]], 0.85)) *
              (1-input$rott/100 - input$nasr/100 +
                 (input$nasl_rebooked/100) * (input$nasl/100))
          ),round(sum(adhoc_import3()[["Total_minutes"]])/52)+
            round(sum(core_import3()[["Total_minutes"]])/52))*1.2)+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          geom_point(aes(x=3, y=round(sum(core_import3()[["Total_minutes"]])/52),
                         text=paste(
                           "Available Core Capacity:",
                           round(sum(core_import3()[["Total_minutes"]])/52)
                         )),
                     shape=4, col="#FFAA4C", cex=4, stroke=0.6)+
          geom_point(aes(x=3,
                         y=round(sum(adhoc_import3()[["Total_minutes"]])/52)+
                           round(sum(core_import3()[["Total_minutes"]])/52),
                         text=paste(
                           "Available Ad-hoc Capacity:",
                           round(sum(adhoc_import3()[["Total_minutes"]])/52)
                         )),
                     shape=8, col="#FFAA4C", cex=4, stroke=0.6)+
          labs(y="Minutes",
               title = "Required vs Available Capacity (Minutes)")
        
        ggplotly(p, tooltip = "text")
      }
    }
  })
  
  
  ### waiting list ##########################################################
  current_waiting_list = reactive({
    if(input$unit_of_work=="Patients"){
      sum(as.numeric(map_chr(waiting_list(),~input[[.x]]%||% "")))
    } else {
      round(
      sum(as.numeric(map_chr(waiting_list(),~input[[.x]]%||% ""))) *
        (sum(df6()[["Total_minutes"]]) / sum(df6()[["Total_patients"]]))
      )
    }
  })
  
  output$current_waiting = renderText({
    current_waiting_list()
  })
  
  output$sustainable_waiting = renderText({
    paste(sustainable_list_lower(), "to", sustainable_list_higher())
  })
  
  output$clearance = renderText({
    if (current_waiting_list() - sustainable_list_lower()>0) {
      paste(0, "to", current_waiting_list() - sustainable_list_lower())
    } else {
      0
    }
  })
  
  observeEvent(input$clear_waiting,{
    updateNumericInput(
      session,
      "clear_waiting",
      label = paste(
        "I would like to clear backlog in", input$clear_waiting, "weeks"
      )
    )
  })
  
  output$additional_capacity = renderText({
    if(input$unit_of_work=="Patients"){
      paste(
        round((current_waiting_list() - sustainable_list_higher())/input$clear_waiting),
        "to",
        ceiling((current_waiting_list() - sustainable_list_lower())/input$clear_waiting),
        "patients per week"
      )
    } else {
      paste(
        round((current_waiting_list() - sustainable_list_higher())/input$clear_waiting),
        "to",
        ceiling((current_waiting_list() - sustainable_list_lower())/input$clear_waiting),
        "minutes per week"
      )
    }
  })
  
  
  urgency1_demand = reactive({
    if(input$unit_of_work == "Patients"){
      round(
        (sum(df7()[,match(c(outer(input$urgency1,
                                  seq_len(input$No_modalities), paste0)),
                          colnames(df7()))]) /
           sum(df7()[["Total_patients"]])) *100
      )
    } else {
      round(
        (as.numeric(sapply(df7()[,match(c(outer(input$urgency1,
                                                seq_len(input$No_modalities),
                                                paste0)), colnames(df7()))],
                           sum) %*% as.numeric(map_chr(modality_minutes(),
                                                       ~input[[.x]]%||% ""))) /
           sum(df7()[["Total_minutes"]])) *100
      )
    }
  })
  
  urgency2_demand = reactive({
    if(input$unit_of_work == "Patients"){
      round(
        (sum(df7()[,match(c(outer(input$urgency2,
                                  seq_len(input$No_modalities), paste0)),
                          colnames(df7()))]) /
           sum(df7()[["Total_patients"]])) *100
      )
    } else {
      round(
        (as.numeric(sapply(df7()[,match(c(outer(input$urgency2,
                                                seq_len(input$No_modalities),
                                                paste0)), colnames(df7()))],
                           sum) %*% as.numeric(map_chr(modality_minutes(),
                                                       ~input[[.x]]%||% ""))) /
           sum(df7()[["Total_minutes"]])) *100
      )
    }
  })
  
  urgency3_demand = reactive({
    if(input$unit_of_work == "Patients"){
      round(
        (sum(df7()[,match(c(outer(input$urgency3,
                                  seq_len(input$No_modalities), paste0)),
                          colnames(df7()))]) /
           sum(df7()[["Total_patients"]])) *100
      )
    } else {
      round(
        (as.numeric(sapply(df7()[,match(c(outer(input$urgency3,
                                                seq_len(input$No_modalities),
                                                paste0)), colnames(df7()))],
                           sum) %*% as.numeric(map_chr(modality_minutes(),
                                                       ~input[[.x]]%||% ""))) /
           sum(df7()[["Total_minutes"]])) *100
      )
    }
  })
  
  # forecast
  df_waiting = reactive({
    if(input$unit_of_work == "Patients"){
    select(df6(), Date, Total_patients) %>%
        dplyr::rename(ds=Date, y=Total_patients)
  } else {
    select(df6(), Date, Total_minutes) %>%
      dplyr::rename(ds=Date, y=Total_minutes)
  }
  })
  
  prophet_prep = reactive({
    prophet(df_waiting(), seasonality.mode = 'multiplicative',
              yearly.seasonality = T)
  })
  
  future <- reactive({
    make_future_dataframe(prophet_prep(),
                                  periods = as.numeric(round(difftime(strptime(as.Date(input$capacity_start,format="%d-%m-%Y"),
                                                                               format = "%Y-%m-%d"),
                                                                      strptime(last(df_waiting()[["ds"]]),
                                                                               format = "%Y-%m-%d"),
                                                                      units="weeks"))+52),
                                  freq = 'week')
  })
  
  fcst <- reactive({predict(prophet_prep(), future())})
  

  
  waiting1 = reactive({
    if(input$unit_of_work == "Patients"){
      if (input$summary_option == "Baseline"){
        round(
          ((input$pathway1_seenby - input$pathway1_start)/2 + input$pathway1_start) *
            (urgency1_demand()/100) *
            (sum(df7()[["Total_patients"]])/NROW(df7()[["Total_patients"]]))
        )
      } else {
        round(
          ((input$pathway1_seenby - input$pathway1_start)/2 + input$pathway1_start) *
            (urgency1_demand()/100) *
            (sum(tail(fcst(),52)[["yhat"]])/52)
        )
      }

    } else {
      if (input$summary_option == "Baseline"){
        round(
          ((input$pathway1_seenby - input$pathway1_start)/2 + input$pathway1_start) *
            (urgency1_demand()/100) *
            (sum(df7()[["Total_minutes"]])/NROW(df7()[["Total_minutes"]]))
        )
      } else {
        round(
          ((input$pathway1_seenby - input$pathway1_start)/2 + input$pathway1_start) *
            (urgency1_demand()/100) *
            (sum(tail(fcst(),52)[["yhat"]])/52)
        )
      }
    }
  })
  
  waiting2 = reactive({
    if(input$unit_of_work == "Patients"){
      if (input$summary_option == "Baseline"){
        round(
          ((input$pathway2_seenby - input$pathway2_start)/2 + input$pathway2_start) *
            (urgency2_demand()/100) *
            (sum(df7()[["Total_patients"]])/NROW(df7()[["Total_patients"]]))
        )
      } else {
        round(
          ((input$pathway2_seenby - input$pathway2_start)/2 + input$pathway2_start) *
            (urgency2_demand()/100) *
            (sum(tail(fcst(),52)[["yhat"]])/52)
        )
      }
    } else {
      if (input$summary_option == "Baseline"){
        round(
          ((input$pathway2_seenby - input$pathway2_start)/2 + input$pathway2_start) *
            (urgency2_demand()/100) *
            (sum(df7()[["Total_minutes"]])/NROW(df7()[["Total_minutes"]]))
        )
      } else {
        round(
          ((input$pathway2_seenby - input$pathway2_start)/2 + input$pathway2_start) *
            (urgency2_demand()/100) *
            (sum(tail(fcst(),52)[["yhat"]])/52)
        )
      }
    }
  })
  
  waiting3 = reactive({
    if(input$unit_of_work == "Patients"){
      if (input$summary_option == "Baseline"){
        round(
          ((input$pathway3_seenby - input$pathway3_start)/2 + input$pathway3_start) *
            (urgency3_demand()/100) *
            (sum(df7()[["Total_patients"]])/NROW(df7()[["Total_patients"]]))
        )
      } else {
        round(
          ((input$pathway3_seenby - input$pathway3_start)/2 + input$pathway3_start) *
            (urgency3_demand()/100) *
            (sum(tail(fcst(),52)[["yhat"]])/52)
        )
      }
    } else {
      if (input$summary_option == "Baseline"){
        round(
          ((input$pathway3_seenby - input$pathway3_start)/2 + input$pathway3_start) *
            (urgency3_demand()/100) *
            (sum(df7()[["Total_minutes"]])/NROW(df7()[["Total_minutes"]]))
        )
      } else {
        round(
          ((input$pathway3_seenby - input$pathway3_start)/2 + input$pathway3_start) *
            (urgency3_demand()/100) *
            (sum(tail(fcst(),52)[["yhat"]])/52)
        )
      }
    }
  })
  
  rebooking1 = reactive({
    if(input$unit_of_work == "Patients"){
      if (input$summary_option == "Baseline"){
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency1_demand()/100) *
          input$pathway1_reb *
          (sum(df7()[["Total_patients"]])/NROW(df7()[["Total_patients"]]))
      } else {
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency1_demand()/100) *
          input$pathway1_reb *
          (sum(tail(fcst(),52)[["yhat"]])/52)
      }
    } else {
      if (input$summary_option == "Baseline"){
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency1_demand()/100) *
          input$pathway1_reb *
          (sum(df7()[["Total_minutes"]])/NROW(df7()[["Total_minutes"]]))
      } else {
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency1_demand()/100) *
          input$pathway1_reb *
          (sum(tail(fcst(),52)[["yhat"]])/52)
      }
    }
  })
  
  rebooking2 = reactive({
    if(input$unit_of_work == "Patients"){
      if (input$summary_option == "Baseline"){
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency2_demand()/100) *
          input$pathway2_reb *
          (sum(df7()[["Total_patients"]])/NROW(df7()[["Total_patients"]]))
      } else {
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency2_demand()/100) *
          input$pathway2_reb *
          (sum(tail(fcst(),52)[["yhat"]])/52)
      }
    } else {
      if (input$summary_option == "Baseline"){
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency2_demand()/100) *
          input$pathway2_reb *
          (sum(df7()[["Total_minutes"]])/NROW(df7()[["Total_minutes"]]))
      } else {
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency2_demand()/100) *
          input$pathway2_reb *
          (sum(tail(fcst(),52)[["yhat"]])/52)
      }
    }
  })
  
  rebooking3 = reactive({
    if(input$unit_of_work == "Patients"){
      if (input$summary_option == "Baseline"){
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency3_demand()/100) *
          input$pathway3_reb *
          (sum(df7()[["Total_patients"]])/NROW(df7()[["Total_patients"]]))
      } else {
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency3_demand()/100) *
          input$pathway3_reb *
          (sum(tail(fcst(),52)[["yhat"]])/52)
      }
    } else {
      if (input$summary_option == "Baseline"){
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency3_demand()/100) *
          input$pathway3_reb *
          (sum(df7()[["Total_minutes"]])/NROW(df7()[["Total_minutes"]]))
      } else {
        (input$nasl_rebooked/100) * (input$nasl/100) * (urgency3_demand()/100) *
          input$pathway3_reb *
          (sum(tail(fcst(),52)[["yhat"]])/52)
      }
    }
  })
  
  sustainable_list_lower = reactive({
    round(round(
      waiting1() + waiting2() + waiting3() + 
        rebooking1() + rebooking2() + rebooking3()
    ) * 0.95)
  })
  
  sustainable_list_higher = reactive({
    round(round(
      waiting1() + waiting2() + waiting3() + 
        rebooking1() + rebooking2() + rebooking3()
    ) * 1.05)
  })
  
  output$sustainable_plot = renderPlotly({
    if(input$summary_units=="Patients"){
        p = ggplot() +
          geom_rect(aes(xmin=1, xmax=5,
                        ymin=sustainable_list_lower(),
                        ymax=sustainable_list_higher(),
                        text=paste(
                          "Sustainable Waiting List: From",
                          sustainable_list_lower(),
                          "to",
                          sustainable_list_higher()
                        )), fill="#D5EEBB", color="#D5EEBB")+
          ylim(0,max(current_waiting_list(),sustainable_list_higher())*1.2)+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          geom_point(aes(x=3, y=current_waiting_list(),
                         text=paste(
                           "Current Waiting List:",
                           current_waiting_list()
                         )),
                     shape=18, col="#5F7A61", cex=4, stroke=0.6)+
          labs(y="Patients",
               title = "Waiting list position (Patients)")
        
        ggplotly(p, tooltip = "text")
    } else{
      p = ggplot() +
        geom_rect(aes(xmin=1, xmax=5,
                      ymin=sustainable_list_lower(),
                      ymax=sustainable_list_higher(),
                      text=paste(
                        "Sustainable Waiting List: From",
                        sustainable_list_lower(),
                        "to",
                        sustainable_list_higher()
                      )), fill="#D5EEBB", color="#D5EEBB")+
        ylim(0,max(current_waiting_list(),sustainable_list_higher())*1.2)+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())+
        geom_point(aes(x=3, y=current_waiting_list(),
                       text=paste(
                         "Current Waiting List:",
                         current_waiting_list()
                       )),
                   shape=18, col="#5F7A61", cex=4, stroke=0.6)+
        labs(y="Minutes",
             title = "Waiting list position (Minutes)")
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  
  
}

shinyApp(ui, server)







