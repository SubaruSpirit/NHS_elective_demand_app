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

reactlog::reactlog_enable()
list1 = read_csv("List.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Core Model"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabset",
      menuItem("Setup", tabName = "Setup", icon = icon("play-circle")),
      menuItem("Demand", tabName = "Demand", icon = icon("user")),
      menuItem("SPC Chart", tabName = "SPC", icon = icon("dashboard")),
      menuItem("Capacity", tabName = "Capacity", icon = icon("bed")),
      menuItem("Capacity Summary", tabName = "Capacity_Summary",
               icon = icon("bed"))
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
              )
      ),
      
      # Demand
      tabItem(tabName = "Demand",
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
      ),
      
      # SPC Chart
      tabItem(tabName = "SPC",
              titlePanel("SPC Chart"),
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
                  dateRangeInput(
                    "baseline_date", label = "Season 1",
                    format = "dd-mm-yyyy"
                  ),
                  actionButton("jump_to_capacity","Next")
                  ,width = 3
                ),
                mainPanel(
                  plotlyOutput( "plot1"),
                  htmlOutput("nelson")
                  , width = 7
                )
              )
      ),
      
      # Capacity
      tabItem(tabName = "Capacity",
              titlePanel("Capacity"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("capacity_type",
                              label = "Basic Or Planning",
                              choices = list("Basic","Planning"), selected = "Basic"),
                  dateInput("capacity_start", label = "Start Date",
                            format = "dd-mm-yyyy",
                            value = as.Date("01-05-2019", format="%d-%m-%Y")),
                  dateInput("capacity_end", label = "End Date (Don't USE)",
                            format = "dd-mm-yyyy",
                            value = as.Date("30-04-2020", format="%d-%m-%Y")),
                  verbatimTextOutput("test10"),
                  selectInput("capacity_unit",
                              label = "Units",
                              choices = list("Patients","Minutes"), selected = "Patients"),
                  actionButton("jump_to_capacity_summary","Next"),
                  width = 2
                ),
                mainPanel(
                  fluidRow(
                    column(4,
                           titlePanel("Core Capacity")
                    ),
                    column(4,
                           downloadButton("download2", "Download Core Capacity")
                    ),
                    column(4,
                           fileInput("file_import2",
                                     "Import Core Capacity", accept = c(".csv", ".tsv"))
                    )
                  ),
                  fluidRow(
                    reactableOutput("template2")),
                  fluidRow(
                    column(4,
                           titlePanel("Ad-hoc Capacity")
                    ),
                    column(4,
                           downloadButton("download3", "Download Ad-hoc")
                    ),
                    column(4,
                           fileInput("file_import3",
                                     "Import Adhoc Capacity", accept = c(".csv", ".tsv"))
                    )
                  ),
                  fluidRow(
                    reactableOutput("template3"))
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
                  width = 8
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
    updateTabItems(session, "tabset",
                   selected = "SPC")
  })
  
  ### SPC chart ###############################################################
  # update the data range when click the next button
  observeEvent(input$file_import, {
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
  
  
  # plot the SPC chart
  output$plot1 = renderPlotly({   
    # filter the date as the input in the dateRangeInput, df7 is the baseline
    df7 = dplyr::filter( if(input$emergency=="No"){df6()}else{dfE3()},
                         between(Date, input$baseline_date[1],
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
    df8 = if(input$emergency=="No"){df6()}else{dfE3()}
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
### above are all plotly ##################################################
  
  
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
  # Capacity dates
  
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
    
    # End Date in read only ////// test10
    output$test10 = renderText({
      as.character(last(cdf1()[["Date"]]))
      })
  })

  
  # capacity template
  # core capacity
  core_data = reactive({
    if(input$capacity_type == "Basic"){
      tibble(Session_Name = rep("",10),
             Description1="", Description2="",
             Weeks_per_year="",
             Patients_per_clinic="")
    } else {
      add_column(tibble(Session_Name = rep("",10),
                        Description1="", Description2="",
                        Weeks_per_year="",
                        Patients_per_clinic=""),
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
  })
  
  core_import2 = reactive({
    add_column(core_import(),
               Total_patients = core_import()[["Weeks_per_year"]] * 
                 core_import()[["Patients_per_clinic"]],
               .after = "Patients_per_clinic")
  })
  
  core_import3 = reactive({
    add_column(core_import2(),
               Average_per_week = round(core_import2()[["Total_patients"]]/52),
               .after = "Total_patients")
  })
  
  
  # after import
  observeEvent(input$file_import2, {
    output$template2 <- renderReactable({
      reactable(core_import3())
    })
  })
  
  
  
  # adhoc capacity
  adhoc_data = reactive({
    if(input$capacity_type == "Basic"){
      tibble(Session_Name = rep("",10),
             Description1="", Description2="",
             Weeks_per_year="",
             Patients_per_clinic="")
    } else {
      add_column(tibble(Session_Name = rep("",10),
                        Description1="", Description2="",
                        Weeks_per_year="",
                        Patients_per_clinic=""),
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
  })
  
  adhoc_import2 = reactive({
    add_column(adhoc_import(),
               Total_patients = adhoc_import()[["Weeks_per_year"]] * 
                 adhoc_import()[["Patients_per_clinic"]],
               .after = "Patients_per_clinic")
  })
  
  adhoc_import3 = reactive({
    add_column(adhoc_import2(),
               Average_per_week = round(adhoc_import2()[["Total_patients"]]/52),
               .after = "Total_patients")
  })
  
  
  # after import
  observeEvent(input$file_import3, {
    output$template3 <- renderReactable({
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
    add_column(cdf2(), total_core = sum(core_import3()[["Total_patients"]]),
               total_adhoc = sum(adhoc_import3()[["Total_patients"]]))
  })
  
  cdf4 = reactive({
    add_column(cdf3(), total = cdf3()[["total_core"]] + cdf3()[["total_adhoc"]])
  })
  
  output$plot2 = renderPlotly({
    ggplot(cdf4(),aes(x=Date_Monday, y=round(total_core/52)))+
      geom_ribbon(aes(ymin=0, ymax=round(total_core/52)), 
                  fill="blue")+
      geom_ribbon(aes(ymin=round(total_core/52), ymax=round(total/52)), 
                  fill="green")+
      ylim(0,round(cdf4()[["total"]][1]/52)+2)+
      scale_x_date(date_breaks = "1 week", date_labels = "%d-%m-%Y")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

  
  
  
  
  
}

shinyApp(ui, server)















