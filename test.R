library(tidyverse)
library(shiny)
library(pdftools)
library(tesseract)
library(tidytext)
library(reactable)

list1 = read_csv("raw.csv")

ui <- shinyUI(fluidPage(
  
  titlePanel("Testing File upload"),
  
  sidebarLayout(
    sidebarPanel(
      titlePanel("Demo"),
      fileInput("file_import", "Upload Files ( . pdf format only)",
                multiple = T, accept = ".pdf"),
      tableOutput("files"),
      selectInput("type", label = "Document Type",
                  choices = c(list1[["document_type"]],""),selected = ""),
      uiOutput("trial_number"),
      uiOutput("spec_id"),
      uiOutput("lot_number"),
      uiOutput("date"),
      uiOutput("vial_kit"),
      actionButton("approve","Approve"),
      textOutput("feedback"),
      actionButton("next_pdf","Next PDF"),
      textOutput("end")
    ),
    
    mainPanel(
      uiOutput("pdfview"),
      reactableOutput("test")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
  # table of the imported file
  output$files <- renderTable({input$file_import})
  
  ### display the pdf ########################################################
  x = reactiveVal(1)
  
  observeEvent(input$file_import,{
    
    file.rename(input$file_import$datapath[x()], "0.pdf")
    file.copy("0.pdf","www", overwrite = T)
    
    output$pdfview <- renderUI({
      tags$iframe(style="height:1200px; width:100%", src="0.pdf")
    })
    
    ### OCR ###########################################################
    pngfile1 <- reactive({pdftools::pdf_convert("0.pdf",
                                                dpi = 300)})
    text1 <- reactive({tesseract::ocr(pngfile1())})
    df1 = reactive({as_tibble(text1())})
    r1 = reactive({df1() %>%
        unnest_tokens(word, value, to_lower = F)})
    ########################################################################
    
    # document type
    updateSelectInput(session, "type", selected = 
                        if((which(grepl("\\<LRA\\>", r1()[["word"]]))[1] +1 ==
                            which(grepl("\\<Spec\\>", r1()[["word"]]))[1]) &
                           (which(grepl("\\<Spec\\>", r1()[["word"]]))[1] +1 ==
                            # %in% TRUE: return FALSE when it's NA
                            which(grepl("\\<ID\\>", r1()[["word"]]))[1])%in% TRUE)
                        {"Label Specification Approval Form (ie variable text approval)"}
                      else if ((which(grepl("\\<P\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<L\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<L\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<Plan\\>", r1()[["word"]]))[1])%in% TRUE){
                        "Pack and Label Plan"}
    )
    
    
    # trial number
    output$trial_number = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
      } else if (input$type=="Pack and Label Plan"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
      }
    })
    
    # spec id
    output$spec_id = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("spec_id", label = "LRA Spec ID",
                  value = 
                    paste(as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))+2,]),
                          "-",
                          as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))+3,]),
                          sep=""
                    )
        )
      } else{""}
    })
    
    # lot number
    output$lot_number = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("lot_number", label = "PMD Orderable Lot Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))[1]+2,])
        )
      } else if (input$type=="Pack and Label Plan"){
        textInput("lot_number", label = "PMD Orderable Lot Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))[1]+1,])
        )
      }
    })
    
    # date
    output$date = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("date", label = "Date",
                  value = 
                    paste(as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+1,]),
                          "-",
                          as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+2,]),
                          "-",
                          as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+3,]),
                          sep=""
                    )
        )
      } else if(input$type=="Pack and Label Plan"){
        textInput("date", label = "Date",
                  value = 
                    paste(
                      as.character(r1()[last(which(grepl("Approved", r1()[["word"]])))+4,]),
                      as.character(r1()[last(which(grepl("Approved", r1()[["word"]])))+5,]),
                      as.character(r1()[last(which(grepl("Approved", r1()[["word"]])))+6,]),
                      sep = "-"
                    )
        )
      }
    })
    
    # vial or kit
    output$vial_kit = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                    selected = if(length(which(grepl("PRIMARY", r1()[["word"]])))==1){
                      "Vial"
                    } else{"Kit"}
        )
      } else{""}
    })
    
    # put info into df and display
    output$feedback = renderText("")
    
    df2 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = input$spec_id,
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = input$vial_kit
        )
      } else if (input$type=="Pack and Label Plan"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = ""
        )
      }
    })
    
    df3 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Specification Approval",
          df2()[["label_spec_id"]],df2()[["pmd_lot_number"]],
          df2()[["vial_kit"]],df2()[["date"]],
          ".pdf"
        ))
      } else if (input$type=="Pack and Label Plan"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"PL Plan", df2()[["pmd_lot_number"]],
          df2()[["date"]], ".pdf"
        ))
      }
    })
    
    # display the dataframe (will be deleted once live)
    output$test <- renderReactable({
      reactable(df3())
    })
    
    
  })
  
  
  ### approve ################################################################
  observeEvent(input$approve, {
    output$feedback = renderText("Oh Yeah!")
    
    df2 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = input$spec_id,
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = input$vial_kit
        )
      } else if (input$type=="Pack and Label Plan"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = ""
        )
      }
    })
    
    df3 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Specification Approval",
          df2()[["label_spec_id"]],df2()[["pmd_lot_number"]],
          df2()[["vial_kit"]],df2()[["date"]],
          ".pdf"
        ))
      } else if (input$type=="Pack and Label Plan"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"PL Plan", df2()[["pmd_lot_number"]],
          df2()[["date"]], ".pdf"
        ))
      }
    })
    
    file.rename("0.pdf",
                df3()[["pdf_name"]]
    )
    file.copy(df3()[["pdf_name"]], "final")
    
    # append to csv
    write.table(df3(),"test.csv", append = T, col.names = F, row.names = F ,
                sep = ",")
    
    file.copy("test.csv", "final", overwrite = T)
    
    
  })
  
  
  ### next pdf ##############################################################
  observeEvent(input$next_pdf, {
    
    if(x()<length(input$file_import$datapath)){
      x(x()+1)
      file.rename(input$file_import$datapath[x()], "0.pdf")
      file.copy("0.pdf","www", overwrite = T)
      
      output$pdfview <- renderUI({
        tags$iframe(style="height:1200px; width:100%", src="0.pdf")
      })
      
      ### OCR ###########################################################
      pngfile1 <- reactive({pdftools::pdf_convert("0.pdf",
                                                  dpi = 300)})
      text1 <- reactive({tesseract::ocr(pngfile1())})
      df1 = reactive({as_tibble(text1())})
      r1 = reactive({df1() %>%
          unnest_tokens(word, value, to_lower = F)})
      ########################################################################
      
      # document type
      updateSelectInput(session, "type", selected = 
                          if((which(grepl("\\<LRA\\>", r1()[["word"]]))[1] +1 ==
                              which(grepl("\\<Spec\\>", r1()[["word"]]))[1]) &
                             (which(grepl("\\<Spec\\>", r1()[["word"]]))[1] +1 ==
                              which(grepl("\\<ID\\>", r1()[["word"]]))[1])%in% TRUE)
                          {"Label Specification Approval Form (ie variable text approval)"}
                        else if ((which(grepl("\\<P\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<L\\>", r1()[["word"]]))[1]) &
                                 (which(grepl("\\<L\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<Plan\\>", r1()[["word"]]))[1])%in% TRUE){
                          "Pack and Label Plan"}
      )
      
      
      # trial number
      output$trial_number = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          textInput("trial_number", label = "Trial Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
        } else if (input$type=="Pack and Label Plan"){
          textInput("trial_number", label = "Trial Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
        }
      })
      
      # spec id
      output$spec_id = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          textInput("spec_id", label = "LRA Spec ID",
                    value = 
                      paste(as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))+2,]),
                            "-",
                            as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))+3,]),
                            sep=""
                      )
          )
        } else{""}
      })
      
      # lot number
      output$lot_number = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          textInput("lot_number", label = "PMD Orderable Lot Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))[1]+2,])
          )
        } else if (input$type=="Pack and Label Plan"){
          textInput("lot_number", label = "PMD Orderable Lot Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))[1]+1,])
          )
        }
      })
      
      # date
      output$date = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          textInput("date", label = "Date",
                    value = 
                      paste(as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+1,]),
                            "-",
                            as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+2,]),
                            "-",
                            as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+3,]),
                            sep=""
                      )
          )
        } else if(input$type=="Pack and Label Plan"){
          textInput("date", label = "Date",
                    value = 
                      paste(
                        as.character(r1()[last(which(grepl("Approved", r1()[["word"]])))+4,]),
                        as.character(r1()[last(which(grepl("Approved", r1()[["word"]])))+5,]),
                        as.character(r1()[last(which(grepl("Approved", r1()[["word"]])))+6,]),
                        sep = "-"
                      )
          )
        }
      })
      
      # vial or kit
      output$vial_kit = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                      selected = if(length(which(grepl("PRIMARY", r1()[["word"]])))==1){
                        "Vial"
                      } else{"Kit"}
          )
        } else{""}
      })
      
      # put info into df and display
      output$feedback = renderText("")
      
      df2 = reactive({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          tibble(
            pdf_name = "0.pdf",
            protocol = input$trial_number,
            report_type = input$type,
            label_spec_id = input$spec_id,
            pmd_lot_number = input$lot_number,
            date = input$date,
            vial_kit = input$vial_kit
          )
        } else if (input$type=="Pack and Label Plan"){
          tibble(
            pdf_name = "0.pdf",
            protocol = input$trial_number,
            report_type = input$type,
            label_spec_id = "",
            pmd_lot_number = input$lot_number,
            date = input$date,
            vial_kit = ""
          )
        }
      })
      
      df3 = reactive({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          mutate(df2(), pdf_name=paste(
            df2()[["protocol"]],"Label Specification Approval",
            df2()[["label_spec_id"]],df2()[["pmd_lot_number"]],
            df2()[["vial_kit"]],df2()[["date"]],
            ".pdf"
          ))
        } else if (input$type=="Pack and Label Plan"){
          mutate(df2(), pdf_name=paste(
            df2()[["protocol"]],"PL Plan", df2()[["pmd_lot_number"]],
            df2()[["date"]], ".pdf"
          ))
        }
      })
      
      # display the dataframe (will be deleted once live)
      output$test <- renderReactable({
        reactable(df3())
      })
      
      
    } else {output$end = renderText("You have reached the last PDF!")}
    
    
  })
  
  
  
  
  
  
  
  
  
})

shinyApp(ui = ui, server = server)
