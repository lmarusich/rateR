library("tidyr") #do we still need this one here?
library("readxl")
library("tools")

dataImport <- function(id, confirmReset){
  moduleServer(
    id,
    
    function(input, output, session) {
      
      ns <- NS(id)
      
      output$file1_ui <- renderUI({
        # input$confirmReset
        fileInput(ns("textFile"),
                label = "File Input",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xls",
                           ".xlsx"),
                buttonLabel = "Browse...", #Unnecessary line, it's the default
                placeholder = "No file selected")
  
      })
      # browser()
      observeEvent(confirmReset(), {

        output$file1_ui <- renderUI({
          fileInput(ns("textFile"),
                    label = "File Input",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv",
                               ".xls",
                               ".xlsx"),
                    buttonLabel = "Browse...", #Unnecessary line, it's the default
                    placeholder = "No file selected")
          
        })
        
        # browser()
        # reset("textFile")
        
      })
      
      selected_sheet <- reactiveVal(NULL)
      imported <- reactiveVal(FALSE)
      
      #update UI to let user pick a sheet
      output$sheets <- renderUI({
        if (is.null(userFile())) {
          return(NULL)
        }
        if (!(grepl("sheet", userFile()$type))){
          return(NULL)
        } else {
          
          selectInput(
            ns("sheets"), "Choose a sheet:",
            choices = c("",sheet_list()),
            selected = NULL
          )
        }
        
      })
      
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        # validate(need(input$excelFile, message = FALSE))
        # return(input$excelFile)
        if (!is.null(input$textFile)){
          return(input$textFile)
        } 
      })
      
      datapath <- reactive({
        userFile()$datapath
      })
      
      sheet_list <- reactive({
        #   validate(need(input$textFile, message = FALSE))
        if (grepl("sheet", userFile()$type)){
          excel_sheets(userFile()$datapath)
        }else{
          NULL
        }
      })
      
      inputData <- reactive({
        
        if (grepl("sheet", userFile()$type)){
          selected_sheet(input$sheets)
          req(selected_sheet() %in% sheet_list())
          # req(selected_sheet())
          filedat <- read_xlsx(
            userFile()$datapath,
            sheet = selected_sheet()
          )
          # browser()
          
        } else {
          # browser()
          filedat <- read.delim2(
            userFile()$datapath,
            header = input$header,
            sep = input$sep,
            quote = input$quote,
            check.names = TRUE,
            dec = input$decimalPoint
          )

        }
        
        #NOTE, this needs to change probably. do the exclusion in the displaying of the data instead
        # if (input$excludeEmptyCols){
        #   filedat <- filedat %>%
        #     select_if(function(x) { sum(!is.na(x)) > 0 })
        # }
        
        #add row numbers here, so we can merge the filtered dataset back with the full one later
        filedat <- filedat %>%
          mutate(rowNum = row_number(), .before = 1)
        
        imported(TRUE)
        return(filedat)
        
      })
      
      columns <- reactive({
        colnames(inputData())
      })
      
      name <- reactive({
        userFile()$name
      })
      
      name_no_ext <- reactive({
        file_path_sans_ext(userFile()$name)
      })
      

      return(list(
        inputData = inputData,
        columns = columns,
        name = name,
        name_no_ext = name_no_ext,
        # code = code,
        datapath = datapath,
        imported = imported
      ))
    }
  )
}

