# For gather
library("tidyr")
library("readxl")
# For the code (already loaded from server.R)
# library("glue")

dataImport <- function(id){
  moduleServer(
    id,
    
    
    
    function(input, output, session) {
      
      ns <- NS(id)
      
      selected_sheet = reactiveVal(NULL)
      
      output$textFile<- renderUI({
        # tagList(
        # p("Upload a .txt, .csv, or .xlsx file with one item per row."),
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
        # )
      })
      
      #update UI to let user pick a sheet
      #if the file has one sheet, just use that one
      #if there are multiple sheets, make them pick
      #need to add the sheet selection to the file reading
      output$sheets <- renderUI({
        if (is.null(userFile())) {
          return(NULL)
        }
        if (!(grepl("sheet", userFile()$type))){
          return(NULL)
        } else {
          
          # nsheets <- reactive(length(excel_sheets(userFile()$datapath)))
          # 
          # if (nsheets() == 1){
          #   return(NULL)
          # } else {
          # # browser()
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
          # browser()
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
        # })
        
      })
      
      # selected_sheet <- reactiveVal({
      #   if (grepl("sheet", userFile()$type)){
      #     input$sheets
      #   }else{
      #     NULL
      #   }
      # })
      
      # observeEvent(input$textFile, {
      #   selected_sheet(NULL)
      #   browser()
      # 
      # })
      
      # observeEvent(input$textFile, {
      #   sheets_updated(FALSE)
      #   runjs("Shiny.setInputValue( 'import_ui-sheets', 'JOEL' );")
      #   browser()
      # }, priority = 100)
      # outputOptions(output, "sheets", suspendWhenHidden = FALSE, priority = 10)
      # 
      # observeEvent({
      #   input$sheets
      # }, {
      #   sheets_updated(TRUE)
      #   browser()
      # })
      # 
      # observeEvent(userFile()$name,{
      #   sheets_updated(FALSE)
      #   browser()
      # }, priority = 100)
      
      # output$sheets <- reactive({
      #   validate(need(input$textFile, message = FALSE))
      #   if (grepl("sheet", userFile()$type)){
      #     excel_sheets(userFile()$datapath)
      #   }else{
      #     NULL
      #   }
      # })
      # outputOptions(output, 'sheets', suspendWhenHidden = FALSE)
      

      inputData <- reactive({
        
        if (grepl("sheet", userFile()$type)){
          selected_sheet(input$sheets)
          req(selected_sheet() %in% sheet_list())
          browser()
          
          req(selected_sheet())
          # browser()
          filedat <- read_xlsx(
            userFile()$datapath,
            sheet = selected_sheet()
          )
          
          
        } else {
        filedat <- read.delim2(
          userFile()$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote,
          check.names = TRUE,
          dec = input$decimalPoint
        )
        }
        
        return(filedat)
        # cbind(rowNumber = 1:dim(filedat)[1], filedat)
      })
      
      
      
      columns <- reactive({
        colnames(inputData())
      })

      name <- reactive({
        userFile()$name
      })
      
      
#       code <- reactive({
#         quoteCode <- ifelse(input$quote == '\'', '"{input$quote}"', '\'{input$quote}\'')
#         sepCode <- ifelse(input$sep == '\t', '\\t', '{input$sep}')
#         glue('## Load the data
# inputData <- read.delim2("{name()}",
#                           header = {input$header},
#                           sep = \'', sepCode, '\',
#                           quote = ', quoteCode, ',
#                           check.names = TRUE,
#                           dec = \'{input$decimalPoint}\')\n\n')})
      
      
      
      return(list(
        inputData = inputData,
        columns = columns,
        name = name,
        # code = code,
        datapath = datapath
        # sheets = sheets
      ))
    }
  )
}

