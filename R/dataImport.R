library("tidyr") #do we still need this one here?
library("readxl")
# For the code (already loaded from server.R)
# library("glue")

dataImport <- function(id){
  moduleServer(
    id,
    
    function(input, output, session) {
      
      ns <- NS(id)
      
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
        
        if (input$excludeEmptyCols){
          filedat <- filedat %>%
            select_if(function(x) { sum(!is.na(x)) > 0 })
        }
        
        imported(TRUE)
        return(filedat)
        # cbind(rowNumber = 1:dim(filedat)[1], filedat)
      })
      
      columns <- reactive({
        colnames(inputData())
      })
      
      name <- reactive({
        userFile()$name
      })
      
      # imported <- reactive({
      #   browser()
      # })
      
      
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
      
      
      # browser()
      return(list(
        inputData = inputData,
        columns = columns,
        name = name,
        # code = code,
        datapath = datapath,
        imported = imported
      ))
    }
  )
}

