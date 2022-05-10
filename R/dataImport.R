# For gather
library("tidyr")
# For the code (already loaded from server.R)
# library("glue")

dataImport <- function(id){
  moduleServer(
    id,
    
    
    
    function(input, output, session) {
      
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        # validate(need(input$excelFile, message = FALSE))
        # return(input$excelFile)
        if (!is.null(input$textFile)){
          return(input$textFile)
        } 
        
        # else {
        #   if(input$sampleData) {
        #     if (input$whichsampleData == 'bland1995'){
        #       return(list('name' = 'bland1995.csv',
        #                   'datapath' = 'bland1995.csv'))
        #     } else if (input$whichsampleData == 'gilden2010'){
        #       return(list('name' = 'gilden2010.csv',
        #                   'datapath' = 'gilden2010.csv'))
        #     } else if (input$whichsampleData == 'marusich2016'){
        #       return(list('name' = 'marusich2016_exp2.csv',
        #                   'datapath' = 'marusich2016_exp2.csv'))
        #     } else if (input$whichsampleData == 'raz2005'){
        #       return(list('name' = 'raz2005.csv',
        #                   'datapath' = 'raz2005.csv'))
        #     }
        #   }
        #   req(input$excelFile)
        # }
      })
      datapath <- reactive({
        userFile()$datapath
      })
      
      inputData <- reactive({
        filedat <- read.delim2(
          userFile()$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote,
          check.names = TRUE,
          dec = input$decimalPoint
        )
        cbind(rowNumber = 1:dim(filedat)[1], filedat)
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
      ))
    }
  )
}

