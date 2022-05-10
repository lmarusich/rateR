# For gather
library("tidyr")
# For the code (already loaded from server.R)
# library("glue")

ratingOptions <- function(id, data){
  moduleServer(
    id,



    function(input, output, session) {


      # UI - Data - Filter the data.
      output$generatedUI <- renderUI({
        # req(data$name()) #conditions are the column names
        tagList(
          selectInput('selectedColumn',
                      label = 'Select the column containing items to be rated:',
                      choices = c("",data$columns()),
                      selected = NULL,
                      multiple = FALSE),
          selectInput('ratingType',
                      label = 'Type of ratings:',
                      choices = c("","labels","numbers"),
                      selected = NULL,
                      multiple = FALSE),
          checkboxInput('specifyRatings',
                        label = "Specify what rating values will be allowed?"),
          conditionalPanel(condition = 'input.ratingType == "numbers" & input.specifyRatings',
                           numericInput('minNumRating',
                                        label = "Minimum rating:",
                                        value = 0),
                           numericInput('maxNumRating',
                                        label = "Maximum rating:",
                                        value = 0)
                           
          ),
          conditionalPanel(condition = 'input.ratingType == "labels" & input.specifyRatings',
                           textAreaInput('ratingLabels',
                                     label = 'Enter acceptable labels, with each label on its own line',
                                     placeholder = 'Label1\nLabel2\nLabel3\n...')

        ))
      })
      # return(generatedUI)

      # # The selected file, if any
      # userFile <- reactive({
      #   # If no file is selected, don't do anything
      #   # validate(need(input$excelFile, message = FALSE))
      #   # return(input$excelFile)
      #   if (!is.null(input$textFile)){
      #     return(input$textFile)
      #   }
      #
      # })
      # datapath <- reactive({
      #   userFile()$datapath
      # })
      #
      # inputData <- reactive({
      #   filedat <- read.delim2(
      #     userFile()$datapath,
      #     # header = input$header,
      #     sep = ",",
      #     # quote = input$quote,
      #     check.names = TRUE,
      #     # dec = input$decimalPoint
      #   )
      #   cbind(rowNumber = 1:dim(filedat)[1], filedat)
      # })
      #
      #
      #
      # conditions <- reactive({
      #   colnames(inputData())
      # })
      #
      # name <- reactive({
      #   userFile()$name
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


      #
      # return(list(
      #   inputData = inputData,
      #   conditions = conditions,
      #   name = name,
      #   # code = code,
      #   datapath = datapath
      # ))
    }
  )
}

