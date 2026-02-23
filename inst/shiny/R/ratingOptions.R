ratingOptions <- function(id, data, confirmReset){
  moduleServer(
    id,
    
    function(input, output, session) {
      # browser()
      
      ns <- NS(id)
      
      # The selected file, if any
      selectedCol <- reactive({
        req(input$selectedColumn)
        
        if (!is.null(input$selectedColumn)){
          return(input$selectedColumn)
        } else {
          return(NULL)
        }
      })
      
      
      output$raterIDUI <- renderUI({
        textInput('raterID',
                  label = "Rater name/ID:",
                  value = "",
                  placeholder = "Rater name/ID")
      })
      
      output$selectColumnUI <- renderUI({
        selectInput(ns('selectedColumn'),
                    label = 'Select the column containing items to be labeled/rated:',
                    choices = c("",data$columns()),
                    selected = NULL,
                    multiple = FALSE)
      })
      
      output$nameColumnUI <- renderUI({
        textInput('ratingName',
                  label = 'Name the new column of labels/ratings:',
                  value = "",
                  placeholder = "new column name")
      })
      
      output$columnTypeUI <- renderUI({
        tagList(
          selectInput('ratingType',
                      label = 'Type of ratings:',
                      choices = c("","labels","numbers"),
                      selected = NULL,
                      multiple = FALSE),
          checkboxInput('specifyRatings',
                        label = "Specify what rating values will be allowed?")
        )
      })
      
      observeEvent(confirmReset(), {
        output$raterIDUI <- renderUI({
          textInput('raterID',
                    label = "Rater name/ID:",
                    value = "",
                    placeholder = "Rater name/ID")
        })
        output$selectColumnUI <- renderUI({
          selectInput(ns('selectedColumn'),
                      label = 'Select the column containing items to be labeled/rated:',
                      choices = c("",data$columns()),
                      selected = NULL,
                      multiple = FALSE)
        })
        output$nameColumnUI <- renderUI({
          textInput('ratingName',
                    label = 'Name the new column of labels/ratings:',
                    value = "",
                    placeholder = "new column name")
        })
        
        output$columnTypeUI <- renderUI({
          tagList(
            selectInput('ratingType',
                        label = 'Type of ratings:',
                        choices = c("","labels","numbers"),
                        selected = NULL,
                        multiple = FALSE),
            checkboxInput('specifyRatings',
                          label = "Specify what rating values will be allowed?")
          )
        })
        
      })
      
      # output$generatedUI2 <- renderUI({
      #   if (is.null(selectedCol())) {
      #     return(NULL)
      #   } else {
      #     checkboxInput('skipEmptyCells',
      #                   label = HTML(paste0("Skip empty cells from <b>", selectedCol(), "?</b>")))
      #   }
      
      # })
      
      
    }
  )
}
  
  
  