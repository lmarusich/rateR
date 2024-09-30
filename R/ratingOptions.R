ratingOptions <- function(id, data){
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
      
      
      output$generatedUI1 <- renderUI({
        selectInput(ns('selectedColumn'),
                    label = 'Select the column containing items to be labeled/rated:',
                    choices = c("",data$columns()),
                    selected = NULL,
                    multiple = FALSE)
        
        
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

