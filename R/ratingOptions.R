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



    }
  )
}

