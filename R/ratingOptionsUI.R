ratingOptionsUI <- function(id, label = "Rating Options") {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('generatedUI1')),
    
    # uiOutput(ns('generatedUI2')),

    conditionalPanel(condition = 'input.selectedColumn',
                     textInput('ratingName',
                               label = 'Name the new column of labels/ratings:',
                               value = "",
                               placeholder = "new column name"),
                     ns = NS(id)
    ),
    
    conditionalPanel(condition = 'input.ratingName',
                     selectInput('ratingType',
                                 label = 'Type of ratings:',
                                 choices = c("","labels","numbers"),
                                 selected = NULL,
                                 multiple = FALSE),
                     checkboxInput('specifyRatings',
                                   label = "Specify what rating values will be allowed?")
    ),
    conditionalPanel(condition = 'input.ratingType == "labels" & input.specifyRatings',
                     textAreaInput('ratingLabels',
                                   label = 'Enter acceptable labels, with each label on its own line',
                                   placeholder = 'Label1\nLabel2\nLabel3\n...')
    ),
    conditionalPanel(condition = 'input.ratingType == "numbers" & input.specifyRatings',
                     numericInput('minNumRating',
                                  label = "Minimum rating:",
                                  value = 0),
                     numericInput('maxNumRating',
                                  label = "Maximum rating:",
                                  value = 0)
                     
    )
  )
}
