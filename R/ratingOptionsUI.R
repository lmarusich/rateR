ratingOptionsUI <- function(id, label = "Rating Options") {
  
  ns <- NS(id)
  
  tagList(
    
    uiOutput(ns('raterIDUI')),
    
    conditionalPanel(
      condition = 'input.raterID',
      uiOutput(ns('selectColumnUI'))
    ),
    
    conditionalPanel(
      condition = 'input.selectedColumn',
      # checkboxInput('randomizeOrder',
      #               label = "Randomize order when rating?"),
      uiOutput(ns('nameColumnUI')),
      ns = NS(id)
      
    ),
    
    conditionalPanel(condition = 'input.ratingName',
                    uiOutput(ns('columnTypeUI'))
                     # selectInput('ratingType',
                     #             label = 'Type of ratings:',
                     #             choices = c("","labels","numbers"),
                     #             selected = NULL,
                     #             multiple = FALSE),
                     # checkboxInput('specifyRatings',
                     #               label = "Specify what rating values will be allowed?")
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
