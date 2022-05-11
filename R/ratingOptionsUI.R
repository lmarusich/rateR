ratingOptionsUI <- function(id, label = "Rating Options") {

  ns <- NS(id)

  tagList(
    uiOutput(ns('generatedUI'))
  )
}
