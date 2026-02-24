#' A shiny application for human coding and rating tasks.
#'
#' rateR provides an intuitive interface with options to import and filter data, choose variables to code/rate, set constraints on the rating options, and save the rated data to a new file.
#' @import shiny
#' @export
#' @seealso Web version: \url{https://lmarusich.shinyapps.io/rateR/}
#' @examples
#' if (interactive()) {
#'   rateR::rateR()
#' }
rateR <- function() {
  app <- system.file("shiny",
                     package = 'rateR')
  shiny::runApp(app)
  
}