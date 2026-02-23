#' A shiny application for 
#'
#' rateR provides an intuitive interface with options to 
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