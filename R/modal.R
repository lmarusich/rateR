# modal <- function(id, data, itemCol, selectedRow, ratingName, ratingType, ratingSpecified){
modal <- function(id, data, selectedRow, ratingSpecs){
  
  moduleServer(
    id,
    
    function(input, output, session) {
      # ns <- session$ns
      ns <- NS(id)
      browser()
      
      myModal <- function(colname, rownum, ratingName, failed = FALSE, failMsg = "") {
        # myModal <- function(){
        # ns <- NS(id)
        # browser()
        modalDialog(
          title = "Item Rating",
          sidebarLayout(
            sidebarPanel(
              h4(paste0(colname,":")),
              p(data[[colname]][rownum]),
              width = 6
            ),
            
            mainPanel(

              div(tags$b(ifelse(failed, failMsg, ""), style = "color: red")), #customize this based on the rating type
              textInput(ns("inputRating"), 
                        label = paste0(ratingName, " Rating:")), 
              width = 6
            )
          ),
          footer = tagList(
            actionButton(ns("prev_button"), "Previous"),
            actionButton(ns("next_button"), "Next"),
            modalButton("Save and Close")
          )
        )
      }
      # Show modal dialog on start up
      rownum <- reactiveVal()
      lastrow <- dim(data)[1]
      rownum(selectedRow())
      cat(file=stderr(), rownum(), "\n")
      # browser()
      showModal(myModal(ratingSpecs$selectedColumn,rownum(), ratingSpecs$ratingName))
      # browser()
      
      observeEvent(input$next_button, {
        validInput <- T
        # removeModal()
        #enforcing the rating/label type
        if (ratingSpecs$ratingType == "labels"){
          #check that the label is in the list
        } else if (ratingSpecs$ratingType == "numbers"){
          
          #first check that it's a number - if not, throw an error
          if (is.na(as.numeric(input$inputRating))){
            validInput = F
            showModal(myModal(ratingSpecs$selectedColumn,rownum(), ratingSpecs$ratingName, failed = TRUE, failMsg = "Please enter a numeric rating"))
          
          #then check if min/max was specified, and if so, if their rating is in that range
          } else if (ratingSpecs$specified){
            if ((input$inputRating < ratingSpecs$minNumRating) | 
                (input$inputRating > ratingSpecs$maxNumRating)){
              #throw an error
              validInput = F
              showModal(myModal(ratingSpecs$selectedColumn,rownum(), 
                                ratingSpecs$ratingName, 
                                failed = TRUE, 
                                failMsg = paste0("Please enter a numeric rating between ", 
                                                 ratingSpecs$minNumRating,
                                                 " and ",
                                                 ratingSpecs$maxNumRating)))
            }
          } 
          
          if (validInput){
            #go to next row
            rownum(rownum() + 1)
            # browser()
            if (rownum() <= lastrow){
              # cat(file=stderr(), rownum, "\n")
              showModal(myModal(ratingSpecs$selectedColumn,rownum(), ratingSpecs$ratingName))
            }

        } 
        }
        
      })
      
      
    }
  )
}

