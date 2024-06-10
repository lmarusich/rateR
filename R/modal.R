modal <- function(id, data, itemCol, selectedRow, ratingType, ratingSpecified){
  moduleServer(
    id,
    
    function(input, output, session) {
      # ns <- session$ns
      ns <- NS(id)
      
      myModal <- function(colname, rownum, failed = FALSE) {
      # myModal <- function(){
        # ns <- NS(id)
        modalDialog(
          title = "Item Rating",
          sidebarLayout(
            sidebarPanel(
              h4(paste0(colname,":")),
              p(data[[colname]][rownum]),
              width = 6
            ),
            mainPanel(
              textInput(ns("inputRating"), label = "Rating:"),
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
      showModal(myModal(itemCol(),rownum()))
      # browser()
      
      observeEvent(input$next_button, {
        # removeModal()
        #do something here about enforcing the rating/label type
        if (ratingType() == "labels"){
          #check that the label is in the list
        } else if (ratingType() == "numbers"){
          #first check that it's a number
          if (is.na(as.numeric(input$inputRating))){
            #throw an error
            browser()
            showModal(myModal(itemCol(),rownum(), failed = TRUE))
            #do something when it fails???
          }
          #check that the rating is within the specified range
          # if ((input$inputRating < input$minNumRating) | 
          #     (input$inputRating > input$maxNumRating)) {
          #   
          # }
        }
        rownum(rownum() + 1)
        browser()
        if (rownum() <= lastrow){
        # cat(file=stderr(), rownum, "\n")
        showModal(myModal(itemCol(),rownum()))
        }
        
      })

      
    }
  )
}

