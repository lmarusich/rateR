
modal <- function(input, data, selectedRow, ratingSpecs){
  # browser()
  df <- data.frame(row = numeric(nrow(data)),
                   rating = numeric(nrow(data)))
  
  myModal <- function(colname, rownum, ratingName, failed = FALSE, failMsg = "") {
    
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
          textInput(("inputRating"), 
                    label = paste0(ratingName, " Rating:")), 
          width = 6
        )
      ),
      footer = tagList(
        actionButton(("prev_button"), "Previous"),
        actionButton(("next_button"), "Next"),
        actionButton(("close_button"), "Save and Close")
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
  event_trigger <- reactive({
    list(input$next_button, input$close_button)
  })
  observeEvent(ignoreInit = T, event_trigger(), {
    if(input$next_button==0 && input$close_button==0){
      return()
    }
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
        # return(input$inputRating)
        #save the rating
        # browser()
        df$row[rownum()] <- rownum()
        df$rating[rownum()] <- input$inputRating
        
        if (input$next_button){
          #go to next row
          rownum(rownum() + 1)

          if (rownum() <= lastrow){
            showModal(myModal(ratingSpecs$selectedColumn,rownum(), ratingSpecs$ratingName))
          }
        }
        
        if (input$close_button){
          removeModal()
        }
      } 
    }
  })

  outputdf <- reactive({df})
  return(outputdf)
}


