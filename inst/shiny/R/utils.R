library(dplyr)

# orderData <- function(isRandom, data){
#   if(isRandom){
#     nLines <- dim(data)[1]
#     return(sample(1:nLines, size = nLines, replace = F))
#   } else {
#     return(NULL)
#   }
# }

addColumns <- function(data, 
                       newcolName, 
                       ratedColumn, 
                       ratings, 
                       notes, 
                       ratedRows,
                       ratingType, 
                       raterID,
                       randOrder,
                       newOrder){
                       # , randOrder, newOrder, seed){
  
  ratings <- map(ratings, ~ifelse(is.null(.x), NA, .x)) %>%
    unlist()
  notes <- map(notes, ~ifelse(is.null(.x), NA, .x)) %>%
    unlist()
  ratedRows <- map(ratedRows, ~ifelse(is.null(.x), NA, .x)) %>%
    unlist()
  
  newcolName <- paste0(raterID, "_", newcolName)
  
  notecolName <- paste0(newcolName, "_notes")
  
  ordercolName <- paste0(newcolName, "_rating_order")
  
  nLines <- dim(data)[1]

  #if the order is not randomized, check if it's been previously randomized
  if (!(randOrder)){
    # if (all(data$rowNum == 1:nLines)) #if not, order stays in row number order
    if (!(is.unsorted(data$rowNum)))
    {
      newOrder <- data$rowNum
      # browser()
    } else { #if yes, reorder things back to row number order
      newOrder <- data$rowNum
      # browser()
    }
  } else if (randOrder){ #if the order is randomized, check if it's already been done
    # if (all(data$rowNum == 1:nLines)){ #if not, use newOrder to rearrange
    if (!(is.unsorted(data$rowNum))){
      #newOrder stays as newOrder
      # browser()
    } else{ #if so, keep it the way it is
      newOrder <- data[[ordercolName]]
    }
  }
  

  newdata <- data %>% 
    # mutate(rowNum = row_number(), .before = 1) %>%
    mutate(!!newcolName := NA,
           !!notecolName := "",
           !!ordercolName := newOrder,
           .after = ratedColumn)
 
  newdata <- isolate(newdata %>%
    arrange(get(ordercolName)))

  if(any(!is.na(ratings))){
    if (ratingType == "numbers"){
      newdata[[newcolName]][1:length(ratings)] <- as.numeric(ratings)
    } else {
      newdata[[newcolName]][1:length(ratings)] <- ratings
    }
  }
  if(any(!is.na(notes))){
    
    newdata[[notecolName]][1:length(notes)] <- notes
    
  }
  # browser()
  return(newdata)
}

myModal <- function(initialdf, colname, rownum, ratingName, existingRating = "", 
                    failed = FALSE, failMsg = "", existingNotes = "") {
  
  jscode2  <- '
$(document).keyup(function(event) {
  if ((event.keyCode == 13)) {
    if (!$("#next_button").prop("disabled")) {
        $("#next_button").click();
    }
      
  }
});
'
  
  # browser()
  
  modalDialog(
    tags$script(HTML(jscode2)),
    title = paste0("Row ", rownum),
    sidebarLayout(
      sidebarPanel(
        h4(paste0(colname,":")),
        p(initialdf$df_data[[colname]][rownum]),
        width = 6
      ),
      
      mainPanel(
        
        div(tags$b(ifelse(failed, failMsg, ""), style = "color: red")), #customize this based on the rating type
        textInput(("inputRating"), 
                  label = paste0(ratingName, " Rating:"),
                  value = existingRating), 
        textInput(("inputNotes"), 
                  label = paste0(ratingName, " Notes:"),
                  value = existingNotes),
        width = 6
      )
    )
    ,
    
    footer = tagList(
      actionButton(("prev_button"), "Previous", disabled = ifelse(rownum == 1, T, F)),
      actionButton(("next_button"), "Next", disabled = ifelse(rownum == dim(initialdf$df_data)[1], T, F)),
      actionButton(("close_button"), "Save and Close"),
      modalButton( "Cancel")
    )
    # }
  )
}