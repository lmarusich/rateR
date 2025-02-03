library(dplyr)

# orderData <- function(isRandom, data){
#   if(isRandom){
#     nLines <- dim(data)[1]
#     return(sample(1:nLines, size = nLines, replace = F))
#   } else {
#     return(NULL)
#   }
# }

addColumns <- function(data, newcolName, ratedColumn, ratings, notes, ratingType, raterID){
                       # , randOrder, newOrder, seed){
  
  ratings <- map(ratings, ~ifelse(is.null(.x), NA, .x)) %>%
    unlist()
  notes <- map(notes, ~ifelse(is.null(.x), NA, .x)) %>%
    unlist()
  
  newcolName <- paste0(raterID, "_", newcolName)
  
  notecolName <- paste0(newcolName, "_notes")
  
  nLines <- dim(data)[1]
  # neworder <- 1:nLines
  
  # set.seed(seed)
  # isolate({
  # if(randOrder){
  #   # browser()
  #   # neworder <- newOrder
  #   # neworder <- nLines:1
  #   # (neworder <- isolate(sample(1:nLines, size = nLines, replace = F)))
  # } else {
  #   # neworder <- 1:nLines
  # }
  # })
  
  newdata <- data %>% 
    # mutate(rowNum = row_number(), .before = 1) %>%
    mutate(!!newcolName := NA,
           !!notecolName := "",
           # ratingOrder = neworder,
           .after = ratedColumn)
  # %>%
    # arrange(neworder)
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