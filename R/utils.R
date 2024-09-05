library(dplyr)

addColumns <- function(data, newcolName, ratedColumn, ratings, notes, ratingType){
  
  ratings <- map(ratings, ~ifelse(is.null(.x), NA, .x)) %>%
    unlist()
  
  notecolName <- paste0(newcolName, "_notes")
  
  nLines <- dim(data)[1]
  newdata <- data %>% 
    mutate(rowNum = row_number(), .before = 1) %>%
    mutate(!!newcolName := NA,
           !!notecolName := "",
           .after = ratedColumn)
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

  modalDialog(
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
    ),

      footer = tagList(
        actionButton(("prev_button"), "Previous", disabled = ifelse(rownum == 1, T, F)),
        actionButton(("next_button"), "Next", disabled = ifelse(rownum == dim(initialdf$df_data)[1], T, F)),
        actionButton(("close_button"), "Save and Close")
      )
    # }
  )
}