library(dplyr)

addColumns <- function(data, newcolName, ratedColumn, ratings, ratingType){
  
  ratings <- map(ratings, ~ifelse(is.null(.x), NA, .x)) %>%
    unlist()
  
  nLines <- dim(data)[1]
  newdata <- data %>% 
    mutate(rowNum = row_number(), .before = 1) %>%
    mutate(!!newcolName := NA,
           notes = "",
           .after = ratedColumn)
  if(any(!is.na(ratings))){
    if (ratingType == "numbers"){
      newdata[[newcolName]][1:length(ratings)] <- as.numeric(ratings)
    } else {
      newdata[[newcolName]][1:length(ratings)] <- ratings
    }
  }
  
  return(newdata)
}

myModal <- function(initialdf, colname, rownum, ratingName, failed = FALSE, failMsg = "") {
  # browser()
  modalDialog(
    title = "Item Rating",
    sidebarLayout(
      sidebarPanel(
        h4(paste0(colname,":")),
        p(initialdf$df_data[[colname]][rownum]),
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