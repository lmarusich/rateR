library(dplyr)

addColumns <- function(data, newcolName, ratedColumn, ratings){
  
  ratings <- unlist(ratings)
  
  nLines <- dim(data)[1]
  newdata <- data %>% 
    mutate(rowNum = row_number(), .before = 1) %>%
    mutate(!!newcolName := NA,
           notes = "",
           .after = ratedColumn)
  
           
  return(newdata)
}