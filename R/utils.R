library(dplyr)

addColumns <- function(data, newcolName, ratedColumn){
  
  nLines <- dim(data)[1]
  newdata <- data %>% 
    mutate(rowNum = row_number(), .before = 1) %>%
    mutate(!!newcolName := NA,
           notes = "",
           .after = ratedColumn)
  
           
  return(newdata)
}