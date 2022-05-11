library(dplyr)

addColumns <- function(data){
  
  nLines <- dim(data)[1]
  newdata <- data %>% 
    mutate(rowNum = row_number(), .before = 1) %>%
    mutate(ratings = NA,
           notes = "")
  
           
  
  return(newdata)
}