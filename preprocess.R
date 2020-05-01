library(reshape2)
library(dplyr)

peek <- function(data){
  rbind(head(data,2), 
        tail(data,2))
}

melt_all <- function(data){
  data[,-1] %>% melt(
    id="country")
}

melt_date <- function(data){
  data %>% melt(
    id.vars=c("date", "country"),
    measured.vars=c("confirmed", "recovered", "dead"))
}