source("reader.R")

corona <- read_github()

inc <- corona %>%
  filter(date >= max(corona$date)-1) %>% 
  select(-country) %>% 
  group_by(date) %>% 
  summarise_all(sum) %>% 
  select(-date) %>% 
  apply(2, diff)