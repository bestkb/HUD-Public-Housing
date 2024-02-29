
library(tidyverse)

look<- read_csv("data/locations_inspectionscores_forMeri_Feb.csv") %>%
  mutate(tract = substr(as.character(block_group), 1, 11))
  
unique_locations <- unique(look$DEVELOPMENT_ID)
unique <- unique(look[c(1, 3:8, 17:19)])

check_one <- unique %>%
  filter(DEVELOPMENT_ID == "LA130000001")

