
library(tidyverse)

look<- read_csv("data/locations_inspectionscores_forMeri_Feb.csv") %>%
  mutate(tract = substr(as.character(block_group), 1, 11))
  
unique_locations <- unique(look$DEVELOPMENT_ID)
unique <- unique(look[c(1, 3:8, 17:19)])

check_one <- unique %>%
  filter(DEVELOPMENT_ID == "LA130000001")


## check combined data ##

insp <- read_csv("data/locations_inspectionscores_w_tracts.csv")
tract <- read_csv("data/raw_tract_data.csv")


comb <- insp %>% left_join(tract, by = c("tract" = "GEOID", 
                                         "inspection_year" = "year"))


#going to try a spatial interpolation

#2013-2020

years <- unique(comb$inspection_year)



check <- read_csv("data/combined_tract_level_April.csv")










