library(tidyverse)
library(readxl)
library(tidycensus)
library(tigris)


######### read data by individual HUD dataset #########
hud_data_2019 <- read_xlsx("data/public-housing-physical-inspection-scores-2019.xlsx") %>%
  select(-c(INSPECTION_ID, LOCATION_QUALITY))
hud_data_2019$LATITUDE <- as.double(hud_data_2019$LATITUDE)
hud_data_2019$LONGITUDE <- as.double(hud_data_2019$LONGITUDE)
hud_data_2019$INSPECTION_DATE <- as.character(hud_data_2019$INSPECTION_DATE)
hud_data_2019$INSPECTION_SCORE <- as.double(hud_data_2019$INSPECTION_SCORE)

hud_data_2021 <- read_xlsx("data/public_housing_physical_inspection_scores_0321.xlsx") %>%
  select(-c(INSPECTION_ID, LOCATION_QUALITY))
hud_data_2021$LATITUDE <- as.double(hud_data_2021$LATITUDE)
hud_data_2021$LONGITUDE <- as.double(hud_data_2021$LONGITUDE)
hud_data_2021$INSPECTION_DATE <- as.character(hud_data_2021$INSPECTION_DATE)
hud_data_2021$INSPECTION_SCORE <- as.double(hud_data_2021$INSPECTION_SCORE)

hud_data_2020 <- read_xlsx("data/public_housing_physical_inspection_scores_0620.xlsx")  %>%
  select(-c(INSPECTION_ID, LOCATION_QUALITY))
hud_data_2020$LATITUDE <- as.double(hud_data_2020$LATITUDE)
hud_data_2020$LONGITUDE <- as.double(hud_data_2020$LONGITUDE)
hud_data_2020$INSPECTION_DATE <- as.character(hud_data_2020$INSPECTION_DATE)
hud_data_2020$INSPECTION_SCORE <- as.double(hud_data_2020$INSPECTION_SCORE)

hud_data_2018 <- read_xlsx("data/public-housing-physical-inspection-scores-2018.xlsx")  %>%
  select(-c(INSPECTION_ID, LOCATION_QUALITY))
hud_data_2018$LATITUDE <- as.double(hud_data_2018$LATITUDE)
hud_data_2018$LONGITUDE <- as.double(hud_data_2018$LONGITUDE)
hud_data_2018$INSPECTION_DATE <- as.character(hud_data_2018$INSPECTION_DATE)
hud_data_2018$INSPECTION_SCORE <- as.double(hud_data_2018$INSPECTION_SCORE)

hud_data_2016 <- read_xlsx("data/public-housing-physical-inspection-scores-2016.xlsx")  %>%
  select(-c(INSPECTION_ID, LOCATION_QUALITY))
hud_data_2016$LATITUDE <- as.double(hud_data_2016$LATITUDE)
hud_data_2016$LONGITUDE <- as.double(hud_data_2016$LONGITUDE)
hud_data_2016$INSPECTION_DATE <- as.character(hud_data_2016$INSPECTION_DATE)
hud_data_2016$INSPECTION_SCORE <- as.double(hud_data_2016$INSPECTION_SCORE)

hud_data_2015 <- read_xlsx("data/public_housing_physical_inspection_scores.xlsx")  %>%
  select(-c(INSPECTION_ID, LOCATION_QUALITY))
hud_data_2015$LATITUDE <- as.double(hud_data_2015$LATITUDE)
hud_data_2015$LONGITUDE <- as.double(hud_data_2015$LONGITUDE)
hud_data_2015$INSPECTION_DATE <- as.character(hud_data_2015$INSPECTION_DATE)
hud_data_2015$INSPECTION_SCORE <- as.double(hud_data_2015$INSPECTION_SCORE)

hud_data_2011 <- read_xls("data/public_housing_physical_inspection_scores_2011.xls") 
hud_data_2011$latitude <- as.double(hud_data_2011$latitude)
hud_data_2011$longitude <- as.double(hud_data_2011$longitude)
hud_data_2011$inspection_date <- as.character(hud_data_2011$inspection_date)
hud_data_2011$inspection_score <- as.double(hud_data_2011$inspection_score)

names(hud_data_2011) <- names(hud_data_2021)
names(hud_data_2020) <- names(hud_data_2021)
names(hud_data_2019) <- names(hud_data_2021)
names(hud_data_2018) <- names(hud_data_2021)
names(hud_data_2016) <- names(hud_data_2021)
names(hud_data_2015) <- names(hud_data_2021)



#######join by row #############

joined_hud <- hud_data_2021 %>%
  bind_rows(hud_data_2020)%>%
  bind_rows(hud_data_2019)%>%
  bind_rows(hud_data_2018)%>%
  bind_rows(hud_data_2016)%>%
  bind_rows(hud_data_2015)%>%
  bind_rows(hud_data_2011)


all_locations <- joined_hud %>%
  select(c(DEVELOPMENT_ID, LATITUDE, LONGITUDE, STATE_NAME)) %>%
  unique()



  