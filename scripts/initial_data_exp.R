library(tidyverse)
library(readxl)
library(tidycensus)
library(tigris)

hud_data_2019 <- read_xlsx("data/public-housing-physical-inspection-scores-2019.xlsx")


## we have city, county, ZIP, inspection score, inspection date (2013-2019)
## lat and long
## location quality 

hud_data_2019 %>%
  ggplot()+
  geom_histogram(aes(x = INSPECTION_SCORE), alpha = 0.7, color = "black")+
  labs(x= "Inspection Score", y = "Count")+
  theme_bw()


#let's map this with flood hazard layer 
## distance to nearest flood plane ###
## block group 

#########################################################################################3
## pull in 2021 data 
hud_data <- read_xlsx("data/public_housing_physical_inspection_scores_0321.xlsx")


## we have city, county, ZIP, inspection score, inspection date (2013-2019)
## lat and long
## location quality 

hud_data %>%
  ggplot()+
  geom_histogram(aes(x = INSPECTION_SCORE), alpha = 0.7, fill = "blue")+
  geom_histogram(data = hud_data_2019, aes(x = INSPECTION_SCORE), alpha = 0.7, color = "red")+
  labs(x= "Inspection Score", y = "Count")+
  theme_bw()



# scores ~ distance to flood plane + socioeconomic data 
  #possible binary of in flood plane or not
  #disaster events (amount of damage or number PDD for example)
# weigh scores by flood distance 


hud_w_flood_raw <- read_csv("data/2pub_housing.csv") %>%
  select(c(DEVELOPMEN, NEAR_DIST))


hud_w_flood <- hud_data %>% left_join(hud_w_flood_raw, 
                                      by= c("DEVELOPMENT_ID" = "DEVELOPMEN")) 


hud_w_flood <- hud_w_flood %>%
  mutate(in_floodplain = ifelse(NEAR_DIST == 0, 1, 0))


############# try some regressions ######################################

lm_simple <- lm(INSPECTION_SCORE ~ NEAR_DIST, data = hud_w_flood)
summary(lm_simple)

## previous disasters in county 
## block group sociodemographic information 
## look at Meri and Qing's public housing for socioeconomic characteristics
## income brackets, people in poverty, disability, race
## control for number of housing units -- from HUD 
############# pull in socioeconomic data ###############################


##1 spatially join to county and block group ### 

hud_w_flood <- hud_w_flood %>%
  mutate(county_FIPS = str_c(STATE_CODE, COUNTY_CODE))


county_hazard_data <- read_csv("data/county_hazard variables.csv") %>%
  select(c(fips, hurrexpo)) %>% 
  unique()
county_hazard_data$fips <- str_pad(county_hazard_data$fips, 5, pad="0")


hud_w_flood <- hud_w_flood %>%
  left_join(county_hazard_data, by = c("county_FIPS" = "fips")) %>%
  filter(STATE_NAME != "PR" & STATE_NAME != "VI" & STATE_NAME != "GU")


hud_w_flood$census_code <- apply(hud_w_flood, 1, 
                                 function(row) 
                                   call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))

jkhuyihud_w_flood$census_code <- as.character(hud_w_flood$census_code)
hud_w_flood <- hud_w_flood %>%
  mutate(block_group = substr(census_code, 1, 12))


##################### read census data for those block groups ############# 


#use ACS to pull in race, age
census_api_key("ff5d487d0a2a22c658bf319ba136c27db32aa0be", install = TRUE, 
               overwrite = TRUE)
v20 <- load_variables(2020, "acs5", cache = TRUE)
View(v20)

other_census_blockgroup <- get_acs(survey = "acs5", geography = "block group", 
                                   variables = c(age = "B01002_001",
                                                 income =  "B19013_001",
                                                 prop_value = "B25077_001",
                                                 public_assistance = "B19057_001",  
                                                 poverty = "B17010_001", 
                                                 vehicles = "B25044_001",
                                                 renter = "B25003_003",
                                                 owner = "B25003_002",
                                                 total_white = "B03002_003",
                                                 total_black = "B03002_004",
                                                 total_hispanic = "B03002_012",
                                                 total_pop = "B01003_001",
                                                 disability = "B18101_001"), 
                                   state = c(unique(hud_w_flood$STATE_CODE)),
                                   year = 2020)



other_census_wider <- other_census_blockgroup%>% pivot_wider(id_cols = "GEOID",
                                                             names_from = "variable",
                                                             values_from = "estimate")

hud_w_block <- hud_w_flood %>% left_join(other_census_wider, 
                                          by = c("block_group" = "GEOID")) 


hud_w_block <- hud_w_block %>%
  mutate(perc_white = (total_white/total_pop)*100,
         perc_renters = (renter/total_pop)*100,
         perc_poverty = (poverty/total_pop)*100,
         perc_vehicles = (vehicles/total_pop)*100,
         perc_disability = (disability/total_pop)*100)

################## should be able to run some regressions now n###########

lm_1 <- lm(INSPECTION_SCORE ~ NEAR_DIST + age + income + perc_white + 
             perc_poverty + perc_vehicles + perc_renters + hurrexpo,
  data = hud_w_block)
summary(lm_1)






########## creating panel data ################################

hud_panel <- read_xlsx("data/PublicHousingScores.xlsx") 
locations <- hud_panel %>%
  select(c(3, 6:15)) %>% unique()


locations_half1<- locations[1:200,]

locations_half1$census_code <- apply(locations_half1, 1, 
                                 function(row) 
                                   call_geolocator_latlon(row['latitude'], row['longitude']))







hud_w_flood <- hud_w_flood %>%
  left_join(county_hazard_data, by = c("county_FIPS" = "fips")) %>%
  filter(STATE_NAME != "PR" & STATE_NAME != "VI" & STATE_NAME != "GU")




hud_w_flood$census_code <- as.character(hud_w_flood$census_code)
hud_w_flood <- hud_w_flood %>%
  mutate(block_group = substr(census_code, 1, 12))


##################### read census data for those block groups ############# 


#use ACS to pull in race, age
census_api_key("ff5d487d0a2a22c658bf319ba136c27db32aa0be", install = TRUE, 
               overwrite = TRUE)
v20 <- load_variables(2020, "acs5", cache = TRUE)
View(v20)

other_census_blockgroup <- get_acs(survey = "acs5", geography = "block group", 
                                   variables = c(age = "B01002_001",
                                                 income =  "B19013_001",
                                                 prop_value = "B25077_001",
                                                 public_assistance = "B19057_001",  
                                                 poverty = "B17010_001", 
                                                 vehicles = "B25044_001",
                                                 renter = "B25003_003",
                                                 owner = "B25003_002",
                                                 total_white = "B03002_003",
                                                 total_black = "B03002_004",
                                                 total_hispanic = "B03002_012",
                                                 total_pop = "B01003_001",
                                                 disability = "B18101_001"), 
                                   state = c(unique(hud_w_flood$STATE_CODE)),
                                   year = 2020)



other_census_wider <- other_census_blockgroup%>% pivot_wider(id_cols = "GEOID",
                                                             names_from = "variable",
                                                             values_from = "estimate")

hud_w_block <- hud_w_flood %>% left_join(other_census_wider, 
                                         by = c("block_group" = "GEOID")) 


hud_w_block <- hud_w_block %>%
  mutate(perc_white = (total_white/total_pop)*100,
         perc_renters = (renter/total_pop)*100,
         perc_poverty = (poverty/total_pop)*100,
         perc_vehicles = (vehicles/total_pop)*100,
         perc_disability = (disability/total_pop)*100)


