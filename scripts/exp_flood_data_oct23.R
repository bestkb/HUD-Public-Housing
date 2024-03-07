library(tidyverse)
library(readxl)
library(tidycensus)
library(tigris)


#each location with flood data
hud_w_flood <- read_csv("data/PublicHousing_FloodData.csv")
hud_w_flood <- hud_w_flood %>%
  mutate(in_floodplain = ifelse(FldZone_YN == "Yes", 1, 0))


#pull in census block

hud_w_flood_codes1 <- hud_w_flood[0:2000, 6:7]
hud_w_flood2 <- hud_w_flood[2001:4000, 6:7]
hud_w_flood3 <- hud_w_flood[4001:8000, 6:7]
hud_w_flood4 <- hud_w_flood[8001:12000, 6:7]
hud_w_flood5 <- hud_w_flood[12001:16000, 6:7]
hud_w_flood6 <- hud_w_flood[16001:17000, 6:7]
hud_w_flood6b1 <- hud_w_flood[17001:17158, 6:7]
hud_w_flood6b2 <- hud_w_flood[17160:18000, 6:7]
hud_w_flood6b <- hud_w_flood[18001:20000, 6:7]
hud_w_flood7 <- hud_w_flood[20001:21966, 6:7]



########## pulling geocodes ################

hud_w_flood_codes1$census_code <- apply(hud_w_flood_codes1, 1, 
                                     function(row) 
                                       call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))
hud_w_flood2$census_code <- apply(hud_w_flood2, 1, 
                                  function(row) 
                                    call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))
hud_w_flood3$census_code <- apply(hud_w_flood3, 1, 
                                  function(row) 
                                    call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))
hud_w_flood4$census_code <- apply(hud_w_flood4, 1, 
                                  function(row) 
                                    call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))
hud_w_flood5$census_code <- apply(hud_w_flood5, 1, 
                                  function(row) 
                                    call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))
hud_w_flood6$census_code <- apply(hud_w_flood6, 1, 
                                  function(row) 
                                    call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))
hud_w_flood6b1$census_code <- apply(hud_w_flood6b1, 1, 
                                  function(row) 
                                    call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))
hud_w_flood6b2$census_code <- apply(hud_w_flood6b2, 1, 
                                    function(row) 
                                      call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))

hud_w_flood7$census_code <- apply(hud_w_flood7, 1, 
                                  function(row) 
                                    call_geolocator_latlon(row['LATITUDE'], row['LONGITUDE']))



census_codes_comb <- hud_w_flood_codes1 %>%
  bind_rows(hud_w_flood2) %>%
  bind_rows(hud_w_flood3) %>%
  bind_rows(hud_w_flood4) %>%
  bind_rows(hud_w_flood5) %>%
  bind_rows(hud_w_flood6) %>%
  bind_rows(hud_w_flood6b) %>%
  bind_rows(hud_w_flood6b1) %>%
  bind_rows(hud_w_flood6b2) %>%
  bind_rows(hud_w_flood7)


####### extracting block group and county codes
census_codes_comb$census_code <- as.character(census_codes_comb$census_code)

census_codes_comb <- census_codes_comb %>%
  mutate(block_group = substr(census_code, 1, 12),
         county = substr(census_code, 1, 5))
  
  
#### combine geocodes
hud_w_flood <- hud_w_flood %>%
  left_join(census_codes_comb, by = c("LATITUDE", "LONGITUDE"))
#wrote as hud_flood_geolocations.Rds


#combine with inspection score dataset
hud_w_flood <- read_rds("data/hud_flood_geolocations.Rds")

hud_w_flood <- hud_w_flood %>%
  select(c(3, 5:8, 32:35))

inspection_scores <- read_rds("data/all_insp_scores.Rds")


inspection_scores_w_flood <- inspection_scores %>%
  left_join(hud_w_flood, by = c("DEVELOPMENT_ID" = "DEVELOPMEN", "LATITUDE", "LONGITUDE"))


#removing puerto rico and territories
inspection_scores_w_flood <- inspection_scores_w_flood %>%
  filter(STATE_CODE <= 56)

#extract year from Inpsection Date field 
inspection_scores_2011 <- inspection_scores_w_flood %>%
  filter(year == 2011) 
inspection_scores_2011$INSPECTION_DATE <- as.Date(inspection_scores_2011$INSPECTION_DATE,
                                                  format = "%m/%d/%Y")
inspection_scores_2011 <- inspection_scores_2011 %>%
  mutate(inspection_year = as.numeric(format(INSPECTION_DATE, "%Y")))

inspection_scores_2015 <- inspection_scores_w_flood %>%
  filter(year == 2015) 
inspection_scores_2015$INSPECTION_DATE <- as.Date(inspection_scores_2015$INSPECTION_DATE,
                                                  format = "%m/%d/%Y")
inspection_scores_2015 <- inspection_scores_2015 %>%
  mutate(inspection_year = as.numeric(format(INSPECTION_DATE, "%Y")))

inspection_scores_2020 <- inspection_scores_w_flood %>%
  filter(year == 2020) 
inspection_scores_2020$INSPECTION_DATE <- as.Date(inspection_scores_2020$INSPECTION_DATE,
                                                  format = "%d-%b-%y")
inspection_scores_2020 <- inspection_scores_2020 %>%
  mutate(inspection_year = as.numeric(format(INSPECTION_DATE, "%Y")))


inspection_scores_other <- inspection_scores_w_flood %>%
  filter(year != 2011 & year != 2015 & year != 2020) 
inspection_scores_other$INSPECTION_DATE <- as.Date(inspection_scores_other$INSPECTION_DATE)
inspection_scores_other <- inspection_scores_other %>%
  mutate(inspection_year = as.numeric(format(INSPECTION_DATE, "%Y")))



inspection_flood_all <- inspection_scores_other %>%
  bind_rows(inspection_scores_2011) %>%
  bind_rows(inspection_scores_2015) %>%
  bind_rows(inspection_scores_2020)


#wrote as "data/all_inspscores_datecorrect.Rds"


inspec_flood_all <- inspection_flood_all %>%
  select(c(1,2, 6:14, 16, 19, 21:25))

#write_csv(inspec_flood_all, "data/locations_inspectionscores_forMeri_Feb.csv")




######## next step is to pull in block group census data from ACS ########
#### write a function that uses inspection_year?
library(tidycensus)

census_api_key("ff5d487d0a2a22c658bf319ba136c27db32aa0be", install = TRUE, 
               overwrite = TRUE)

insp_years <- unique(na.omit(inspection_flood_all$inspection_year))
blockgroups <- unique(na.omit(inspection_flood_all$block_group))

insp_years <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

hold_data <- data.frame()

for (i in insp_years){
  
  temp <- inspection_flood_all %>%
    filter(inspection_year == i) 
  counties <- unique(na.omit(temp$county))
  
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
                                     state = c(unique(temp$STATE_CODE)), 
                                    # county = c(as.numeric(unique(na.omit(temp$county)))),
                                     year = i)
                                     
  other_census_wider <- other_census_blockgroup%>% pivot_wider(id_cols = "GEOID",
                                                               names_from = "variable",
                                                               values_from = "estimate")
  other_census_wider <- other_census_wider %>%
    mutate(year = i)
  
  hold_data <- hold_data %>% rbind(other_census_wider)
  
}

hold_data$GEOID <- as.character(hold_data$GEOID)

join_block <- inspection_flood_all %>%
  left_join(hold_data, by = c("block_group" = "GEOID" , "inspection_year" = "year"))

join_block <- join_block %>%
  select(-c(disability))
#write_rds(join_block, "data/insp_w_census.Rds")

join_block_2013_on <- join_block %>%
  filter(inspection_year >= 2013) %>%
  select(-c(year))

join_block_2013_clean <- join_block_2013_on %>%
  select(c(1, 6:18, 20:36)) %>%
  unique()

#write_rds(join_block_2013_clean, "data/all_data_clean_2013on.Rds")



############# still have repeats ################# 
############# other approach ##########################3


## explore repeats ##

just_location <- join_block_2013_clean %>%
  select(DEVELOPMENT_ID, LATITUDE, LONGITUDE, FloodDistM) %>%
  na.omit() %>%
  group_by(DEVELOPMENT_ID) %>%
  summarise(latitude = mean(LATITUDE),
            longitude = mean(LONGITUDE),
            distance_to_floodzone = mean(FloodDistM))



## geocode these lats and longs for census data #########

look_latlon <- just_location[,2:3]

look_latlon$census_code <- apply(look_latlon, 1, function(row) 
      call_geolocator_latlon(row['latitude'], row['longitude']))

#6,944 locations in this set 

just_location <- just_location %>%
  left_join(look_latlon, by = c("latitude", "longitude"))

just_location <- just_location %>%
  mutate(block_group = substr(census_code, 1, 12),
         county = substr(census_code, 1, 5)) %>% unique()
#still 6,944


inspection_smaller <- inspection_flood_all %>% 
  select(1, 9:11, 16:17, 21, 25) %>% unique()


no_conf <- join_block_2013_clean %>%
  select(c(1:7, 13, 15, 17:31)) %>%
  unique()


inspection_smaller <- inspection_smaller %>%
  left_join(just_location, by = "DEVELOPMENT_ID")


join_block <- inspection_smaller %>%
  left_join(hold_data, by = c("block_group" = "GEOID" , "inspection_year" = "year"))

join_block <- join_block %>%
  select(-c(disability))
#write_rds(join_block, "data/insp_w_census.Rds")

## this now has 30,082 observations

insp_w_census <- read_rds("data/insp_w_census.Rds") %>%
  filter(!is.na(INSPECTION_DATE)) %>%
  select(c(1:14))

#write_csv(insp_w_census, "data/locations_inspectionscores_forMeri_Nov.csv")


### pull county data ### 
hold_data_c <- data.frame()

for (i in insp_years){
  
  temp <- inspection_flood_all %>%
    filter(inspection_year == i) 
  counties <- unique(na.omit(temp$county))
  
  other_census_blockgroup <- get_acs(survey = "acs5", geography = "county", 
                                     variables = c(age_c = "B01002_001",
                                                   income_c =  "B19013_001",
                                                   prop_value_c = "B25077_001",
                                                   public_assistance_c = "B19057_001",  
                                                   poverty_c = "B17010_001", 
                                                   vehicles_c = "B25044_001",
                                                   renter_c = "B25003_003",
                                                   owner_c = "B25003_002",
                                                   total_white_c = "B03002_003",
                                                   total_black_c = "B03002_004",
                                                   total_hispanic_c = "B03002_012",
                                                   total_pop_c = "B01003_001",
                                                   disability_c = "B18101_001"), 
                                     county = c(as.numeric(unique(na.omit(temp$county)))),
                                     year = i)
  
  other_census_wider <- other_census_blockgroup%>% pivot_wider(id_cols = "GEOID",
                                                               names_from = "variable",
                                                               values_from = "estimate")
  other_census_wider <- other_census_wider %>%
    mutate(year = i)
  
  hold_data_c <- hold_data_c %>% rbind(other_census_wider)
  
}



join_block <- join_block %>%
  left_join(hold_data_c, by = c("county" = "GEOID" , "inspection_year" = "year"))


