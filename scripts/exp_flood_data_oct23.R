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


census_codes_comb$census_code <- as.character(census_codes_comb$census_code)

census_codes_comb <- census_codes_comb %>%
  mutate(block_group = substr(census_code, 1, 12),
         county = substr(census_code, 1, 5))
  
  

hud_w_flood <- hud_w_flood %>%
  left_join(census_codes_comb, by = c("LATITUDE", "LONGITUDE"))
#wrote as hud_flood_geolocations.Rds


#combine with inspection score dataset




