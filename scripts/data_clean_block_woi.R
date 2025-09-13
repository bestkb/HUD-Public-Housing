library(tidyverse)
library(readxl)
library(tidycensus)
library(tigris)
library(dplyr)
library(gstat)
library(sp)
library(censusxy)
library(sf)

setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

# Load flood data with updated GEOIDs
hud_w_flood <- read_csv("data/PublicHousing_FloodData_updated.csv")
hud_w_flood <- hud_w_flood[hud_w_flood$STATE_NAME != "PR",]
hud_w_flood <- hud_w_flood[hud_w_flood$STATE_NAME != "VI",]
hud_w_flood <- hud_w_flood[hud_w_flood$STATE_NAME != "GU",]

hud_w_flood <- hud_w_flood %>%
  mutate(STATE = state.name[match(STATE_NAME, state.abb)])
hud_w_flood$STATE[hud_w_flood$STATE_NAME == "DC"] <- "District of Columbia"
hud_w_flood <- hud_w_flood %>%
  filter(!is.na(STATE))

cols_to_remove <- c("TRACT_2010", "TRACT_2020","BLOCK_2010", "BLOCK_2020")
hud_w_flood <- hud_w_flood %>%
  select(-all_of(cols_to_remove))

hud_w_flood <- hud_w_flood %>%
  mutate(in_floodplain = ifelse(FldZone_YN == "Yes", 1, 0))
# hud_w_flood <- hud_w_flood %>%
#   select(c("FloodDistM","DEVELOPMEN","LATITUDE","LONGITUDE","STATE_NAME","FLD_AR_ID",
#            "STUDY_TYP","FLD_ZONE","ZONE_SUBTY","SFHA_TF","STATIC_BFE","V_DATUM",
#            "DEPTH","LEN_UNIT","VELOCITY","VEL_UNIT","AR_REVERT","AR_SUBTRV","BFE_REVERT",
#            "DEP_REVERT","DUAL_ZONE","SOURCE_CIT","GFID","Shape_Leng","Shape_Area",
#            "FldZone_YN","TRACT_2010","TRACT_2020","BLOCK_2010","BLOCK_2020","in_floodplain",
#            "county_2010","county_2020"))

hud_w_flood <- unique(hud_w_flood)

# Load inspection_score data with updated GEOIDs
inspection_scores <- read_csv("data/insp_score_2010added.csv")

inspection_scores <- inspection_scores %>%
  select(c("DEVELOPMENT_ID","DEVELOPMENT_NAME","CBSA_CODE","COUNTY_NAME","COUNTY_CODE",
           "STATE_NAME.x","STATE_CODE","ZIPCODE","LATITUDE","LONGITUDE","PHA_CODE","INSPECTION_SCORE",
           "inspection_year"))
colnames(inspection_scores)[colnames(inspection_scores) == "inspection_year"] <- "year"
colnames(inspection_scores)[colnames(inspection_scores) == "STATE_NAME.x"] <- "STATE_NAME"

# remove overlaps in inspection score data
inspection_scores_uniq <- inspection_scores %>%
  distinct()

inspection_scores_uniq2 <- inspection_scores_uniq %>%
  distinct(DEVELOPMENT_ID, LATITUDE, LONGITUDE, INSPECTION_SCORE, year, .keep_all = TRUE)

print(paste("There are ",(nrow(inspection_scores)-nrow(inspection_scores_uniq))," overlaps which are now removed."))

# which(hud_w_flood$DEVELOPMEN == inspection_scores$DEVELOPMENT_ID[228])
# hud_w_flood <- hud_w_flood[-c(5244,6309,6627,14391,15561,16225), ]

inspection_scores_w_flood <- inspection_scores_uniq %>%
  left_join(hud_w_flood, by = c("DEVELOPMENT_ID" = "DEVELOPMEN", "LATITUDE", "LONGITUDE")) 

# Join flood data to inspection scores
inspection_scores_w_flood <- inspection_scores_w_flood %>%
  select(c("DEVELOPMENT_ID","DEVELOPMENT_NAME","CBSA_CODE","COUNTY_NAME",
           "COUNTY_CODE","STATE","STATE_NAME.x","STATE_CODE","ZIPCODE",
           "LATITUDE","LONGITUDE","PHA_CODE","INSPECTION_SCORE",
           "year","FloodDistM","in_floodplain"))
inspection_scores_w_flood <- inspection_scores_w_flood[rowSums(is.na(inspection_scores_w_flood)) == 0, ]
inspection_scores_w_flood <- inspection_scores_w_flood[inspection_scores_w_flood$year > 2012 &
                                                         inspection_scores_w_flood$year <=2019,]
colnames(inspection_scores_w_flood)[colnames(inspection_scores_w_flood) == "STATE_NAME.x"] <- "STATE_NAME"
inspection_scores_w_flood$year <- floor(inspection_scores_w_flood$year)
# 
# inspection_scores_w_flood_2010 <- inspection_scores_w_flood[inspection_scores_w_flood$year < 2020,]
# inspection_scores_w_flood_2020 <- inspection_scores_w_flood[inspection_scores_w_flood$year == 2020,]

# API for Woi
# census_api_key("8c8fe7ff5198b2b97811f1f66873724c02e8630b", install = TRUE, 
#                overwrite = TRUE)

# Load census data
tract_demo <- read_csv("data/tract_demographics_forMeri_Feb.csv")
tract_demo$tract <- round(as.numeric(tract_demo$tract))
tract_demo$block_group <- round(as.numeric(tract_demo$block_group))

# inspection_scores_w_flood_demo_2010 <- inspection_scores_w_flood_2010 %>%
#   left_join(tract_demo, by = c("year","tract","DEVELOPMENT_ID","block_group"))
# inspection_scores_w_flood_demo_2020 <- inspection_scores_w_flood_2020 %>%
#   left_join(tract_demo, by = c("year","TRACT_2020" = "tract","DEVELOPMENT_ID","BLOCK_2020" = "block_group"))
# inspection_scores_w_flood_demo <- bind_rows(inspection_scores_w_flood_demo_2010, inspection_scores_w_flood_demo_2020)

# Join census data to inspection score and flood data
inspection_scores_w_flood_demo <- inspection_scores_w_flood %>%
  left_join(tract_demo, by = c("year","tract","DEVELOPMENT_ID","block_group"))

inspection_scores_w_flood_demo<-inspection_scores_w_flood_demo[,-23]

target_cols <- colnames(inspection_scores_w_flood_demo)[23:34]
# inspection_scores_w_flood_demo$LATITUDE <- as.numeric(inspection_scores_w_flood_demo$LATITUDE)
# inspection_scores_w_flood_demo$LONGITUDE <- as.numeric(inspection_scores_w_flood_demo$LONGITUDE)
inspection_scores_w_flood_demo <- inspection_scores_w_flood_demo %>%
  mutate(across(all_of(target_cols), as.numeric))

print(paste("Rows with NAs : ",(sum(rowSums(is.na(inspection_scores_w_flood_demo)) > 0))))

# Try different block ID just in case
for (i in 1:nrow(inspection_scores_w_flood_demo)){
  for (col in target_cols) {
   if (is.na(inspection_scores_w_flood_demo[i,col]) & 
       floor(inspection_scores_w_flood_demo$BLOCK_2010[i] / 1000) != floor(inspection_scores_w_flood_demo$BLOCK_2020[i] / 1000)){
     block_id <- c(inspection_scores_w_flood_demo$BLOCK_2010[i] / 1000,inspection_scores_w_flood_demo$BLOCK_2020[i] / 1000)
     block_id <- floor(block_id[floor(block_id) != floor(inspection_scores_w_flood_demo$block_group[i])])
     int <- tract_demo[tract_demo$DEVELOPMENT_ID == inspection_scores_w_flood_demo$DEVELOPMENT_ID[i] &
                         tract_demo$year == inspection_scores_w_flood_demo$year[i] &
                         tract_demo$block_group == block_id,]
     inspection_scores_w_flood_demo[i, col] <- as.numeric(int[1,col])
   }
  }
}

print(paste("Rows with NAs : ",(sum(rowSums(is.na(inspection_scores_w_flood_demo)) > 0))))
# the row numbers are still 9239 meaning that using different block IDs did not really work
# save.image("before_interpolate.RData")

# # kriging
# # clean dat
# dat <- inspection_scores_w_flood_demo
# dat_list <- list()
# for (i in 1:12){
#   int <- dat[,c(9,10,13,(i+22))]
#   coordinates(int) <- ~LONGITUDE + LATITUDE  # Define spatial coordinates
#   int$time <- as.POSIXct(paste(int$year, "-01-01", sep=""))  # Convert year to time format
#   years_seq <- seq(min(int$year), max(int$year), by = 1)  # Define the time steps
#   grid <- expand.grid(
#     LONGITUDE = seq(min(int$LONGITUDE), max(int$LONGITUDE), length.out = 10),  # Adjust spatial resolution
#     LATITUDE = seq(min(int$LATITUDE), max(int$LATITUDE), length.out = 10),
#     year = years_seq
#   )
#   coordinates(grid) <- ~LONGITUDE + LATITUDE
#   grid$time <- as.POSIXct(paste(grid$year, "-01-01", sep=""))
#   
#   int_no_na <- int[!is.na(int$age_c), ]
#   vgm_model <- gstat::variogram(age_c ~ 1, int_no_na)
#   
#   # Fit a variogram model (e.g., Spherical, Exponential)
#   fit_model <- gstat::fit.variogram(vgm_model, model = vgm("Sph"))
#   
#   int_na <- int[is.na(int$age_c), ]
#   
#   kriging_result <- gstat::krige(age_c ~ 1, int[!is.na(int$age_c), ], int_na, model = fit_model)
#   
#   # Extract the predicted values (var1.pred contains the predictions)
#   predicted_values <- kriging_result$var1.pred
#   
#   
# }



# Get the mean of county
inspection_scores_w_flood_demo_noNA <- inspection_scores_w_flood_demo[rowSums(is.na(inspection_scores_w_flood_demo)) == 0,]

for (i in 1:nrow(inspection_scores_w_flood_demo)){
  for (col in target_cols) {
    if (is.na(inspection_scores_w_flood_demo[i,col])){
      current_county1 <- floor(inspection_scores_w_flood_demo$BLOCK_2010[i]/10^9)
      current_county2 <- floor(inspection_scores_w_flood_demo$BLOCK_2020[i]/10^9)
      current_year <- inspection_scores_w_flood_demo$year[i]
      
      nearby_rows <- which((floor(inspection_scores_w_flood_demo_noNA$BLOCK_2010/10^9) == current_county1 |
                            floor(inspection_scores_w_flood_demo_noNA$BLOCK_2010/10^9) == current_county2 |
                            floor(inspection_scores_w_flood_demo_noNA$BLOCK_2020/10^9) == current_county1 |
                            floor(inspection_scores_w_flood_demo_noNA$BLOCK_2020/10^9) == current_county2) & 
                               inspection_scores_w_flood_demo_noNA$year == current_year)
      nearby_values <- na.omit(inspection_scores_w_flood_demo_noNA[[col]][nearby_rows])
      if (length(nearby_values) == 0) {
        inspection_scores_w_flood_demo[i, col] <- NA
      } else {
        inspection_scores_w_flood_demo[i, col] <- mean(nearby_values)
      }

    }
  }
}

print(paste("Rows with NAs : ",(sum(rowSums(is.na(inspection_scores_w_flood_demo)) > 0))))

# Get the mean of those in radius 0.1 degree (11 km)
for (i in 1:nrow(inspection_scores_w_flood_demo)){
  for (col in target_cols) {
    if (is.na(inspection_scores_w_flood_demo[i,col])){
      current_lat <- inspection_scores_w_flood_demo$LATITUDE[i]
      current_lon <- inspection_scores_w_flood_demo$LONGITUDE[i]
      current_year <- inspection_scores_w_flood_demo$year[i]

      if (is.na(current_lat) | is.na(current_lon)) next

      distances <- sqrt((current_lat - inspection_scores_w_flood_demo_noNA$LATITUDE)^2 + (current_lon - inspection_scores_w_flood_demo_noNA$LONGITUDE)^2)
      nearby_rows <- which(distances <= 0.5 & inspection_scores_w_flood_demo$year == current_year)
      nearby_values <- na.omit(inspection_scores_w_flood_demo[[col]][nearby_rows])
      if (length(nearby_values) == 0) {
        inspection_scores_w_flood_demo[i, col] <- NA
      } else {
        inspection_scores_w_flood_demo[i, col] <- mean(nearby_values)
      }

    }
  }
}

print(paste("Rows with NAs : ",(sum(rowSums(is.na(inspection_scores_w_flood_demo)) > 0))))
# 8587



# Get the mean of neighboring county
for (i in 1:nrow(inspection_scores_w_flood_demo)){
  for (col in target_cols) {
    if (is.na(inspection_scores_w_flood_demo[i,col])){
      current_county1 <- floor(inspection_scores_w_flood_demo$BLOCK_2010[i]/10^9)
      current_county2 <- floor(inspection_scores_w_flood_demo$BLOCK_2020[i]/10^9)
      current_year <- inspection_scores_w_flood_demo$year[i]

      neigh_county1 <- counties10_neigh[[which(as.numeric(counties10_code$ctcode) == current_county1)]]
      neigh_county2 <- counties20_neigh[[which(as.numeric(counties20_code$ctcode) == current_county2)]]

      nearby_rows <- which(((floor(inspection_scores_w_flood_demo_noNA$BLOCK_2010/10^9) %in% neigh_county1) |
                             (floor(inspection_scores_w_flood_demo_noNA$BLOCK_2010/10^9) %in% neigh_county1) |
                             (floor(inspection_scores_w_flood_demo_noNA$BLOCK_2010/10^9) %in% neigh_county1) |
                             (floor(inspection_scores_w_flood_demo_noNA$BLOCK_2010/10^9) %in% neigh_county1)) &
                             (inspection_scores_w_flood_demo_noNA$year == current_year)
                           )

      nearby_values <- na.omit(inspection_scores_w_flood_demo_noNA[[col]][nearby_rows])
      if (length(nearby_values) == 0) {
        inspection_scores_w_flood_demo[i, col] <- NA
      } else {
        inspection_scores_w_flood_demo[i, col] <- mean(nearby_values)
      }

    }
  }
}

print(paste("Rows with NAs : ",(sum(rowSums(is.na(inspection_scores_w_flood_demo)) > 0))))

inspection_scores_final <- na.omit(inspection_scores_w_flood_demo)
write.csv(inspection_scores_final,"data/insp_score_flood_demo.csv")


inspection_scores_final <- read_csv("data/insp_score_flood_demo.csv")

nri <- read_csv("data/NRI_Table_CensusTracts.csv")
nri <- nri %>%
  select(c("TRACTFIPS","RISK_RATNG","SOVI_RATNG","RESL_RATNG","CFLD_RISKR","HWAV_RISKR",
           "HRCN_RISKR","RFLD_RISKR","WFIR_RISKR"))
nri$TRACTFIPS <- round(as.numeric(nri$TRACTFIPS))

nri <- nri %>%
  mutate(risk_all = ifelse(RISK_RATNG %in% c("Very High", "Relatively High"),1,0),
         soc_vul = ifelse(SOVI_RATNG %in% c("Very High", "Relatively High"),1,0),
         low_res = ifelse(RESL_RATNG %in% c("Very Low", "Relatively Low"),1,0),
         flood = ifelse(CFLD_RISKR %in% c("Very High", "Relatively High") | RFLD_RISKR %in% c("Very High", "Relatively High"), 1, 0),
         heat = ifelse(HWAV_RISKR %in% c("Very High", "Relatively High"),1,0),
         hurricane = ifelse(HRCN_RISKR %in% c("Very High", "Relatively High"),1,0),
         wildfire = ifelse(WFIR_RISKR %in% c("Very High", "Relatively High"),1,0)
  )

inspection_scores_final <- inspection_scores_final %>%
  left_join(nri, by = c("TRACT_2010" = "TRACTFIPS"))

write.csv(inspection_scores_final,"data/insp_score_flood_demo_nri.csv")

#####


for (i in 1:nrow(inspection_scores_w_flood)){
  lat <- inspection_scores_w_flood$LATITUDE[i]
  lon <- inspection_scores_w_flood$LONGITUDE[i]
  yr <- inspection_scores_w_flood$year[i]
  st <- inspection_scores_w_flood$STATE[i]
  ct <- inspection_scores_w_flood$COUNTY_NAME[i]
  
  int <- tract_dat_cleaned[tract_dat_cleaned$State == st & tract_dat_cleaned$County == ct &
                             tract_dat_cleaned$year == yr,]
  
  point <- st_as_sf(data.frame(lat = lat, lon = lon), coords = c("lon", "lat"), crs = 4269)
  
  result <- st_join(point, int, join = st_within)
  
  inspection_scores_w_flood$tract[i] <- result$GEOID
  inspection_scores_w_flood$age_b[i] <- result$age_b
  inspection_scores_w_flood$total_pop_b[i] <- result$total_pop_b
  inspection_scores_w_flood$total_white_b[i] <- result$total_white_b
  inspection_scores_w_flood$total_black_b[i] <- result$total_black_b
  inspection_scores_w_flood$total_hispanic_b[i] <- result$total_hispanic_b
  inspection_scores_w_flood$poverty_b[i] <- result$poverty_b
  inspection_scores_w_flood$income_b[i] <- result$income_b
  inspection_scores_w_flood$public_assistance_b[i] <- result$public_assistance_b
  inspection_scores_w_flood$owner_b[i] <- result$owner_b
  inspection_scores_w_flood$renter_b[i] <- result$renter_b
  inspection_scores_w_flood$vehicles_b[i] <- result$vehicles_b
  inspection_scores_w_flood$prop_value_b[i] <- result$prop_value_b
}


fail <- inspection_scores_w_flood[is.na(inspection_scores_w_flood$block_group),]
fail_indices <- which(is.na(inspection_scores_w_flood$block_group))
for (i in 1:nrow(fail)){
  lat <- fail$LATITUDE[i]
  lon <- fail$LONGITUDE[i]
  yr <- fail$year[i]
  st <- fail$STATE[i]
  ct <- fail$COUNTY_NAME[i]
  
  int <- block_dat_cleaned[block_dat_cleaned$State == st &
                             block_dat_cleaned$year == yr,]
  
  point <- st_as_sf(data.frame(lat = lat, lon = lon), coords = c("lon", "lat"), crs = 4269)
  
  result <- st_join(point, int, join = st_within)
  
  inspection_scores_w_flood$block_group[fail_indices[i]] <- result$GEOID
  inspection_scores_w_flood$age_b[fail_indices[i]] <- result$age_b
  inspection_scores_w_flood$total_pop_b[fail_indices[i]] <- result$total_pop_b
  inspection_scores_w_flood$total_white_b[fail_indices[i]] <- result$total_white_b
  inspection_scores_w_flood$total_black_b[fail_indices[i]] <- result$total_black_b
  inspection_scores_w_flood$total_hispanic_b[fail_indices[i]] <- result$total_hispanic_b
  inspection_scores_w_flood$poverty_b[fail_indices[i]] <- result$poverty_b
  inspection_scores_w_flood$income_b[fail_indices[i]] <- result$income_b
  inspection_scores_w_flood$public_assistance_b[fail_indices[i]] <- result$public_assistance_b
  inspection_scores_w_flood$owner_b[fail_indices[i]] <- result$owner_b
  inspection_scores_w_flood$renter_b[fail_indices[i]] <- result$renter_b
  inspection_scores_w_flood$vehicles_b[fail_indices[i]] <- result$vehicles_b
  inspection_scores_w_flood$prop_value_b[fail_indices[i]] <- result$prop_value_b
}

last <- inspection_scores_w_flood[is.na(inspection_scores_w_flood$block_group),]
last_indices <- which(is.na(inspection_scores_w_flood$block_group))
for (i in 1:nrow(last)){
  lat <- last$LATITUDE[i]
  lon <- last$LONGITUDE[i]
  yr <- last$year[i]
  st <- last$STATE[i]
  int <- block_dat_cleaned[block_dat_cleaned$State == st &
                             block_dat_cleaned$year == yr,]
  
  point <- st_as_sf(data.frame(lat = lat, lon = lon), coords = c("lon", "lat"), crs = 4269)
  
  closest_geom_index <- st_nearest_feature(point, int)
  closest_geometry <- int[closest_geom_index, ]
  
  distance <- st_distance(point, closest_geometry)

  inspection_scores_w_flood$block_group[last_indices[i]] <- closest_geometry$GEOID
  inspection_scores_w_flood$block_group[last_indices[i]] <- closest_geometry$GEOID
  inspection_scores_w_flood$age_b[last_indices[i]] <- closest_geometry$age_b
  inspection_scores_w_flood$total_pop_b[last_indices[i]] <- closest_geometry$total_pop_b
  inspection_scores_w_flood$total_white_b[last_indices[i]] <- closest_geometry$total_white_b
  inspection_scores_w_flood$total_black_b[last_indices[i]] <- closest_geometry$total_black_b
  inspection_scores_w_flood$total_hispanic_b[last_indices[i]] <- closest_geometry$total_hispanic_b
  inspection_scores_w_flood$poverty_b[last_indices[i]] <- closest_geometry$poverty_b
  inspection_scores_w_flood$income_b[last_indices[i]] <- closest_geometry$income_b
  inspection_scores_w_flood$public_assistance_b[last_indices[i]] <- closest_geometry$public_assistance_b
  inspection_scores_w_flood$owner_b[last_indices[i]] <- closest_geometry$owner_b
  inspection_scores_w_flood$renter_b[last_indices[i]] <- closest_geometry$renter_b
  inspection_scores_w_flood$vehicles_b[last_indices[i]] <- closest_geometry$vehicles_b
  inspection_scores_w_flood$prop_value_b[last_indices[i]] <- closest_geometry$prop_value_b
  
  # print(distance)
  
}

save(inspection_scores_w_flood,file = "data_101324.RData")

inspection_scores_w_flood2 <- inspection_scores_w_flood

find_neighbors <- function(census) {
  poly <- census$geometry
  sf_data <- st_sf(ID = c(1:nrow(census)), geometry = st_sfc(poly))
  neigh  <- st_touches(sf_data)
  return(neigh)
}

neigh = find_neighbors(block_dat_cleaned)

for (i in 1:nrow(inspection_scores_w_flood)){
  int <- inspection_scores_w_flood[i,]
  if (sum(is.na(int[,18:29])) > 0){
    bg <- int$block_group
    yr <- int$year
    block_idx <- which(block_dat_cleaned$GEOID == bg & block_dat_cleaned$year == yr)
    block_neighbor <- block_dat_cleaned[neigh[[block_idx]],]
    block_neighbor <- block_neighbor[block_neighbor$year == yr,]
    
    na_idx <- which(is.na(int))
    for (j in 1:length(na_idx)){
      inspection_scores_w_flood[i,na_idx[j]] <- mean(block_neighbor[[na_idx[j]-11]],na.rm = TRUE)
    }
  }
}

###
for (i in 1:nrow(inspection_scores_w_flood)){
  int <- inspection_scores_w_flood[i,]
  if (sum(is.na(int[,18:29])) > 0){
    bg <- int$block_group
    yr <- int$year
    block_idx <- which(block_dat_cleaned$GEOID == bg & block_dat_cleaned$year == yr)
    block_neighbor <- block_dat_cleaned[neigh[[block_idx]],]
    block_neighbor <- block_neighbor[block_neighbor$year == yr,]
    
    first_order_neigh <- block_neighbor[[1]]
    block_neighbor2_all <- block_neighbor %>% slice(0)
    for (k in 1:length(first_order_neigh)){
      bg2 <- first_order_neigh[[k]]
      block_idx2 <-which(block_dat_cleaned$GEOID == bg2 & block_dat_cleaned$year == yr)
      block_neighbor2  <- block_dat_cleaned[neigh[[block_idx2]],]
      block_neighbor2 <- block_neighbor2[block_neighbor2$year == yr,]
      block_neighbor2_all <- bind_rows(block_neighbor2_all,block_neighbor2)
    }
    
    na_idx <- which(is.na(int))
    for (j in 1:length(na_idx)){
      inspection_scores_w_flood[i,na_idx[j]] <- mean(block_neighbor2[[na_idx[j]-11]],na.rm = TRUE)
    }
  }
}
write.csv(inspection_scores_w_flood,"data/inspection_scores_w_flood_block.csv")




################### tract

for (i in 1:nrow(inspection_scores_w_flood_block)){
  if (floor(i/2000) == i / 2000){print(paste("step ",i))}
  lat <- inspection_scores_w_flood_block$LATITUDE[i]
  lon <- inspection_scores_w_flood_block$LONGITUDE[i]
  yr <- inspection_scores_w_flood_block$year[i]
  st <- inspection_scores_w_flood_block$STATE[i]
  ct <- inspection_scores_w_flood_block$COUNTY_NAME[i]
  
  int <- tract_dat_cleaned[tract_dat_cleaned$State == st & tract_dat_cleaned$County == ct &
                             tract_dat_cleaned$year == yr,]
  
  point <- st_as_sf(data.frame(lat = lat, lon = lon), coords = c("lon", "lat"), crs = 4269)
  
  result <- st_join(point, int, join = st_within)
  
  inspection_scores_w_flood_block$tract[i] <- result$GEOID
  inspection_scores_w_flood_block$age_t[i] <- result$age_t
  inspection_scores_w_flood_block$total_pop_t[i] <- result$total_pop_t
  inspection_scores_w_flood_block$total_white_t[i] <- result$total_white_t
  inspection_scores_w_flood_block$total_black_t[i] <- result$total_black_t
  inspection_scores_w_flood_block$total_hispanic_t[i] <- result$total_hispanic_t
  inspection_scores_w_flood_block$poverty_t[i] <- result$poverty_t
  inspection_scores_w_flood_block$income_t[i] <- result$income_t
  inspection_scores_w_flood_block$public_assistance_t[i] <- result$public_assistance_t
  inspection_scores_w_flood_block$owner_t[i] <- result$owner_t
  inspection_scores_w_flood_block$renter_t[i] <- result$renter_t
  inspection_scores_w_flood_block$vehicles_t[i] <- result$vehicles_t
  inspection_scores_w_flood_block$prop_value_t[i] <- result$prop_value_t
}



close <- inspection_scores_w_flood_block[is.na(inspection_scores_w_flood_block$tract),]
close_indices <- which(is.na(inspection_scores_w_flood_block$tract))
for (i in 1:nrow(close)){
  lat <- close$LATITUDE[i]
  lon <- close$LONGITUDE[i]
  yr <- close$year[i]
  st <- close$STATE[i]
  int <- tract_dat_cleaned[tract_dat_cleaned$State == st &
                             tract_dat_cleaned$year == yr,]
  
  point <- st_as_sf(data.frame(lat = lat, lon = lon), coords = c("lon", "lat"), crs = 4269)
  
  closest_geom_index <- st_nearest_feature(point, int)
  closest_geometry <- int[closest_geom_index, ]
  
  distance <- st_distance(point, closest_geometry)
  
  inspection_scores_w_flood_block$tract[close_indices[i]] <- closest_geometry$GEOID
  inspection_scores_w_flood_block$age_t[close_indices[i]] <- closest_geometry$age_t
  inspection_scores_w_flood_block$total_pop_t[close_indices[i]] <- closest_geometry$total_pop_t
  inspection_scores_w_flood_block$total_white_t[close_indices[i]] <- closest_geometry$total_white_t
  inspection_scores_w_flood_block$total_black_t[close_indices[i]] <- closest_geometry$total_black_t
  inspection_scores_w_flood_block$total_hispanic_t[close_indices[i]] <- closest_geometry$total_hispanic_t
  inspection_scores_w_flood_block$poverty_t[close_indices[i]] <- closest_geometry$poverty_t
  inspection_scores_w_flood_block$income_t[close_indices[i]] <- closest_geometry$income_t
  inspection_scores_w_flood_block$public_assistance_t[close_indices[i]] <- closest_geometry$public_assistance_t
  inspection_scores_w_flood_block$owner_t[close_indices[i]] <- closest_geometry$owner_t
  inspection_scores_w_flood_block$renter_t[close_indices[i]] <- closest_geometry$renter_t
  inspection_scores_w_flood_block$vehicles_t[close_indices[i]] <- closest_geometry$vehicles_t
  inspection_scores_w_flood_block$prop_value_t[close_indices[i]] <- closest_geometry$prop_value_t
  
  print(distance)
  
}

neigh_tract = find_neighbors(tract_dat_cleaned)

for (i in 1:nrow(inspection_scores_w_flood_block)){
  int <- inspection_scores_w_flood_block[i,]
  if (sum(is.na(int[,32:43])) > 0){
    bg <- int$tract
    yr <- int$year
    tract_idx <- which(tract_dat_cleaned$GEOID == bg & tract_dat_cleaned$year == yr)
    tract_neighbor <- tract_dat_cleaned[neigh_tract[[tract_idx]],]
    tract_neighbor <- tract_neighbor[tract_neighbor$year == yr,]
    
    na_idx <- which(is.na(int))
    for (j in 1:length(na_idx)){
      inspection_scores_w_flood_block[i,na_idx[j]] <- mean(tract_neighbor[[na_idx[j]-26]],na.rm = TRUE)
    }
  }
}

write.csv(inspection_scores_w_flood_block,"data/inspection_scores_w_flood_blocktract.csv")

##
dat <- read_csv("data/inspection_scores_w_flood_blocktract.csv")
dat$area_b<-0
dat$area_t<-0
for (i in 1:nrow(dat)){
  if (floor(i/2000) == i / 2000){print(paste("step ",i))}
  
  yr <- dat$year[i]
  tr <- dat$tract[i]
  bg <- dat$block_group[i]
  
  dat$area_b[i] <- block_dat2$aland_b[block_dat2$GEOID == bg & block_dat2$year == yr]/10^6
  dat$area_t[i] <- tract_dat2$aland_t[tract_dat2$GEOID == tr & tract_dat2$year == yr]/10^6
}

write.csv(dat,"data/inspection_scores_w_flood_102624.csv")

# NRI data
dat <- read_csv("data/inspection_scores_w_flood_102624.csv")
nri <- read_csv("data/NRI_Table_CensusTracts.csv")
crosswalk <- read_csv("data/nhgis_tr2020_tr2010.csv")

dat$cf_riskscore_t <- 0
dat$cf_riskrating_t <- 0
dat$rf_riskscore_t <- 0
dat$rf_riskrating_t <- 0
dat$f_riskscore_t <- 0
dat$f_riskrating_t <- 0

for (i in 1:nrow(dat)){
  if (floor(i/2000) == i / 2000){print(paste("step ",i))}
  
  yr <- dat$year[i]
  tr <- dat$tract[i]
  
  int <- crosswalk[crosswalk$tr2010ge == tr,]
  int <- int %>%
    left_join(tract_area2, by = c("tr2020ge" = "GEOID"))
  int <- int %>%
    left_join(nri2, by = c("tr2020ge" = "TRACTFIPS"))
  
  if (nrow(int) == 0){
    int_nri <- nri2[nri2$TRACTFIPS == tr,]
    
    dat$cf_riskscore_t[i] <- int_nri$CFLD_RISKS
    dat$rf_riskscore_t[i] <- int_nri$RFLD_RISKS
    dat$f_riskscore_t[i] <- max(dat$cf_riskscore_t[i],dat$rf_riskscore_t[i],na.rm = TRUE)
  }else{
    int$w_CFLD_RISKS <- int$parea * int$ALAND * int$CFLD_RISKS
    int$w_RFLD_RISKS <- int$parea * int$ALAND * int$RFLD_RISKS
    if (sum(int$ALAND == 0) > 0){
      int <- int[int$ALAND != 0, ]
    }
    
    
    dat$cf_riskscore_t[i] <- sum(int$w_CFLD_RISKS) /sum(int$parea * int$ALAND)
    dat$rf_riskscore_t[i] <- sum(int$w_RFLD_RISKS) /sum(int$parea * int$ALAND)
    dat$f_riskscore_t[i] <- max(dat$cf_riskscore_t[i],dat$rf_riskscore_t[i],na.rm = TRUE)
  }
  
  
  if (dat$cf_riskscore_t[i] > cfld_th[1]){
    dat$cf_riskrating_t[i] <- "Very High"
    intscore_c <- 5
  }
  if (dat$cf_riskscore_t[i] > cfld_th[2] & dat$cf_riskscore_t[i] <= cfld_th[1]){
    dat$cf_riskrating_t[i] <- "Relatively High"
    intscore_c <- 4
  }
  if (dat$cf_riskscore_t[i] > cfld_th[3] & dat$cf_riskscore_t[i] <= cfld_th[2]){
    dat$cf_riskrating_t[i] <- "Relatively Moderate"
    intscore_c <- 3
  }
  if (dat$cf_riskscore_t[i] > cfld_th[4] & dat$cf_riskscore_t[i] <= cfld_th[3]){
    dat$cf_riskrating_t[i] <- "Relatively Low"
    intscore_c <- 2
  }
  if (dat$cf_riskscore_t[i] <= cfld_th[4]){
    dat$cf_riskrating_t[i] <- "Very Low"
    intscore_c <- 1
  }
  
  if (dat$rf_riskscore_t[i] > rfld_th[1]){
    dat$rf_riskrating_t[i] <- "Very High"
    intscore_r <- 5
  }
  if (dat$rf_riskscore_t[i] > rfld_th[2] & dat$rf_riskscore_t[i] <= rfld_th[1]){
    dat$rf_riskrating_t[i] <- "Relatively High"
    intscore_r <- 4
  }
  if (dat$rf_riskscore_t[i] > rfld_th[3] & dat$rf_riskscore_t[i] <= rfld_th[2]){
    dat$rf_riskrating_t[i] <- "Relatively Moderate"
    intscore_r <- 3
  }
  if (dat$rf_riskscore_t[i] > rfld_th[4] & dat$rf_riskscore_t[i] <= rfld_th[3]){
    dat$rf_riskrating_t[i] <- "Relatively Low"
    intscore_r <- 2
  }
  if (dat$rf_riskscore_t[i] <= rfld_th[4]){
    dat$rf_riskrating_t[i] <- "Very Low"
    intscore_r <- 1
  }
  intscore <- max(intscore_r, intscore_c)
  dat$f_riskrating_t[i] <- ratings[intscore]
}

cfld_th <- c(mean(min(nri$CFLD_RISKS[nri$CFLD_RISKR == "Very High"]),max(nri$CFLD_RISKS[nri$CFLD_RISKR == "Relatively High"])),
             mean(min(nri$CFLD_RISKS[nri$CFLD_RISKR == "Relatively High"]),max(nri$CFLD_RISKS[nri$CFLD_RISKR == "Relatively Moderate"])),
             mean(min(nri$CFLD_RISKS[nri$CFLD_RISKR == "Relatively Moderate"]),max(nri$CFLD_RISKS[nri$CFLD_RISKR == "Relatively Low"])),
             mean(min(nri$CFLD_RISKS[nri$CFLD_RISKR == "Relatively Low"]),max(nri$CFLD_RISKS[nri$CFLD_RISKR == "Very Low"]))
             )

rfld_th <- c(mean(min(nri$RFLD_RISKS[nri$RFLD_RISKR == "Very High"]),max(nri$RFLD_RISKS[nri$RFLD_RISKR == "Relatively High"])),
             mean(min(nri$RFLD_RISKS[nri$RFLD_RISKR == "Relatively High"]),max(nri$RFLD_RISKS[nri$RFLD_RISKR == "Relatively Moderate"])),
             mean(min(nri$RFLD_RISKS[nri$RFLD_RISKR == "Relatively Moderate"]),max(nri$RFLD_RISKS[nri$RFLD_RISKR == "Relatively Low"])),
             mean(min(nri$RFLD_RISKS[nri$RFLD_RISKR == "Relatively Low"]),max(nri$RFLD_RISKS[nri$RFLD_RISKR == "Very Low"]))
)

## nfip
nfip <- read_csv("data/FimaNfipClaims.csv")
nfip <- nfip[year(nfip$dateOfLoss) > 2012 &year(nfip$dateOfLoss) < 2020,]

#claim
nfip_claim_b <- nfip %>%
  mutate(year = year(dateOfLoss)) %>%
  group_by(year,censusBlockGroupFips) %>%
  summarise(yrClaim_b = n())

nfip_claim_t <- nfip %>%
  mutate(year = year(dateOfLoss)) %>%
  group_by(year,censusTract) %>%
  summarise(yrClaim_t = n())

nfip_tot_claim_b <- nfip %>%
  group_by(censusBlockGroupFips) %>%
  summarise(totClaim_b = n())

nfip_tot_claim_t <- nfip %>%
  group_by(censusTract) %>%
  summarise(totClaim_t = n())


#policycount
nfip_policy_b <- nfip %>%
  mutate(year = year(dateOfLoss)) %>%
  group_by(year,censusBlockGroupFips) %>%
  summarise(yrPolicyCount_b = sum(policyCount,na.rm = TRUE))
nfip_policy_b <- nfip_policy_b[nfip_policy_b$year > 2012 & nfip_policy_b$year < 2020,]

nfip_policy_t <- nfip %>%
  mutate(year = year(dateOfLoss)) %>%
  group_by(year,censusTract) %>%
  summarise(yrPolicyCount_t = sum(policyCount,na.rm = TRUE))
nfip_policy_t <- nfip_policy_t[nfip_policy_t$year > 2012 & nfip_policy_t$year < 2020,]

nfip_tot_policy_b <- nfip %>%
  group_by(censusBlockGroupFips) %>%
  summarise(totPolicyCount_b = sum(policyCount,na.rm = TRUE))

nfip_tot_policy_t <- nfip %>%
  group_by(censusTract) %>%
  summarise(totPolicyCount_t = sum(policyCount,na.rm = TRUE))

# amountPaidOnBuildingClaim
nfip_buildingclaim_b <- nfip %>%
  mutate(year = year(dateOfLoss)) %>%
  group_by(year,censusBlockGroupFips) %>%
  summarise(yrPaidOnBuildingClaim_b = sum(amountPaidOnBuildingClaim,na.rm = TRUE))
# nfip_buildingclaim_b <- nfip_buildingclaim_b[nfip_buildingclaim_b$year > 2012 & nfip_buildingclaim_b$year < 2020,]

nfip_buildingclaim_t <- nfip %>%
  mutate(year = year(dateOfLoss)) %>%
  group_by(year,censusTract) %>%
  summarise(yrPaidOnBuildingclaim_t = sum(amountPaidOnBuildingClaim,na.rm = TRUE))
# nfip_buildingclaim_t <- nfip_buildingclaim_t[nfip_buildingclaim_t$year > 2012 & nfip_buildingclaim_t$year < 2020,]

nfip_tot_buildingclaim_b <- nfip %>%
  group_by(censusBlockGroupFips) %>%
  summarise(totPaidOnBuildingClaim_b = sum(amountPaidOnBuildingClaim,na.rm = TRUE))
# nfip_buildingclaim_b <- nfip_buildingclaim_b[nfip_buildingclaim_b$year > 2012 & nfip_buildingclaim_b$year < 2020,]

nfip_tot_buildingclaim_t <- nfip %>%
  group_by(censusTract) %>%
  summarise(totPaidOnBuildingclaim_t = sum(amountPaidOnBuildingClaim,na.rm = TRUE))
# nfip_buildingclaim_t <- nfip_buildingclaim_t[nfip_buildingclaim_t$year > 2012 & nfip_buildingclaim_t$year < 2020,]

# amountPaidOnContentsClaim
nfip_contentclaim_b <- nfip %>%
  mutate(year = year(dateOfLoss)) %>%
  group_by(year,censusBlockGroupFips) %>%
  summarise(yrPaidOnContentClaim_b = sum(amountPaidOnContentsClaim,na.rm = TRUE))
# nfip_contentclaim_b <- nfip_contentclaim_b[nfip_contentclaim_b$year > 2012 & nfip_contentclaim_b$year < 2020,]

nfip_contentclaim_t <- nfip %>%
  mutate(year = year(dateOfLoss)) %>%
  group_by(year,censusTract) %>%
  summarise(yrPaidOnContentClaim_t = sum(amountPaidOnContentsClaim,na.rm = TRUE))
# nfip_contentclaim_t <- nfip_contentclaim_t[nfip_contentclaim_t$year > 2012 & nfip_contentclaim_t$year < 2020,]

nfip_tot_contentclaim_b <- nfip %>%
  group_by(censusBlockGroupFips) %>%
  summarise(totPaidOnContentClaim_b = sum(amountPaidOnContentsClaim,na.rm = TRUE))

nfip_tot_contentclaim_t <- nfip %>%
  group_by(censusTract) %>%
  summarise(totPaidOnContentClaim_t = sum(amountPaidOnContentsClaim,na.rm = TRUE))

# dat$nfip_policy_b <- 0
# dat$nfip_policy_t <- 0
# dat$nfip_buildingclaim_b <- 0
# dat$nfip_buildingclaim_t <- 0
# dat$nfip_contentclaim_b <- 0
# dat$nfip_contentclaim_t <- 0

nfip_policy_b$censusBlockGroupFips <- as.numeric(nfip_policy_b$censusBlockGroupFips)
nfip_policy_t$censusTract <- as.numeric(nfip_policy_t$censusTract)
nfip_tot_policy_b$censusBlockGroupFips <- as.numeric(nfip_tot_policy_b$censusBlockGroupFips)
nfip_tot_policy_t$censusTract <- as.numeric(nfip_tot_policy_t$censusTract)
nfip_buildingclaim_b$censusBlockGroupFips <- as.numeric(nfip_buildingclaim_b$censusBlockGroupFips)
nfip_buildingclaim_t$censusTract <- as.numeric(nfip_buildingclaim_t$censusTract)
nfip_tot_buildingclaim_b$censusBlockGroupFips <- as.numeric(nfip_tot_buildingclaim_b$censusBlockGroupFips)
nfip_tot_buildingclaim_t$censusTract <- as.numeric(nfip_tot_buildingclaim_t$censusTract)
nfip_contentclaim_b$censusBlockGroupFips <- as.numeric(nfip_contentclaim_b$censusBlockGroupFips)
nfip_contentclaim_t$censusTract <- as.numeric(nfip_contentclaim_t$censusTract)
nfip_tot_contentclaim_b$censusBlockGroupFips <- as.numeric(nfip_tot_contentclaim_b$censusBlockGroupFips)
nfip_tot_contentclaim_t$censusTract <- as.numeric(nfip_tot_contentclaim_t$censusTract)
nfip_claim_b$censusBlockGroupFips <- as.numeric(nfip_claim_b$censusBlockGroupFips)
nfip_claim_t$censusTract <- as.numeric(nfip_claim_t$censusTract)
nfip_tot_claim_b$censusBlockGroupFips <- as.numeric(nfip_tot_claim_b$censusBlockGroupFips)
nfip_tot_claim_t$censusTract <- as.numeric(nfip_tot_claim_t$censusTract)

dat <- dat %>%
  left_join(nfip_policy_b, by = c("year", "block_group" = "censusBlockGroupFips"))
dat <- dat %>%
  left_join(nfip_policy_t, by = c("year", "tract" = "censusTract"))
dat <- dat %>%
  left_join(nfip_buildingclaim_b, by = c("year", "block_group" = "censusBlockGroupFips"))
dat <- dat %>%
  left_join(nfip_buildingclaim_t, by = c("year", "tract" = "censusTract"))
dat <- dat %>%
  left_join(nfip_contentclaim_b, by = c("year", "block_group" = "censusBlockGroupFips"))
dat <- dat %>%
  left_join(nfip_contentclaim_t, by = c("year", "tract" = "censusTract"))
dat <- dat %>%
  left_join(nfip_claim_b, by = c("year", "block_group" = "censusBlockGroupFips"))
dat <- dat %>%
  left_join(nfip_claim_t, by = c("year", "tract" = "censusTract"))

dat <- dat %>%
  left_join(nfip_tot_policy_b, by = c("block_group" = "censusBlockGroupFips"))
dat <- dat %>%
  left_join(nfip_tot_policy_t, by = c("tract" = "censusTract"))
dat <- dat %>%
  left_join(nfip_tot_buildingclaim_b, by = c("block_group" = "censusBlockGroupFips"))
dat <- dat %>%
  left_join(nfip_tot_buildingclaim_t, by = c("tract" = "censusTract"))
dat <- dat %>%
  left_join(nfip_tot_contentclaim_b, by = c("block_group" = "censusBlockGroupFips"))
dat <- dat %>%
  left_join(nfip_tot_contentclaim_t, by = c("tract" = "censusTract"))
dat <- dat %>%
  left_join(nfip_tot_claim_b, by = c("block_group" = "censusBlockGroupFips"))
dat <- dat %>%
  left_join(nfip_tot_claim_t, by = c("tract" = "censusTract"))

dat$cf_riskscore_t2[is.na(dat$cf_riskscore_t2)] <- 0
dat$totPolicyCount_b[is.na(dat$totPolicyCount_b)] <- 0
dat$totPolicyCount_t[is.na(dat$totPolicyCount_t)] <- 0
dat$totPaidOnBuildingClaim_b[is.na(dat$totPaidOnBuildingClaim_b)] <- 0
dat$totPaidOnBuildingclaim_t[is.na(dat$totPaidOnBuildingclaim_t)] <- 0
dat$totPaidOnContentClaim_b[is.na(dat$totPaidOnContentClaim_b)] <- 0
dat$totPaidOnContentClaim_t[is.na(dat$totPaidOnContentClaim_t)] <- 0
dat$yrPolicyCount_b[is.na(dat$yrPolicyCount_b)] <- 0
dat$yrPolicyCount_t[is.na(dat$yrPolicyCount_t)] <- 0
dat$yrPaidOnBuildingClaim_b[is.na(dat$yrPaidOnBuildingClaim_b)] <- 0
dat$yrPaidOnBuildingclaim_t[is.na(dat$yrPaidOnBuildingclaim_t)] <- 0
dat$yrPaidOnContentClaim_b[is.na(dat$yrPaidOnContentClaim_b)] <- 0
dat$yrPaidOnContentClaim_t[is.na(dat$yrPaidOnContentClaim_t)] <- 0
dat$totClaim_b[is.na(dat$totClaim_b)] <- 0
dat$totClaim_t[is.na(dat$totClaim_t)] <- 0
dat$yrClaim_b[is.na(dat$yrClaim_b)] <- 0
dat$yrClaim_t[is.na(dat$yrClaim_t)] <- 0

names(dat)[names(dat) == "totPaidOnBuildingclaim_t"] <- "totPaidOnBuildingClaim_t"
names(dat)[names(dat) == "yrPaidOnBuildingclaim_t"] <- "yrPaidOnBuildingClaim_t"

write.csv(dat,"data/inspection_scores_w_flood_110624.csv",row.names=FALSE)


x <- nri$CFLD_RISKV
ecdf_x <- ecdf(x)
percentiles <- ecdf_x(x) * 100  # Convert to percentiles
result <- data.frame(NRI = nri$CFLD_RISKS, Percentile = percentiles)
result

x <- nri$CFLD_EALT
ecdf_x <- ecdf(x)
percentiles <- ecdf_x(x) * 100  # Convert to percentiles
result <- data.frame(NRI = nri$CFLD_EALS, Percentile = percentiles)
result



###
nri2 <- nri[,c(11,69,76,82,300,309,316)]
tract2010 <- per_capita_incom_tract[per_capita_incom_tract$year == 2013,1]
for (i in 18764:nrow(tract2010)){
  if (floor(i/2000) == i / 2000){print(paste("step ",i))}
  
  tr <- tract2010$GEOID[i]
  
  int <- crosswalk[crosswalk$tr2010ge == tr,]
  int <- int %>%
    left_join(tract_area2, by = c("tr2020ge" = "GEOID"))
  int <- int %>%
    left_join(nri2, by = c("tr2020ge" = "TRACTFIPS"))
  
  if (nrow(int) == 0){
    int_nri <- nri2[nri2$TRACTFIPS == tr,]
    if (nrow(int_nri) == 0){
      tract2010$cf_riskvalue_t[i] <- NA
      tract2010$rf_riskvalue_t[i] <- NA
      tract2010$cf_expovalue_t[i] <- NA
      tract2010$rf_expovalue_t[i] <- NA
      tract2010$cf_lossvalue_t[i] <- NA
      tract2010$rf_lossvalue_t[i] <- NA
      
    }else{
      tract2010$cf_riskvalue_t[i] <- int_nri$CFLD_RISKV
      tract2010$rf_riskvalue_t[i] <- int_nri$RFLD_RISKV
      tract2010$cf_expovalue_t[i] <- int_nri$CFLD_EXPT
      tract2010$rf_expovalue_t[i] <- int_nri$RFLD_EXPT
      tract2010$cf_lossvalue_t[i] <- int_nri$CFLD_EALT
      tract2010$rf_lossvalue_t[i] <- int_nri$RFLD_EALT
    }
  }else{
    int$w_CFLD_RISKV <- int$parea * int$ALAND * int$CFLD_RISKV
    int$w_RFLD_RISKV <- int$parea * int$ALAND * int$RFLD_RISKV
    int$w_CFLD_EXPT <- int$parea * int$ALAND * int$CFLD_EXPT
    int$w_RFLD_EXPT <- int$parea * int$ALAND * int$RFLD_EXPT
    int$w_CFLD_EALT <- int$parea * int$ALAND * int$CFLD_EALT
    int$w_RFLD_EALT <- int$parea * int$ALAND * int$RFLD_EALT
    if (sum(int$ALAND == 0) > 0){
      int <- int[int$ALAND != 0, ]
    }
    
    tract2010$cf_riskvalue_t[i] <- sum(int$w_CFLD_RISKV) /sum(int$parea * int$ALAND)
    tract2010$rf_riskvalue_t[i] <- sum(int$w_RFLD_RISKV) /sum(int$parea * int$ALAND)
    tract2010$cf_expovalue_t[i] <- sum(int$w_CFLD_EXPT) /sum(int$parea * int$ALAND)
    tract2010$rf_expovalue_t[i] <- sum(int$w_RFLD_EXPT) /sum(int$parea * int$ALAND)
    tract2010$cf_lossvalue_t[i] <- sum(int$w_CFLD_EALT) /sum(int$parea * int$ALAND)
    tract2010$rf_lossvalue_t[i] <- sum(int$w_RFLD_EALT) /sum(int$parea * int$ALAND)
    
  }
}

x <- tract2010$cf_riskvalue_t
ecdf_x <- ecdf(x)
percentiles <- ecdf_x(x) * 100  # Convert to percentiles
tract2010$cf_riskscore_t <- percentiles
tract2010$cf_riskscore_t[tract2010$cf_riskscore_t == min(percentiles,na.rm = TRUE)] <- 0

x <- tract2010$rf_riskvalue_t
ecdf_x <- ecdf(x)
percentiles <- ecdf_x(x) * 100  # Convert to percentiles
tract2010$rf_riskscore_t <- percentiles
tract2010$rf_riskscore_t[tract2010$rf_riskscore_t == min(percentiles,na.rm = TRUE)] <- 0

x <- tract2010$cf_expovalue_t
ecdf_x <- ecdf(x)
percentiles <- ecdf_x(x) * 100  # Convert to percentiles
tract2010$cf_exposcore_t <- percentiles
tract2010$cf_exposcore_t[tract2010$cf_exposcore_t == min(percentiles,na.rm = TRUE)] <- 0

x <- tract2010$rf_expovalue_t
ecdf_x <- ecdf(x)
percentiles <- ecdf_x(x) * 100  # Convert to percentiles
tract2010$rf_exposcore_t <- percentiles
tract2010$rf_exposcore_t[tract2010$rf_exposcore_t == min(percentiles,na.rm = TRUE)] <- 0

x <- tract2010$cf_lossvalue_t
ecdf_x <- ecdf(x)
percentiles <- ecdf_x(x) * 100  # Convert to percentiles
tract2010$cf_lossscore_t <- percentiles
tract2010$cf_lossscore_t[tract2010$cf_lossscore_t == min(percentiles,na.rm = TRUE)] <- 0

x <- tract2010$rf_lossvalue_t
ecdf_x <- ecdf(x)
percentiles <- ecdf_x(x) * 100  # Convert to percentiles
tract2010$rf_lossscore_t <- percentiles
tract2010$rf_lossscore_t[tract2010$rf_lossscore_t == min(percentiles,na.rm = TRUE)] <- 0

tract2010 <-tract2010[,c(1,8:13)]
colnames(tract2010) <- c("GEOID","cf_riskscore_t2","cf_exposcore_t2","cf_lossscore_t2",
                         "rf_exposcore_t2","rf_lossscore_t2","rf_riskscore_t2")

dat2 <- dat %>%
  left_join(tract2010, by = c("tract" = "GEOID"))

nri3 <- nri[,c(11,69,77,83,300,310,317)]
x <- nri3$CFLD_EXPT
ecdf_x <- ecdf(x)
percentiles <- ecdf_x(x) * 100  # Convert to percentiles
nri3$CFLD_EXPS <- percentiles
nri3$CFLD_EXPS[nri3$CFLD_EXPS == min(percentiles,na.rm = TRUE)] <- 0

write.csv(dat2,"data/inspection_scores_w_flood_110224.csv")

dat <- read_csv("data/inspection_scores_w_flood_110224.csv")
dat$cf_riskscore_t2[is.na(dat$cf_riskscore_t2)] <- 0
dat$totPolicyCount_b[is.na(dat$totPolicyCount_b)] <- 0
dat$totPolicyCount_t[is.na(dat$totPolicyCount_t)] <- 0
dat$totPaidOnBuildingClaim_b[is.na(dat$totPaidOnBuildingClaim_b)] <- 0
dat$totPaidOnBuildingClaim_t[is.na(dat$totPaidOnBuildingClaim_t)] <- 0
dat$totPaidOnContentClaim_b[is.na(dat$totPaidOnContentClaim_b)] <- 0
dat$totPaidOnContentClaim_t[is.na(dat$totPaidOnContentClaim_t)] <- 0

dat$yrPolicyCount_b[is.na(dat$yrPolicyCount_b)] <- 0
dat$yrPolicyCount_t[is.na(dat$yrPolicyCount_t)] <- 0
dat$yrPaidOnBuildingClaim_b[is.na(dat$yrPaidOnBuildingClaim_b)] <- 0
dat$yrPaidOnBuildingClaim_t[is.na(dat$yrPaidOnBuildingClaim_t)] <- 0
dat$yrPaidOnContentClaim_b[is.na(dat$yrPaidOnContentClaim_b)] <- 0
dat$yrPaidOnContentClaim_t[is.na(dat$yrPaidOnContentClaim_t)] <- 0

dat <- dat %>%
  left_join(per_capita_incom_blockgroup, by = c("year","block_group" = "GEOID"))
dat <- dat %>%
  left_join(per_capita_incom_tract, by = c("year","tract" = "GEOID"))

# load("block_data.RData")

dat <- read_csv("data/inspection_scores_w_flood_110624.csv")
dat$block_group <- as.numeric(inspection_scores_w_flood$block_group)

# neigh = find_neighbors(block_dat_cleaned)

for (i in 1:nrow(dat)){
  int <- dat[i,]
  if (sum(is.na(int[,63])) > 0){
    bg <- int$block_group
    yr <- int$year
    block_idx <- which(as.numeric(block_dat_cleaned$GEOID) == bg & block_dat_cleaned$year == yr)
    if (length(block_idx) >0){
      block_neighbor <- per_capita_incom_blockgroup[neigh[[block_idx]],]
      block_neighbor <- block_neighbor[block_neighbor$year == yr,]
      
      dat[i,63] <- mean(block_neighbor[[2]],na.rm = TRUE)
    }else{
      dat[i,63] <- NA
    }
    
  }
}

for (i in 1:nrow(dat)){
  int <- dat[i,]
  if (sum(is.na(int[,64])) > 0){
    tr <- int$tract
    yr <- int$year
    tract_idx <- which(as.numeric(tract_dat$GEOID) == tr & tract_dat$year == yr)
    if (length(tract_idx) >0){
      tract_neighbor <- per_capita_incom_tract[neigh_t[[tract_idx]],]
      tract_neighbor <- tract_neighbor[tract_neighbor$year == yr,]
      
      dat[i,64] <- mean(tract_neighbor[[2]],na.rm = TRUE)
    }else{
      dat[i,64] <- NA
    }
    
  }
}

write.csv(dat,"data/inspection_scores_w_flood_110824.csv")

# unit_blockgroup <- unit_blockgroup[,c(1,3,4)]
# unit_tract <- unit_tract[,c(1,3,4)]
unit_blockgroup$GEOID <- as.numeric(unit_blockgroup$GEOID)
unit_tract$GEOID <- as.numeric(unit_tract$GEOID)

dat <- dat %>%
  left_join(unit_blockgroup, by = c("year","block_group" = "GEOID"))
dat <- dat %>%
  left_join(unit_tract, by = c("year","tract" = "GEOID"))

write.csv(dat,"data/inspection_scores_w_flood_110824.csv")

