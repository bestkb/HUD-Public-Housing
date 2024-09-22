library(tidyverse)
library(readxl)
library(tidycensus)
library(tigris)
library(dplyr)
library(gstat)
library(sp)

setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

# Load flood data with updated GEOIDs
hud_w_flood <- read_csv("data/PublicHousing_FloodData_updated.csv")
hud_w_flood <- hud_w_flood %>%
  mutate(in_floodplain = ifelse(FldZone_YN == "Yes", 1, 0))
hud_w_flood <- hud_w_flood %>%
  mutate(county_2010 = substr(TRACT_2010, 1, 5),
         county_2020 = substr(TRACT_2020, 1, 5))
hud_w_flood <- hud_w_flood %>%
  select(c("FloodDistM","DEVELOPMEN","LATITUDE","LONGITUDE","STATE_NAME","FLD_AR_ID",
           "STUDY_TYP","FLD_ZONE","ZONE_SUBTY","SFHA_TF","STATIC_BFE","V_DATUM",
           "DEPTH","LEN_UNIT","VELOCITY","VEL_UNIT","AR_REVERT","AR_SUBTRV","BFE_REVERT",
           "DEP_REVERT","DUAL_ZONE","SOURCE_CIT","GFID","Shape_Leng","Shape_Area",
           "FldZone_YN","TRACT_2010","TRACT_2020","BLOCK_2010","BLOCK_2020","in_floodplain",
           "county_2010","county_2020"))

hud_w_flood <- unique(hud_w_flood)
hud_w_flood <- hud_w_flood[!is.na(hud_w_flood$TRACT_2010), ]

# Load inspection_score data with updated GEOIDs
inspection_scores <- read_csv("data/insp_score_2010added.csv")
inspection_scores$BLOCK_2010 <- inspection_scores$BLOCKID10
inspection_scores$TRACT_2010 <- floor(inspection_scores$BLOCKID10/10000)
inspection_scores <- inspection_scores %>%
  mutate(tract = if_else(`inspection_year` < 2020, TRACT_2010, TRACT_2020))
inspection_scores <- inspection_scores %>%
  mutate(block = if_else(`inspection_year` < 2020, BLOCK_2010, BLOCK_2020))
inspection_scores$block_group <- floor(inspection_scores$block/1000)

inspection_scores <- inspection_scores %>%
  select(c("DEVELOPMENT_ID","DEVELOPMENT_NAME","CBSA_CODE","COUNTY_NAME","COUNTY_CODE",
           "STATE_NAME.x","STATE_CODE","ZIPCODE","LATITUDE","LONGITUDE","PHA_CODE","INSPECTION_SCORE",
           "inspection_year","TRACT_2010","TRACT_2020","BLOCK_2010","BLOCK_2020","tract","block","block_group"))
colnames(inspection_scores)[colnames(inspection_scores) == "inspection_year"] <- "year"

# remove overlaps in inspection score data
inspection_scores_uniq <- inspection_scores %>%
  distinct()

print(paste("There are ",(nrow(inspection_scores)-nrow(inspection_scores_uniq))," overlaps which are now removed."))

inspection_scores_w_flood <- inspection_scores_uniq %>%
  left_join(hud_w_flood, by = c("DEVELOPMENT_ID" = "DEVELOPMEN", "LATITUDE", "LONGITUDE"))

# Join flood data to inspection scores
inspection_scores_w_flood <- inspection_scores_w_flood %>%
  select(c("DEVELOPMENT_ID","DEVELOPMENT_NAME","CBSA_CODE","COUNTY_NAME",
           "COUNTY_CODE","STATE_NAME.x","STATE_CODE","ZIPCODE",
           "LATITUDE","LONGITUDE","PHA_CODE","INSPECTION_SCORE",
           "year","TRACT_2010.x","TRACT_2020.x","BLOCK_2010.x",
           "BLOCK_2020.x","tract","block","block_group","FloodDistM","in_floodplain"))
inspection_scores_w_flood <- inspection_scores_w_flood[rowSums(is.na(inspection_scores_w_flood)) == 0, ]
inspection_scores_w_flood <- inspection_scores_w_flood[inspection_scores_w_flood$year > 2009 &
                                                         inspection_scores_w_flood$year <=2020,]
colnames(inspection_scores_w_flood)[colnames(inspection_scores_w_flood) == "DEVELOPMENT_ID.x"] <- "DEVELOPMENT_ID"
colnames(inspection_scores_w_flood)[colnames(inspection_scores_w_flood) == "TRACT_2010.x"] <- "TRACT_2010"
colnames(inspection_scores_w_flood)[colnames(inspection_scores_w_flood) == "TRACT_2020.x"] <- "TRACT_2020"
colnames(inspection_scores_w_flood)[colnames(inspection_scores_w_flood) == "BLOCK_2010.x"] <- "BLOCK_2010"
colnames(inspection_scores_w_flood)[colnames(inspection_scores_w_flood) == "BLOCK_2020.x"] <- "BLOCK_2020"
inspection_scores_w_flood$BLOCK_2010 <- floor(inspection_scores_w_flood$BLOCK_2010)
inspection_scores_w_flood$BLOCK_2020 <- floor(inspection_scores_w_flood$BLOCK_2020)
inspection_scores_w_flood$TRACT_2010 <- floor(inspection_scores_w_flood$TRACT_2010)
inspection_scores_w_flood$TRACT_2020 <- floor(inspection_scores_w_flood$TRACT_2020)
inspection_scores_w_flood$tract <- floor(inspection_scores_w_flood$tract)
inspection_scores_w_flood$block <- floor(inspection_scores_w_flood$block)
inspection_scores_w_flood$block_group <- floor(inspection_scores_w_flood$block_group)
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


