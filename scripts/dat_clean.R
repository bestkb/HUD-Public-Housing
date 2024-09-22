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
# ~9K NA values

# Try to exchange block ID of 2010 and 2020 just in case
# this loop takes some time.
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

# from county data (using tidycensus)
load("data/county_dat.RData")
inspection_scores_w_flood_demo$county <- inspection_scores_w_flood_demo$STATE_CODE*1000+inspection_scores_w_flood_demo$COUNTY_CODE 

for (i in 1:nrow(inspection_scores_w_flood_demo)){
  for (col in target_cols) {
    if (is.na(inspection_scores_w_flood_demo[i,col])){
      current_county1 <- inspection_scores_w_flood_demo$county[i]
      current_year <- inspection_scores_w_flood_demo$year[i]
      
      row <- which(county_dat$GEOID == current_county1 & 
                             county_dat$year == current_year)
      if (length(row) == 0) {
        inspection_scores_w_flood_demo[i, col] <- NA
      } else {
        inspection_scores_w_flood_demo[i, col] <- county_dat[[col]][row]
      }
      
    }
  }
}

print(paste("Rows with NAs : ",(sum(rowSums(is.na(inspection_scores_w_flood_demo)) > 0))))
# 2 NA values!

inspection_scores_w_flood_demo <- na.omit(inspection_scores_w_flood_demo)

# nri <- read_csv("data/NRI_Table_CensusTracts.csv")
# nri <- nri %>%
#   select(c("TRACTFIPS","RISK_RATNG","SOVI_RATNG","RESL_RATNG","CFLD_RISKR","HWAV_RISKR",
#            "HRCN_RISKR","RFLD_RISKR","WFIR_RISKR"))
# write.csv(nri,"data/NRI_Table_CensusTracts.csv")

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

inspection_scores_final_nri <- inspection_scores_w_flood_demo %>%
  left_join(nri, by = c("TRACT_2010" = "TRACTFIPS"))

write.csv(inspection_scores_final_nri,"data/insp_score_flood_demo_nri.csv")


