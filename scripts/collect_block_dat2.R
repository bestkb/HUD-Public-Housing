library(tidycensus)
library(tidyr)
library(dplyr)
library(tigris)
library(sf)

setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

# API for Woi
census_api_key("8c8fe7ff5198b2b97811f1f66873724c02e8630b", install = TRUE,
               overwrite = TRUE)

v10 <- load_variables(2010, "acs5", cache = TRUE)
View(v10)

data(fips_codes)
state_codes <- unique(fips_codes$state)
state_codes <- state_codes[1:51]
state_full_names <- state.name[match(state_codes, state.abb)]
state_full_names[9] <- "District of Columbia"

inspection_score_uniq <- read.csv("data/inspection_scores_w_flood_033125.csv",header=TRUE)



block_dat <- data.frame()
for (i in 2013:2019){
  for (j in 1:51){
    x <- get_acs(geography = "block group",
                 variables = c(age_b = "B01002_001",
                               income_b =  "B19013_001",
                               prop_value_b = "B25077_001",
                               public_assistance_b = "B19057_001",  
                               poverty_b = "B17010_001", 
                               vehicles_b = "B25044_001",
                               renter_b = "B25003_003",
                               owner_b = "B25003_002",
                               total_white_b = "B03002_003",
                               total_black_b = "B03002_004",
                               total_hispanic_b = "B03002_012",
                               total_pop_b = "B01003_001"),
                 year = i,
                 state = state_codes[j],
                 survey = "acs5",
                 geometry = TRUE)
    
    x2 <- x %>%
      select(-moe) %>%  # Remove the `moe` column
      pivot_wider(
        names_from = variable,  # Columns will be created based on the `variable` column
        values_from = estimate  # Values will come from `estimate` and `moe`
      )
    
    x2$year <- i
    block_dat <- rbind(block_dat,x2)
  }
}

save.image("block_data.RData")

block_dat2 <- data.frame()
for (i in 2013:2019){
  for (j in 1:51){
    state_name <- state_full_names[j]
    state_abb <- state_codes[j]
    int <- block_dat_cleaned[block_dat_cleaned$year == i & block_dat_cleaned$State == state_name,] %>%
      arrange(GEOID)
    block_groups_aland <- block_groups(state = state_abb, year = i, class = "sf") %>%
      arrange(GEOID)
    if (sum(block_groups_aland$GEOID !=int$GEOID) == 0){
      int$aland_b <- block_groups_aland$ALAND
    }else{
      print(paste("Check state ",state_name,"Year ",i))
    }

  block_dat2 <- rbind(block_dat2,int)
  }
}

# block_dat_cleaned <- block_dat %>%
#   separate(NAME, into = c("BlockGroup", "CensusTract", "County", "State"), sep = ", ") %>%
#   mutate(County = gsub(" County", "", County))


unique_statecounty <- block_dat_cleaned %>%
  distinct(State, County)


#### tract

tract_dat <- data.frame()
for (i in 2013:2019){
  # print(paste("YEAR : ",i))
  for (j in 1:51){
    x <- get_acs(geography = "tract",
                 variables = c(age_t = "B01002_001",
                               income_t =  "B19013_001",
                               prop_value_t = "B25077_001",
                               public_assistance_t = "B19057_001",  
                               poverty_t = "B17010_001", 
                               vehicles_t = "B25044_001",
                               renter_t = "B25003_003",
                               owner_t = "B25003_002",
                               total_white_t = "B03002_003",
                               total_black_t = "B03002_004",
                               total_hispanic_t = "B03002_012",
                               total_pop_t = "B01003_001"),
                 year = i,
                 state = state_codes[j],
                 survey = "acs5",
                 geometry = TRUE)
    
    x2 <- x %>%
      select(-moe) %>%  # Remove the `moe` column
      pivot_wider(
        names_from = variable,  # Columns will be created based on the `variable` column
        values_from = estimate  # Values will come from `estimate` and `moe`
      )
    
    x2$year <- i
    tract_dat <- rbind(tract_dat,x2)
  }
}

save.image("tract_data.RData")


tract_dat2 <- data.frame()
for (i in 2013:2019){
  for (j in 1:51){
    state_name <- state_full_names[j]
    state_abb <- state_codes[j]
    int <- tract_dat_cleaned[tract_dat_cleaned$year == i & tract_dat_cleaned$State == state_name,] %>%
      arrange(GEOID)
    tract_aland <- tracts(state = state_abb, year = i, class = "sf") %>%
      arrange(GEOID)
    if (sum(tract_aland$GEOID !=int$GEOID) == 0){
      int$aland_t <- tract_aland$ALAND
    }else{
      print(paste("Check state ",state_name,"Year ",i))
    }
    
    tract_dat2 <- rbind(tract_dat2,int)
  }
}

tract_dat_cleaned <- tract_dat2 %>%
  separate(NAME, into = c("CensusTract", "County", "State"), sep = ", ") %>%
  mutate(County = gsub(" County", "", County))


unique_statecounty <- tract_dat_cleaned %>%
  distinct(State, County)


tract_area <- data.frame()
for (j in 1:51){
  state_name <- state_full_names[j]
  state_abb <- state_codes[j]
  tract_aland <- tracts(state = state_abb, year = 2020, class = "sf") %>%
    arrange(GEOID)
  tract_area <- rbind(tract_area,tract_aland)
}


tract_dat <- tract_dat2
## per-capita income in tract
income_dat <- data.frame()
for (i in 2013:2019){
  # print(paste("YEAR : ",i))
  for (j in 1:51){
    x <- get_acs(geography = "tract",
                 variables = c(per_capita_income_t = "B19301_001"),
                 year = i,
                 state = state_codes[j],
                 survey = "acs5",
                 geometry = FALSE)
    
    x2 <- x %>%
      select(-moe) %>%  # Remove the `moe` column
      pivot_wider(
        names_from = variable,  # Columns will be created based on the `variable` column
        values_from = estimate  # Values will come from `estimate` and `moe`
      )
    
    x2$year <- i
    income_dat <- rbind(income_dat,x2)
  }
}

per_capita_incom_tract <- tract_dat

## per-capita income in tract
per_capita_incom_blockgroup <- data.frame()
for (i in 2013:2019){
  # print(paste("YEAR : ",i))
  for (j in 1:51){
    x <- get_acs(geography = "block group",
                 variables = c(per_capita_income_b = "B19301_001"),
                 year = i,
                 state = state_codes[j],
                 survey = "acs5",
                 geometry = FALSE)
    
    x2 <- x %>%
      select(-moe) %>%  # Remove the `moe` column
      pivot_wider(
        names_from = variable,  # Columns will be created based on the `variable` column
        values_from = estimate  # Values will come from `estimate` and `moe`
      )
    
    x2$year <- i
    per_capita_incom_blockgroup <- rbind(per_capita_incom_blockgroup,x2)
  }
}

per_capita_incom_blockgroup <- per_capita_incom_blockgroup[,c(1,3,4)]
per_capita_incom_tract <- per_capita_incom_tract[,c(1,3,4)]

per_capita_incom_blockgroup$GEOID <- as.numeric(per_capita_incom_blockgroup$GEOID)
per_capita_incom_tract$GEOID <- as.numeric(per_capita_incom_tract$GEOID)

tract_dat$per_capita_income_t <- income_dat$per_capita_income_t
tract_dat$pop_density_t <- tract_dat$total_pop_t /tract_dat$aland_t

## unit
## unit in tract
tract_dat <- data.frame()
for (i in 2013:2019){
  # print(paste("YEAR : ",i))
  for (j in 1:51){
    x <- get_acs(geography = "tract",
                 variables = c(unit_t = "B25001_001"),
                 year = i,
                 state = state_codes[j],
                 survey = "acs5",
                 geometry = FALSE)
    
    x2 <- x %>%
      select(-moe) %>%  # Remove the `moe` column
      pivot_wider(
        names_from = variable,  # Columns will be created based on the `variable` column
        values_from = estimate  # Values will come from `estimate` and `moe`
      )
    
    x2$year <- i
    tract_dat <- rbind(tract_dat,x2)
  }
}

unit_tract <- tract_dat

## unit in tract
unit_blockgroup <- data.frame()
for (i in 2013:2019){
  # print(paste("YEAR : ",i))
  for (j in 1:51){
    x <- get_acs(geography = "block group",
                 variables = c(unit_b = "B25001_001"),
                 year = i,
                 state = state_codes[j],
                 survey = "acs5",
                 geometry = FALSE)
    
    x2 <- x %>%
      select(-moe) %>%  # Remove the `moe` column
      pivot_wider(
        names_from = variable,  # Columns will be created based on the `variable` column
        values_from = estimate  # Values will come from `estimate` and `moe`
      )
    
    x2$year <- i
    unit_blockgroup <- rbind(unit_blockgroup,x2)
  }
}


louisiana_sf_homes <- get_acs(
  geography = "tract",
  state = "LA",
  year = 2020,
  survey = "acs5",
  variables = c(
    detached = "B25024_002",  # Code for single-family detached
    attached = "B25024_003"
  ),
  geometry = TRUE  # Optional: if you want spatial data
)

louisiana_sf_homes_summary <- louisiana_sf_homes %>%
  group_by(NAME) %>%
  summarize(
    total_single_family_units = sum(estimate, na.rm = TRUE)
  )

# View the summarized data
louisiana_sf_homes_summary

louisiana_sf_units <- get_acs(
  geography = "tract",
  state = "LA",
  year = 2020,
  survey = "acs5",
  variables = c(
    unit = "B25001_001"# Code for single-family attached
  ),
  geometry = TRUE  # Optional: if you want spatial data
)

louisiana_sf_units_summary <- louisiana_sf_units %>%
  group_by(NAME) %>%
  summarize(
    total_units = sum(estimate, na.rm = TRUE)
  )

louisiana_sf_homes_summary$units <- louisiana_sf_units_summary$total_units
louisiana_sf_homes_summary$percentage <- louisiana_sf_homes_summary$total_single_family_units /louisiana_sf_homes_summary$units * 100

ggplot(louisiana_sf_homes_summary) +
  geom_sf(aes(fill = percentage), color = NA) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 50,  # Midpoint set to 0 for neutral values
                       name = "Single Family Units %", 
                       labels = scales::comma) + 
  labs(
    title = "Single Family Units percentages in Louisiana (2020)",
    caption = "Source: American Community Survey 2016-2020"
  ) +
  theme_minimal()


louisiana_sf_renter <- get_acs(
  geography = "tract",
  state = "LA",
  year = 2020,
  survey = "acs5",
  variables = c(
    renter = "B25024_003"  # Code for single-family detached
  ),
  geometry = TRUE  # Optional: if you want spatial data
)

louisiana_sf_all <- get_acs(
  geography = "tract",
  state = "LA",
  year = 2020,
  survey = "acs5",
  variables = c(
    total = "B25024_002"
  ),
  geometry = TRUE  # Optional: if you want spatial data
)

louisiana_sf_renter_summary <- louisiana_sf_renter %>%
  group_by(NAME) %>%
  summarize(
    renter = sum(estimate, na.rm = TRUE)
  )

louisiana_sf_all_summary <- louisiana_sf_all %>%
  group_by(NAME) %>%
  summarize(
    total = sum(estimate, na.rm = TRUE)
  )

louisiana_sf_renter_summary$owner <- louisiana_sf_all_summary$total
louisiana_sf_renter_summary$percentage <- louisiana_sf_renter_summary$renter / (louisiana_sf_renter_summary$owner + louisiana_sf_renter_summary$renter)

ggplot(louisiana_sf_renter_summary) +
  geom_sf(aes(fill = percentage), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Total Single Family Units") +
  labs(
    title = "Renterratio in Louisiana (2020)",
    caption = "Source: American Community Survey 2016-2020"
  ) +
  theme_minimal()



##
library(tmap)

tmap_mode("view")  # Set to interactive mode

franklinton_sf_income_2019 <- get_acs(
  geography = "tract",
  state = "OH",
  year = 2019,
  survey = "acs5",
  variables = c(
    # unit_b = "B25001_001",
    pc_income_b = "B19301_001"
  ),
  geometry = TRUE  
)

franklinton_sf_income_summary <- franklinton_sf_income_2019 %>%
  group_by(NAME) %>%
  summarize(
    pc_income_t = sum(estimate, na.rm = TRUE)
  )
franklinton_sf_income_summary$admin <- franklinton_sf_income_summary$NAME
franklinton_sf_income_cleaned <- franklinton_sf_income_summary %>%
  separate(NAME, into = c("CensusTract", "County", "State"), sep = ", ") %>%
  mutate(County = gsub(" County", "", County), CensusTract = gsub("Census Tract ","",CensusTract))


franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned %>%
  select(admin, everything())

franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$County == "Franklin",]
franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$CensusTract == "42" |
                                                                 franklinton_sf_income_cleaned$CensusTract == "43" | 
                                                                 franklinton_sf_income_cleaned$CensusTract == "50",]

tm_shape(franklinton_sf_income_cleaned) +
  tm_polygons("pc_income_t", palette = "viridis",alpha = 0.5,style = "cont", title = "Per capita income") +
  tm_text("pc_income_t", size = 1, col = "white", auto.placement = TRUE) + 
  tm_basemap("OpenStreetMap") +
  tm_layout(title = "Per capita income in Franklin (2019)", frame = FALSE)


##
franklinton_sf_income_2009 <- get_acs(
  geography = "tract",
  state = "OH",
  year = 2009,
  survey = "acs5",
  variables = c(
    # unit_b = "B25001_001",
    pc_income_b = "B19301_001"
  ),
  geometry = TRUE  
)

franklinton_sf_income_summary <- franklinton_sf_income_2009 %>%
  group_by(NAME) %>%
  summarize(
    pc_income_t = sum(estimate, na.rm = TRUE)
  )
franklinton_sf_income_summary$admin <- franklinton_sf_income_summary$NAME
franklinton_sf_income_cleaned <- franklinton_sf_income_summary %>%
  separate(NAME, into = c("CensusTract", "County", "State"), sep = ", ") %>%
  mutate(County = gsub(" County", "", County), CensusTract = gsub("Census Tract ","",CensusTract))


franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned %>%
  select(admin, everything())

franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$County == "Franklin",]
franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$CensusTract == "42" |
                                                                 franklinton_sf_income_cleaned$CensusTract == "43" | 
                                                                 franklinton_sf_income_cleaned$CensusTract == "50",]

tm_shape(franklinton_sf_income_cleaned) +
  tm_polygons("pc_income_t", palette = "viridis",alpha = 0.5,style = "cont", title = "Per capita income") +
  tm_text("pc_income_t", size = 1.5, col = "white", auto.placement = TRUE) + 
  tm_basemap("OpenStreetMap") +
  tm_layout(title = "Per capita income in Franklin (2009)", frame = FALSE)


##
franklinton_sf_income_2019 <- get_acs(
  geography = "tract",
  state = "OH",
  year = 2019,
  survey = "acs5",
  variables = c(
    # unit_b = "B25001_001",
    pc_income_b = "B25001_001"
  ),
  geometry = TRUE  
)

franklinton_sf_income_summary <- franklinton_sf_income_2019 %>%
  group_by(NAME) %>%
  summarize(
    pc_income_t = sum(estimate, na.rm = TRUE)
  )
franklinton_sf_income_summary$admin <- franklinton_sf_income_summary$NAME
franklinton_sf_income_cleaned <- franklinton_sf_income_summary %>%
  separate(NAME, into = c("CensusTract", "County", "State"), sep = ", ") %>%
  mutate(County = gsub(" County", "", County), CensusTract = gsub("Census Tract ","",CensusTract))

franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned %>%
  select(admin, everything())

franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$County == "Franklin",]
franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$CensusTract == "42" |
                                                                 franklinton_sf_income_cleaned$CensusTract == "43" | 
                                                                 franklinton_sf_income_cleaned$CensusTract == "50",]


tm_shape(franklinton_sf_income_cleaned) +
  tm_polygons("pc_income_t", palette = "viridis",alpha = 0.5,style = "cont", title = "Units") +
  tm_text("pc_income_t", size = 1.5, col = "white", auto.placement = TRUE) + 
  tm_basemap("OpenStreetMap") +
  tm_layout(title = "Housing units in Franklin (2019)", frame = FALSE)


##
franklinton_sf_income_2019 <- get_acs(
  geography = "tract",
  state = "OH",
  year = 2009,
  survey = "acs5",
  variables = c(
    # unit_b = "B25001_001",
    pc_income_b = "B02001_002"
  ),
  geometry = TRUE  
)

franklinton_sf_income_summary <- franklinton_sf_income_2019 %>%
  group_by(NAME) %>%
  summarize(
    pc_income_t = sum(estimate, na.rm = TRUE)
  )
franklinton_sf_income_summary$admin <- franklinton_sf_income_summary$NAME
franklinton_sf_income_cleaned <- franklinton_sf_income_summary %>%
  separate(NAME, into = c("CensusTract", "County", "State"), sep = ", ") %>%
  mutate(County = gsub(" County", "", County), CensusTract = gsub("Census Tract ","",CensusTract))

franklinton_sf_pop_2019 <- get_acs(
  geography = "tract",
  state = "OH",
  year = 2009,
  survey = "acs5",
  variables = c(
    # unit_b = "B01003_001",
    pc_income_b = "B02001_001"
  ),
  geometry = TRUE  
)
franklinton_sf_pop_summary <- franklinton_sf_pop_2019 %>%
  group_by(NAME) %>%
  summarize(
    pc_income_t = sum(estimate, na.rm = TRUE)
  )

franklinton_sf_income_cleaned$pc_income_t <- franklinton_sf_income_cleaned$pc_income_t / franklinton_sf_pop_summary$pc_income_t
franklinton_sf_income_cleaned$pc_income_t <- round(franklinton_sf_income_cleaned$pc_income_t,4)
franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned %>%
  select(admin, everything())

franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$County == "Franklin",]
franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$CensusTract == "42" |
                                                                 franklinton_sf_income_cleaned$CensusTract == "43" | 
                                                                 franklinton_sf_income_cleaned$CensusTract == "50",]


tm_shape(franklinton_sf_income_cleaned) +
  tm_polygons("pc_income_t", palette = "viridis",alpha = 0.5,style = "cont", title = "White %") +
  tm_text("pc_income_t", size = 1.5, col = "white", auto.placement = TRUE) + 
  tm_basemap("OpenStreetMap") +
  tm_layout(title = "White % in Franklin (2009)", frame = FALSE)

##
franklinton_sf_income_2019 <- get_acs(
  geography = "tract",
  state = "OH",
  year = 2019,
  survey = "acs5",
  variables = c(
    # unit_b = "B25001_001",
    pc_income_b = "B25075_001"
  ),
  geometry = TRUE  
)

franklinton_sf_income_summary <- franklinton_sf_income_2019 %>%
  group_by(NAME) %>%
  summarize(
    pc_income_t = sum(estimate, na.rm = TRUE)
  )
franklinton_sf_income_summary$admin <- franklinton_sf_income_summary$NAME
franklinton_sf_income_cleaned <- franklinton_sf_income_summary %>%
  separate(NAME, into = c("CensusTract", "County", "State"), sep = ", ") %>%
  mutate(County = gsub(" County", "", County), CensusTract = gsub("Census Tract ","",CensusTract))

franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned %>%
  select(admin, everything())

franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$County == "Franklin",]
franklinton_sf_income_cleaned <- franklinton_sf_income_cleaned[franklinton_sf_income_cleaned$CensusTract == "42" |
                                                                 franklinton_sf_income_cleaned$CensusTract == "43" | 
                                                                 franklinton_sf_income_cleaned$CensusTract == "50",]


tm_shape(franklinton_sf_income_cleaned) +
  tm_polygons("pc_income_t", palette = "viridis",alpha = 0.5,style = "cont", title = "Value") +
  tm_text("pc_income_t", size = 1.5, col = "white", auto.placement = TRUE) + 
  tm_basemap("OpenStreetMap") +
  tm_layout(title = "Housing Value in Franklin (2019)", frame = FALSE)

