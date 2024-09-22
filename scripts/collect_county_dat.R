library(tidycensus)

# API for Woi
census_api_key("8c8fe7ff5198b2b97811f1f66873724c02e8630b", install = TRUE,
               overwrite = TRUE)

v10 <- load_variables(2010, "acs5", cache = TRUE)
View(v10)

county_dat <- data.frame()
for (i in 2010:2020){
  x <- get_acs(geography = "county", 
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
                                           total_pop_c = "B01003_001"),
                             year = i, 
                             survey = "acs5")
  
  x2 <- x %>%
    select(-moe) %>%  # Remove the `moe` column
    pivot_wider(
      names_from = variable,  # Columns will be created based on the `variable` column
      values_from = estimate  # Values will come from `estimate` and `moe`
    )
  
  x2$year <- i
  county_dat <- rbind(county_dat,x2)
}


