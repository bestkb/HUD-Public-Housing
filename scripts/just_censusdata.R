

library(tidyverse)
library(readxl)
library(tidycensus)
library(tigris)

######## next step is to pull in block group census data from ACS ########

insp_w_census <- read_csv("data/locations_inspectionscores_forMeri_Feb.csv") 

locations_only <- insp_w_census %>%
  select(c(1, 16, 17)) %>%
  mutate(tract = substr(as.character(block_group), 1, 11))%>%
  unique()


census_api_key("ff5d487d0a2a22c658bf319ba136c27db32aa0be", install = TRUE, 
               overwrite = TRUE)

blockgroups <- unique(na.omit(insp_w_census$block_group))
counties <- unique(na.omit(insp_w_census$county))
states <- unique(na.omit(insp_w_census$STATE_CODE))
insp_years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)



# block groups not available for the 2008-2012 ACS and earlier
hold_data <- data.frame()

for (i in insp_years){
  
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
                                                   total_pop = "B01003_001"), 
                                     state = c(unique(states)), 
                                     year = i)
  
  other_census_wider <- other_census_blockgroup%>% pivot_wider(id_cols = "GEOID",
                                                               names_from = "variable",
                                                               values_from = "estimate")
  other_census_wider <- other_census_wider %>%
    mutate(year = i)
  
  hold_data <- hold_data %>% rbind(other_census_wider)
  
}



### pull county data ### 
############### just county ################3
hold_data_c <- data.frame()

for (i in insp_years){
  
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
                                                   total_pop_c = "B01003_001"),
                                     county = c(as.numeric(unique(counties))),
                                     year = i)
  
  other_census_wider <- other_census_blockgroup%>% pivot_wider(id_cols = "GEOID",
                                                               names_from = "variable",
                                                               values_from = "estimate")
  other_census_wider <- other_census_wider %>%
    mutate(year = i)
  
  hold_data_c <- hold_data_c %>% rbind(other_census_wider)
  
}


all_county_demographics <- locations_only %>%
  left_join(hold_data_c, by = c("county" = "GEOID"))


#write_csv(all_county_demographics, "data/county_demographics_forMeri_Feb.csv")




### pull tract data ### 
############### just county ################3
hold_data_t <- data.frame()

for (i in insp_years){
  
  other_census_blockgroup <- get_acs(survey = "acs5", geography = "tract", 
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
                                     state = c(unique(states)), 
                                     year = i)
  
  other_census_wider <- other_census_blockgroup%>% pivot_wider(id_cols = "GEOID",
                                                               names_from = "variable",
                                                               values_from = "estimate")
  other_census_wider <- other_census_wider %>%
    mutate(year = i)
  
  hold_data_t <- hold_data_t %>% rbind(other_census_wider)
  
}


all_tract_demographics <- locations_only %>%
  left_join(hold_data_t, by = c("tract" = "GEOID"))


#write_csv(all_tract_demographics, "data/tract_demographics_forMeri_Feb.csv")


