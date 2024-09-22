# Install and load necessary libraries
library(sf)
library(dplyr)

setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

# Load the U.S. counties shapefile (download it from the U.S. Census Bureau)
# Replace with the actual path to the shapefile
counties10 <- st_read("../census_shp/gz_2010_us_050_00_20m.shp")
counties20 <- st_read("../census_shp/cb_2020_us_county_20m.shp")

counties10 <- counties10 %>% mutate(ctcode = paste0(STATE, COUNTY))
counties20 <- counties20 %>% mutate(ctcode = paste0(STATEFP, COUNTYFP))

counties10 <- counties10 %>% arrange(ctcode)
counties20 <- counties20 %>% arrange(ctcode)

find_neighbors <- function(census) {
  poly <- census$geometry
  sf_data <- st_sf(ID = c(1:nrow(census)), geometry = st_sfc(poly))
  neigh  <- st_touches(sf_data)
  return(neigh)
}

counties10_neigh <- find_neighbors(counties10)
counties20_neigh <- find_neighbors(counties20)

counties10_code <- counties10 %>%
  select("ctcode")
counties20_code <- counties20 %>%   
  select("ctcode")
