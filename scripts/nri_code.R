library(readr)
library(dplyr)
library(stringr)

nri <- read.csv("C:/Users/twokr/OneDrive - University of Waterloo/AsstProf_SYDE_UWAT/Data/National Risk Index (us)/NRI_Table_CensusTracts.csv")
cf <- nri[,c(11,82)]
rf <- nri[,c(11,316)]

crosswalk <- read.csv("C:/Users/twokr/OneDrive - University of Waterloo/AsstProf_SYDE_UWAT/Data/National Risk Index (us)/nhgis_tr2020_tr2010.csv")

cf2 <- cf %>%
  left_join(crosswalk, by = c("TRACTFIPS" = "tr2020ge")) %>%
  group_by(tr2010ge) %>%
  summarise(
    total_wt      = sum(wt_pop, na.rm = TRUE),
    na_wt         = sum(wt_pop[is.na(CFLD_RISKV)], na.rm = TRUE),
    non_na_wt     = sum(wt_pop[!is.na(CFLD_RISKV)], na.rm = TRUE),
    na_share      = na_wt / total_wt,
    CFLD_RISKV10 = case_when(
      na_share <= 0.10 ~ sum(CFLD_RISKV * wt_pop, na.rm = TRUE) / non_na_wt,  # weighted avg of non-NA only
      TRUE             ~ NA_real_                                               # >10% NA → set to NA
    )
  ) %>%
  select(tract = tr2010ge, CFLD_RISKV10)

rf2 <- rf %>%
  left_join(crosswalk, by = c("TRACTFIPS" = "tr2020ge")) %>%
  group_by(tr2010ge) %>%
  summarise(
    total_wt      = sum(wt_pop, na.rm = TRUE),
    na_wt         = sum(wt_pop[is.na(IFLD_RISKV)], na.rm = TRUE),
    non_na_wt     = sum(wt_pop[!is.na(IFLD_RISKV)], na.rm = TRUE),
    na_share      = na_wt / total_wt,
    IFLD_RISKV10 = case_when(
      na_share <= 0.10 ~ sum(IFLD_RISKV * wt_pop, na.rm = TRUE) / non_na_wt,  # weighted avg of non-NA only
      TRUE             ~ NA_real_                                               # >10% NA → set to NA
    )
  ) %>%
  select(tract = tr2010ge, IFLD_RISKV10)

coastal_counties <- read.csv("C:/Users/twokr/OneDrive - University of Waterloo/AsstProf_SYDE_UWAT/Data/National Risk Index (us)/coastal_counties.csv")  
coastal_counties$coastal <- 1
coastal_counties <- coastal_counties[,c(1,5)]
coastal_counties <- coastal_counties %>%
  mutate(FIPS = str_pad(as.character(FIPS), 5, pad = "0")) %>%
  distinct(FIPS, .keep_all = TRUE)

cf2 <- cf2 %>%
  mutate(
    tract_str = str_pad(as.character(tract), 11, pad = "0"),
    FIPS = substr(tract_str, 1, 5)
  ) %>%
  left_join(coastal_counties, by = "FIPS") %>%
  select(-c(tract, FIPS)) %>%
  rename(tract = "tract_str")

rf2 <- rf2 %>%
  mutate(
    tract_str = str_pad(as.character(tract), 11, pad = "0"),
    FIPS = substr(tract_str, 1, 5)
  ) %>%
  select(-c(tract, FIPS)) %>%
  rename(tract = "tract_str")


