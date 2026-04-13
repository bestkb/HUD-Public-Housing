library(tidycensus)
library(tidyr)
library(dplyr)
library(tigris)
library(sf)
library(plm)

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

options(tigris_use_cache = TRUE)   # cache downloads

all_states <- unique(fips_codes$state)[1:51]   # 50 states + DC

bg_all <- do.call(rbind, lapply(all_states, function(s) {
  tryCatch(block_groups(state = s, year = 2016, cb = TRUE), error = function(e) NULL)
}))

bg_all <- st_transform(bg_all, crs = 4326)


##
inspection_scores_uniq <- read.csv("data/inspection_scores_w_flood_033125.csv",header=TRUE)[,1:16]
inspection_scores_uniq$LONGITUDE2 <- inspection_scores_uniq$LONGITUDE
inspection_scores_uniq$LATITUDE2  <- inspection_scores_uniq$LATITUDE

points_sf <- st_as_sf(inspection_scores_uniq, coords = c("LONGITUDE2", "LATITUDE2"), crs = 4326)
result <- st_join(points_sf, bg_all, join = st_within)

result[, c("AFFGEOID", "NAME", "LSAD")] <- NULL
names(result)[names(result) == "GEOID"] <- "blockgroup"
names(result)[names(result) == "ALAND"] <- "ALAND_b"
names(result)[names(result) == "AWATER"] <- "AWATER_b"

##
tr_all <- do.call(rbind, lapply(all_states, function(s) {
  tryCatch(tracts(state = s, year = 2016, cb = TRUE), error = function(e) NULL)
}))
tr_all <- st_transform(tr_all, crs = 4326)

# Spatial join
result_tr <- st_join(points_sf, tr_all, join = st_within)
result_tr[, c("AFFGEOID", "NAME", "LSAD")] <- NULL
names(result_tr)[names(result_tr) == "GEOID"] <- "tract"
names(result_tr)[names(result_tr) == "ALAND"]  <- "ALAND_tr"
names(result_tr)[names(result_tr) == "AWATER"] <- "AWATER_tr"


##
result$tract <- result_tr$tract
result$ALAND_tr <- result_tr$ALAND_tr
result$AWATER_tr <- result_tr$AWATER_tr

##
census_api_key("8c8fe7ff5198b2b97811f1f66873724c02e8630b", install = TRUE,
               overwrite = TRUE)

vars  <- c(
  age_b              = "B01002_001",
  income_b           = "B19013_001",
  income_pc_b        = "B19301_001",
  prop_value_b       = "B25077_001",
  public_assistance_b = "B19057_001",
  poverty_pop_b      = "B17021_001",
  poverty_b          = "B17021_002",
  vehicles_b         = "B25044_001",
  renter_b           = "B25003_003",
  owner_b            = "B25003_002",
  total_white_b      = "B03002_003",
  total_black_b      = "B03002_004",
  total_hispanic_b   = "B03002_012",
  total_pop_b        = "B01003_001"
)

unique_years <- unique(result$year)
unique_states <- unique(result$STATEFP)   # pull only states present in your data

acs_bg <- lapply(unique_years, function(yr) {
  lapply(unique_states, function(st) {
    tryCatch(
      get_acs(
        geography = "block group",
        variables = vars,
        state     = st,
        year      = yr,
        survey    = "acs5",
        output    = "wide"
      ) %>% mutate(year = yr),
      error = function(e) {
        message("Skipping state ", st, " year ", yr, ": ", e$message)
        NULL
      }
    )
  }) %>% bind_rows()
}) %>% bind_rows()

acs_bg <- acs_bg %>%
  select(GEOID, year, NAME, ends_with("E")) %>%
  rename_with(~ sub("E$", "", .x), ends_with("E"))


result <- result %>%
  st_drop_geometry() %>%
  left_join(acs_bg, by = c("blockgroup" = "GEOID", "year" = "year"))

elderly_vars <- c(
  # Male 65+
  "B01001_020",  # 65-66
  "B01001_021",  # 67-69
  "B01001_022",  # 70-74
  "B01001_023",  # 75-79
  "B01001_024",  # 80-84
  "B01001_025",  # 85+
  # Female 65+
  "B01001_044",  # 65-66
  "B01001_045",  # 67-69
  "B01001_046",  # 70-74
  "B01001_047",  # 75-79
  "B01001_048",  # 80-84
  "B01001_049"   # 85+
)

acs_elderly <- lapply(unique_years, function(yr) {
  lapply(unique_states, function(st) {
    tryCatch(
      get_acs(
        geography = "block group",
        variables = elderly_vars,
        state     = st,
        year      = yr,
        survey    = "acs5",
        output    = "wide"
      ) %>% mutate(year = yr),
      error = function(e) NULL
    )
  }) %>% bind_rows()
}) %>% bind_rows()

acs_elderly <- acs_elderly %>%
  mutate(
    elderly_count_b = rowSums(select(., ends_with("E") & 
                                       starts_with("B01001")), na.rm = TRUE)) %>%
  select(GEOID, year, elderly_count_b)

result <- result %>%
  left_join(acs_elderly, by = c("blockgroup" = "GEOID", "year" = "year"))

tract_vars <- c(
  total_white_t    = "B03002_003",
  total_black_t    = "B03002_004",
  total_hispanic_t = "B03002_012",
  total_pop_t      = "B01003_001"
)

acs_tract <- lapply(unique_years, function(yr) {
  lapply(unique_states, function(st) {
    tryCatch(
      get_acs(
        geography = "tract",
        variables = tract_vars,
        state     = st,
        year      = yr,
        survey    = "acs5",
        output    = "wide"
      ) %>% mutate(year = yr),
      error = function(e) {
        message("Skipping state ", st, " year ", yr, ": ", e$message)
        NULL
      }
    )
  }) %>% bind_rows()
}) %>% bind_rows()

# Clean up
acs_tract <- acs_tract %>%
  select(GEOID, year, ends_with("E")) %>%
  rename_with(~ sub("E$", "", .x), ends_with("E"))

result <- result %>%
  left_join(acs_tract, by = c("tract" = "GEOID", "year" = "year"))

poc_poverty_vars <- c(
  total_poverty_t    = "B17001_002",   # all people below poverty
  white_nh_poverty_t = "B17001H_002",  # white non-Hispanic below poverty
  black_poverty_t    = "B17001B_002",  # Black
  hispanic_poverty_t = "B17001I_002"  # Hispanic
)

acs_poc_poverty <- lapply(unique_years, function(yr) {
  lapply(unique_states, function(st) {
    tryCatch(
      get_acs(
        geography = "tract",
        variables = poc_poverty_vars,   # <-- used here
        state     = st,
        year      = yr,
        survey    = "acs5",
        output    = "wide"
      ) %>% mutate(year = yr),
      error = function(e) {
        message("Skipping state ", st, " year ", yr, ": ", e$message)
        NULL
      }
    )
  }) %>% bind_rows()
}) %>% bind_rows()

acs_poc_poverty <- acs_poc_poverty %>%
  select(GEOID, year, ends_with("E")) %>%
  rename_with(~ sub("E$", "", .x), ends_with("E")) %>%
  mutate(
    poc_poverty_t  = total_poverty_t - white_nh_poverty_t,
    poc_poverty_t2 = black_poverty_t + hispanic_poverty_t,
  )

result <- result %>%
  left_join(acs_poc_poverty, by = c("tract" = "GEOID", "year" = "year"))


poverty_vars <- c(
  poverty_pop_t = "B17001_001",  # total families
  poverty_t     = "B17001_002"   # families below poverty
   
)

acs_poverty <- lapply(unique_years, function(yr) {
  lapply(unique_states, function(st) {
    tryCatch(
      get_acs(
        geography = "tract",
        variables = poverty_vars,
        state     = st,
        year      = yr,
        survey    = "acs5",
        output    = "wide"
      ) %>% mutate(year = yr),
      error = function(e) {
        message("Skipping state ", st, " year ", yr, ": ", e$message)
        NULL
      }
    )
  }) %>% bind_rows()
}) %>% bind_rows()

acs_poverty <- acs_poverty %>%
  select(GEOID, year, ends_with("E")) %>%
  rename_with(~ sub("E$", "", .x), ends_with("E")) %>%
  mutate(
    poverty_pct_t     = poverty_t / poverty_pop_t * 100,
  )

result <- result %>%
  left_join(acs_poverty, by = c("tract" = "GEOID", "year" = "year"))

result <- result[!is.na(result[,19]),]
result$pct_white <- result$total_white_b / result$total_pop_b * 100
result$pct_poor <- result$poverty_pct_t
result$pct_poor_poc1 <- result$poc_poverty_t / result$total_pop_t * 100
result$pct_poor_poc2 <- result$poc_poverty_t2 / result$total_pop_t * 100
result$pct_elderly <- result$elderly_count_b / result$total_pop_b * 100
result$log_income_pc <- log10(result$income_pc_b)
result$pct_renter <- result$renter_b / (result$renter_b + result$owner_b) * 100
result$pop_density <- result$total_pop_b / result$ALAND_b
  

dat <- result[,c("DEVELOPMENT_ID","STATE_NAME","INSPECTION_SCORE","LONGITUDE","LATITUDE","year","in_floodplain",
                 "STATEFP","COUNTYFP","TRACTCE","BLKGRPCE","blockgroup","ALAND_b",
                 "AWATER_b","tract","ALAND_tr","AWATER_tr","pct_poor_poc1",
                 "pct_poor_poc2","pct_white","pct_poor","pct_elderly","log_income_pc",
                 "pct_renter","pop_density")]

dat <- dat %>%
  filter(complete.cases(.))

# use nri_code.R to get rf2 and cf2
dat <- dat %>%
  left_join(cf2,by = "tract") %>%
  left_join(rf2,by = "tract")

dat$CFLD_RISKS <- rank(dat$CFLD_RISKV10) / sum(!is.na(dat$CFLD_RISKV10)) * 100
dat[is.na(dat$CFLD_RISKV10),"CFLD_RISKS"] <- 0
dat[which(dat$CFLD_RISKV10 == 0),"CFLD_RISKS"] <- 0

dat$IFLD_RISKS <- rank(dat$IFLD_RISKV10) / sum(!is.na(dat$IFLD_RISKV10)) * 100
dat[is.na(dat$IFLD_RISKV10),"IFLD_RISKS"] <- 0
dat[which(dat$IFLD_RISKV10 == 0),"IFLD_RISKS"] <- 0
dat$STCO <- paste0(dat$STATEFP, dat$COUNTYFP)

write.csv(dat, "data_rev2.csv") 
save.image("data_rev.RData")

model_vars <- c("INSPECTION_SCORE", "in_floodplain", "CFLD_RISKS", "IFLD_RISKS",
                "pct_white", "pct_poor", "pct_elderly", "log_income_pc",
                "pct_renter", "pop_density", "DEVELOPMENT_ID", "year","pct_poor_poc1","pct_poor_poc2","STCO","tract","blockgroup")

dat2 <- dat %>%
  select(all_of(model_vars)) %>%
  # Average inspection score if multiple inspections in same year
  group_by(DEVELOPMENT_ID, year) %>%
  summarise(
    INSPECTION_SCORE = mean(INSPECTION_SCORE, na.rm = TRUE),
    across(c(in_floodplain, CFLD_RISKS, IFLD_RISKS,
             pct_white, pct_poor, pct_elderly, log_income_pc,
             pct_renter, pop_density,
             blockgroup, STCO),
           ~ first(.x)),
    .groups = "drop"
  ) %>%
  filter(complete.cases(.)) %>%
  mutate(
    DEVELOPMENT_ID = as.factor(DEVELOPMENT_ID),
    blockgroup     = as.factor(blockgroup),
    STCO           = as.factor(STCO),
    year           = as.integer(year)
  )

devs_keep <- dat2 %>%
  group_by(DEVELOPMENT_ID) %>%
  summarise(n_years = n_distinct(year)) %>%
  filter(n_years >= 2) %>%
  pull(DEVELOPMENT_ID)

dat3 <- dat2 %>% filter(DEVELOPMENT_ID %in% devs_keep)
pdat <- pdata.frame(dat3, index = c("DEVELOPMENT_ID", "year"))


#
m1 <- plm(INSPECTION_SCORE ~ in_floodplain + CFLD_RISKS + IFLD_RISKS +
            pct_white + pct_poor + pct_elderly + log_income_pc +
            pct_renter + pop_density +
            factor(year) +                # μ_t — year fixed effects
            factor(STCO),                 # μ_c — county fixed effects
          data  = pdat,
          model = "random")
summary(m1)

