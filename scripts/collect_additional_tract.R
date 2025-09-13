library(tidyverse)
library(tidycensus)
library(dplyr)
library(plm)
library(readxl)

data(fips_codes)
state_codes <- unique(fips_codes$state)
state_codes <- state_codes[1:51]
state_full_names <- state.name[match(state_codes, state.abb)]
state_full_names[9] <- "District of Columbia"

new_block_dat <- data.frame()
for (i in 2013:2019){
  for (j in 1:51){
    x <- get_acs(geography = "block group",
                 variables = c(poverty_white_b = "B17001H_001",
                               poverty_black_b =  "B17001B_001",
                               poverty_hispanic_b =  "B17001I_001",
                               income_white_b = "B19001H_001",
                               income_black_b =  "B19001B_001",
                               income_hispanic_b =  "B19001I_001",
                               tenure_by_income_b = "B25118_001"
                               ),
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
    new_block_dat <- rbind(new_block_dat,x2)
  }
}

new_tract_dat <- data.frame()
for (i in 2013:2019){
  for (j in 1:51){
    x <- get_acs(geography = "tract",
                 variables = c(
                               age_t = "B01002_001",
                               tot_poverty_t =  "B17010_001",
                               poverty_black_t =  "B17001B_002",
                               poverty_hispanic_t =  "B17001I_002",
                               total_pop_t = "B01003_001",
                               total_white_t = "B03002_003"
                 ),
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
    new_tract_dat <- rbind(new_tract_dat,x2)
  }
}

new_tract_dat2 <- new_tract_dat[,-2]

dat <- read_csv("data/inspection_scores_w_flood_110824.csv")
dat <- dat[,2:71]
dat <- dat %>%
  mutate(tract = ifelse(nchar(tract) == 10, 
                    paste0("0", tract), 
                    as.character(tract))) 

dat2 <- dat %>%
  left_join(new_tract_dat2,by = c("year","tract" = "GEOID"))

dat2[is.na(dat2$poverty_black_t),c(71:77)] <- new_tract_dat2[c(481366,481365),c(2:8)]

write.csv(dat2,"data/inspection_scores_w_flood_010925.csv", row.names = FALSE)

new_block_dat <- data.frame()
for (i in 2013:2019){
  for (j in 1:51){
    x <- get_acs(geography = "block group",
                 variables = c(
                   poverty_b = "B17010_002",
                   pop_b = "B01003_001"
                 ),
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
    new_block_dat <- rbind(new_block_dat,x2)
  }
}

