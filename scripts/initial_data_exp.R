library(tidyverse)
library(readxl)
library(tidycensus)

hud_data_2019 <- read_xlsx("data/public-housing-physical-inspection-scores-2019.xlsx")


## we have city, county, ZIP, inspection score, inspection date (2013-2019)
## lat and long
## location quality 

hud_data_2019 %>%
  ggplot()+
  geom_histogram(aes(x = INSPECTION_SCORE), alpha = 0.7, color = "black")+
  labs(x= "Inspection Score", y = "Count")+
  theme_bw()


#let's map this with flood hazard layer 
## distance to nearest flood plane ###
## block group 

## pull in 2021 data 
hud_data <- read_xlsx("data/public_housing_physical_inspection_scores_0321.xlsx")


## we have city, county, ZIP, inspection score, inspection date (2013-2019)
## lat and long
## location quality 

hud_data %>%
  ggplot()+
  geom_histogram(aes(x = INSPECTION_SCORE), alpha = 0.7, fill = "blue")+
  geom_histogram(data = hud_data_2019, aes(x = INSPECTION_SCORE), alpha = 0.7, color = "red")+
  labs(x= "Inspection Score", y = "Count")+
  theme_bw()



# scores ~ distance to flood plane + socioeconomic data 
  #possible binary of in flood plane or not
  #disaster events (amount of damage or number PDD for example)
# weigh scores by flood distance 


hud_w_flood_raw <- read_csv("data/2pub_housing.csv") %>%
  select(c(DEVELOPMEN, NEAR_DIST))


hud_w_flood <- hud_data %>% left_join(hud_w_flood_raw, 
                                      by= c("DEVELOPMENT_ID" = "DEVELOPMEN")) 


hud_w_flood <- hud_w_flood %>%
  mutate(in_floodplain = ifelse(NEAR_DIST == 0, 1, 0))


############# try some regressions ######################################

lm_simple <- lm(INSPECTION_SCORE ~ NEAR_DIST, data = hud_w_flood)
summary(lm_simple)


############# pull in socioeconomic data ###############################






