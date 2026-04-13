library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(tigris) 
library(usmap)
library(ggjoy)
library(ggtext)
library(ggdist)
library(glue)
library(patchwork)
library(ggbeeswarm)
library(tidycensus)
library(tidyr)
library(plm)
library(readxl)

setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))
# options(scipen = 999)

inspection_score_uniq <- read.csv("data/inspection_scores_w_flood_033125.csv",header=TRUE)

us_mainland <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Hawaii", "Alaska"))

phd <- readr::read_csv("data/Public_Housing_Developments.csv")
phd$PEOPLE_TOTAL[phd$PCT_OCCUPIED == 0] <- 0
phd$PEOPLE_TOTAL[phd$PEOPLE_TOTAL < 0] <- NA

mean_score <- mean(inspection_score_uniq$INSPECTION_SCORE, na.rm = TRUE)
median_score <- median(inspection_score_uniq$INSPECTION_SCORE, na.rm = TRUE)
n_score <- nrow(subset(inspection_score_uniq, !is.na(title)))

bg_color <- "grey97"

# Plot yearly distribution of inspection scores from 2013 to 2019
p <- inspection_score_uniq %>% 
  ggplot(aes(year, INSPECTION_SCORE)) +
  stat_halfeye(fill_type = "segments", alpha = 0.3) +
  stat_interval() +
  stat_summary(geom = "point", fun = median) +
  geom_hline(yintercept = median_score, col = "grey30", lty = "dashed") +
  scale_x_reverse(breaks = sort(unique(inspection_score_uniq$year)),
                     labels = sort(unique(inspection_score_uniq$year)))+
  scale_y_reverse(breaks = c(0,30,60,90,100),labels = c(0,30,60,90,100)) + 
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  coord_flip(ylim = c(0, 100), clip = "off") +
  guides(col = "none") +
  labs(
    x = "Year",
    y = "Inspection score"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = NA, fill = "white"), 
    panel.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1, color = "grey75"),
    plot.title = element_text(),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 4, b = 0), size = 10),
    plot.caption = element_textbox_simple(
      margin = margin(t = 12), size = 7
    ),
    plot.caption.position = "plot",
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16,"black"),        
    axis.title.y = element_text(size = 20, margin = margin(r = 10)),   
    axis.title.x = element_text(size = 20, margin = margin(t = 10)),    
    plot.margin = margin(4, 4, 4, 4)
  )
p
ggsave("yearly_score.png",p)

# get inspection scores at the county level
dat <- inspection_score_uniq %>%
  mutate(
    state_formatted = sprintf("%02d", as.integer(STATE_CODE)),
    county_formatted = sprintf("%03d", as.integer(COUNTY_CODE)),
    county = paste0(state_formatted, county_formatted)
  )


county_units <- dat %>%
  group_by(county) %>%    
  summarise(uniq_count = n_distinct(paste(LATITUDE, LONGITUDE)),
            all_count = n())
county_units$fips <- county_units$county
county_insp <- dat %>%
  group_by(county) %>%
  summarize(score = mean(INSPECTION_SCORE, na.rm = TRUE),
            bad = sum(INSPECTION_SCORE < 60, na.rm = TRUE))
county_insp$fips <- county_insp$county

load("county_geom.RData")
us_counties_units <- us_counties %>%
  left_join(county_units, by = c("GEOID" = "county"))

county_dev <- phd %>%
  group_by(COUNTY_LEVEL) %>%      
  summarise(dev_count = n_distinct(paste(LAT, LON)),
            unit_count = sum(TOTAL_UNITS, na.rm = TRUE),
            resi_count = sum(PEOPLE_TOTAL, na.rm = TRUE))
county_dev$fips <- county_dev$COUNTY_LEVEL
county_dev$log_unit_count <- log10(county_dev$unit_count)
county_dev$log_resi_count <- log10(county_dev$resi_count)

df <- usmap_transform(inspection_score_uniq,input_names = c("LONGITUDE","LATITUDE"))
df <- st_as_sf(df)
df <- df %>%
  mutate(
    LONGITUDE = st_coordinates(geometry)[, 1],
    LATITUDE = st_coordinates(geometry)[, 2] 
  )

# plot a map of mean inspection score
map_score <- plot_usmap(
  regions = "counties",
  data = county_insp,
  values = "score",
  color = "white",
  size = 0.0001
) +
  scale_fill_gradientn(
    colors = c("firebrick4","red", "yellow", "darkgreen"),
    values = scales::rescale(c(0, 40, 80, 90, 100)),
    limits = c(0, 100),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5, 
      barwidth = 10,
      barheight = 1.5 
    )
  ) +
  labs(
    fill = "Inspection score"
  ) +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    legend.position = c(0.8, 0.1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)
  )
ggsave("map_score.png", map_score, width = 10,height = 6,dpi = 250)

# plot a map of inspected unit #
map_unit<- plot_usmap(
  regions = "counties",
  data = county_units,
  values = "uniq_count",
  color = "white",
  size = 0.0001
) +
  scale_fill_gradientn(
    colors = c("red", "yellow", "darkgreen"), 
    trans = "log",  
    breaks = c(1, 4, 16, 64, 256), 
    labels = c("1", "4", "16", "64", "256"), 
    limits = c(1, 256),
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,  
      barwidth = 10,       
      barheight = 1.5    
    )
  ) +
  labs(
    fill = "Inspected unit #"
  ) +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.8, 0.1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)  
  )
ggsave("map_unit2.png", map_unit, width = 10,height = 6,dpi = 250)

# plot a map of total public housing units per county
map_totunit2 <- plot_usmap(
  regions = "counties",
  data = county_dev,
  values = "log_unit_count",
  color = "white",
  size = 0.00001
) +
  scale_fill_gradientn(
    colors = c("firebrick4","red", "yellow", "green", "darkgreen"),
    values = scales::rescale(c(0, 1, 2, 3, 4, 5)),
    limits = c(0, 5),
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0.5, 
      barwidth = 10,  
      barheight = 1.5  
    )
  ) +
  labs(
    fill = "Public housing units"
  ) +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),      
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    legend.position = c(0.8, 0.1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 10, angle = -45, hjust = 0)  
  )
ggsave("map_tot_units_22.png", map_totunit2, width = 10,height = 6,dpi = 250)


##
# load tract-level and block-group-level data of median age, total pop, and total white pop, and year
tract_dat1 <- read.csv("tract_dat2.csv") # not included in the github repo due to size
blockg_dat <- read.csv("block_dat.csv") # not included in the github repo due to size

# get national-level data
median_tract_dat <- data.frame()
for (i in 2013:2019){
    x <- get_acs(geography = "us",
                 variables = c(
                   poverty_black_t_median =  "B17001B_002",
                   poverty_hispanic_t_median =  "B17001I_002",
                   total_pop_t_median = "B01003_001",
                   male_65_66 = "B01001_020",
                   male_67_69 = "B01001_021",
                   male_70_74 = "B01001_022",
                   male_75_79 = "B01001_023",
                   male_80_84 = "B01001_024",
                   male_85_plus = "B01001_025",
                   female_65_66 = "B01001_044",
                   female_67_69 = "B01001_045",
                   female_70_74 = "B01001_046",
                   female_75_79 = "B01001_047",
                   female_80_84 = "B01001_048",
                   female_85_plus = "B01001_049",
                   total_pop_b = "B01003_001",
                   total_white_b = "B03002_003"
                   
                 ),
                 year = i,
                 survey = "acs5",
                 geometry = FALSE)
    
    x2 <- x %>%
      select(-moe) %>% 
      pivot_wider(
        names_from = variable, 
        values_from = estimate
      )
    
    x2$year <- i
    median_tract_dat <- rbind(median_tract_dat,x2)
}

median_tract_dat$poverty_color_t_median <- (median_tract_dat$poverty_black_t_median + median_tract_dat$poverty_hispanic_t_median)/median_tract_dat$total_pop_t_median
median_tract_dat$pct_white_t_median <- median_tract_dat$total_white_b / median_tract_dat$total_pop_t_median
median_tract_dat$pct_elderly_median <- (median_tract_dat$male_65_66 + median_tract_dat$male_67_69 + median_tract_dat$male_70_74 +
  median_tract_dat$male_75_79 + median_tract_dat$male_80_84 + median_tract_dat$male_85_plus +
  median_tract_dat$female_65_66 + median_tract_dat$female_67_69 + median_tract_dat$female_70_74 +
  median_tract_dat$female_75_79 + median_tract_dat$female_80_84 + median_tract_dat$female_85_plus) / median_tract_dat$total_pop_t_median

tract_dat1$pov_color <- (tract_dat1$poverty_black_t + tract_dat1$poverty_hispanic_t)/tract_dat1$total_pop_t
tract_dat1$pct_white <- tract_dat1$total_white_t / tract_dat1$total_pop_t
tract_dat1$GEOID <- as.numeric(tract_dat$GEOID)
tract_dat1 <- tract_dat[,c(1,8,10)]

blockg_dat$pct_white <- blockg_dat$total_white_b / blockg_dat$total_pop_b
blockg_dat <- blockg_dat[,c(1,6,7)]
blockg_dat$GEOID <- as.numeric(blockg_dat$GEOID)


datt <- read_excel("./data/sample_dataset.xlsx")
datt4 <- datt %>%
  left_join(datt2[,c(1,85,86)], by = c("v1" = "X"))
datt <- datt[datt$inspection_score > 0 ,]
datt$pov_color <- (datt$poverty_black_t2 + datt$poverty_hispanic_t2)/datt$total_pop_t
datt$pct_white <- datt$total_white_b / datt$total_pop_b
inspection_score_uniq2 <- datt

dat_goodq <- inspection_score_uniq2[inspection_score_uniq2$inspection_score >= 60,]
dat_badq <- inspection_score_uniq2[inspection_score_uniq2$inspection_score < 60,]

dat_badq_fl <- dat_badq[dat_badq$in_floodplain == 1,]
dat_badq_notfl <- dat_badq[dat_badq$in_floodplain == 0,]

dat_badq_fl <- dat_badq_fl %>%
  left_join(median_tract_dat,by = "year") %>%
  mutate(
    age_t_bad = ifelse(elderly_pct > pct_elderly_median, 1, 0),
    poverty_t_bad = ifelse(pct_poor_color > poverty_color_t_median, 1, 0),
    pct_white_bad = ifelse(pct_white < pct_white_t_median, 1, 0),
  )

dat_goodq <- usmap_transform(dat_goodq,input_names = c("longitude","latitude"))
df_coords <- st_coordinates(dat_goodq)
dat_goodq <- cbind(dat_goodq, df_coords) 
colnames(dat_goodq)[ncol(dat_goodq)-2] <- "LONGITUDE" 
colnames(dat_goodq)[ncol(dat_goodq)-1] <- "LATITUDE" 

dat_badq_notfl <- usmap_transform(dat_badq_notfl,input_names = c("longitude","latitude"))
df_coords <- st_coordinates(dat_badq_notfl)
dat_badq_notfl <- cbind(dat_badq_notfl, df_coords) 
colnames(dat_badq_notfl)[ncol(dat_badq_notfl)-2] <- "LONGITUDE"
colnames(dat_badq_notfl)[ncol(dat_badq_notfl)-1] <- "LATITUDE"

df_map <- usmap_transform(dat_badq_fl,input_names = c("longitude","latitude"))
df_coords <- st_coordinates(df_map)
df_map <- cbind(df_map, df_coords)
colnames(df_map)[ncol(df_map)-2] <- "LONGITUDE"
colnames(df_map)[ncol(df_map)-1] <- "LATITUDE" 

df_badq_fl_age <- df_map[df_map$age_t_bad == 1,]
df_badq_fl_pov <- df_map[df_map$poverty_t_bad == 1,]
df_badq_fl_white <- df_map[df_map$pct_white_bad == 1,]
df_badq_fl_notdis <- df_map[df_map$age_t_bad == 0 & df_map$poverty_t_bad == 0 & df_map$pct_white_bad == 0,]

unique_rows <- bind_rows(
  df_badq_fl_age,
  df_badq_fl_pov,
  df_badq_fl_white,
) %>%
  distinct() 

unique_rows <- unique_rows %>%
  mutate(combination = factor(paste(age_t_bad, poverty_t_bad, pct_white_bad, sep = "_"),
                              levels = c("1_1_0", "1_0_0","1_0_1", "1_1_1", "0_1_0", "0_1_1", "0_0_1","0_0_0")))

df_count <- unique_rows %>%
  group_by(LATITUDE, LONGITUDE, development_id,development_name, year, inspection_score,combination) %>%  
  summarize(count = n(), .groups = "drop") 

p_age <- plot_usmap(
  regions = "states",
  color = "black",
  size = 0.1
) +
  geom_point(data = dat_goodq, aes(x = LONGITUDE, y = LATITUDE),color = "lightgray", size = 0.5, shape = 16, alpha = 1) +
  geom_point(data = dat_badq_notfl, aes(x = LONGITUDE, y = LATITUDE),color = "blue", size = 1, shape = 16, alpha = 1) +
  # geom_point(data = df_count, aes(x = LONGITUDE, y = LATITUDE, color = combination, size = count+5), shape = 16, alpha = 0.7) +
  geom_point(data = df_badq_fl_notdis, aes(x = LONGITUDE, y = LATITUDE),color = "green", size = 3, shape = 16, alpha = 1) +
  geom_point(data = df_badq_fl_age, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 3, shape = 16, alpha = 1,fill = "red") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
  )

p_pov <- plot_usmap(
  regions = "states",
  color = "black",
  size = 0.1
) +
  geom_point(data = dat_goodq, aes(x = LONGITUDE, y = LATITUDE),color = "lightgray", size = 0.5, shape = 16, alpha = 1) +
  geom_point(data = dat_badq_notfl, aes(x = LONGITUDE, y = LATITUDE),color = "blue", size = 1, shape = 16, alpha = 1) +
  # geom_point(data = df_count, aes(x = LONGITUDE, y = LATITUDE, color = combination, size = count+5), shape = 16, alpha = 0.7) +
  geom_point(data = df_badq_fl_notdis, aes(x = LONGITUDE, y = LATITUDE),color = "green", size = 3, shape = 16, alpha = 1) +
  geom_point(data = df_badq_fl_pov, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 3, shape = 25, alpha = 1,fill = "red") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),
    legend.position = "none",
  )

p_white <- plot_usmap(
  regions = "states",
  color = "black",
  size = 0.1
) +
  geom_point(data = dat_goodq, aes(x = LONGITUDE, y = LATITUDE),color = "lightgray", size = 0.5, shape = 16, alpha = 1) +
  geom_point(data = dat_badq_notfl, aes(x = LONGITUDE, y = LATITUDE),color = "blue", size = 1, shape = 16, alpha = 1) +
  # geom_point(data = df_count, aes(x = LONGITUDE, y = LATITUDE, color = combination, size = count+5), shape = 16, alpha = 0.7) +
  geom_point(data = df_badq_fl_notdis, aes(x = LONGITUDE, y = LATITUDE),color = "green", size = 3, shape = 16, alpha = 1) +
  geom_point(data = df_badq_fl_white, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 3, shape = 24, alpha = 1, fill = "red") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),
    legend.position = "none",
  )

png("map_age_rev2.png", width = 3000, height = 2000,res = 300)  
p_age
dev.off()  

png("map_pov_rev2.png", width = 3000, height = 2000,res = 300)  
p_pov
dev.off()  

png("map_white_rev2.png", width = 3000, height = 2000,res = 300)  
p_white
dev.off()  


p_nj <- plot_usmap(
  regions = "states",
  color = "black",
  size = 0.1
) +
  geom_sf(data = shapefile_combined, fill = "lightblue", color = NA) +
  geom_point(data = dat_goodq, aes(x = LONGITUDE, y = LATITUDE),color = "lightgray", size = 0.5, shape = 16, alpha = 1) +
  geom_point(data = dat_badq_notfl, aes(x = LONGITUDE, y = LATITUDE),color = "blue", size = 1, shape = 16, alpha = 1) +
  geom_point(data = df_badq_fl_age, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 3, shape = 24, alpha = 1,fill = "red") +
  geom_point(data = df_badq_fl_pov, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 3, shape = 25, alpha = 1,fill = "red") +
  geom_point(data = df_badq_fl_white, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 3, shape = 16, alpha = 1,fill = "red") +
  theme_minimal()+
  coord_sf(xlim = c(2001684, 2203928), ylim = c(-354899.78, -50000), expand = FALSE) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),
    legend.position = "none",
  )

inspection_score_uniq3 <- inspection_score_uniq2 %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  mutate(LONGITUDE = st_coordinates(.)[,1],
         LATITUDE = st_coordinates(.)[,2])
dat_goodq2 <- inspection_score_uniq3[inspection_score_uniq3$INSPECTION_SCORE >= 60,]
dat_badq2 <- inspection_score_uniq3[inspection_score_uniq3$INSPECTION_SCORE < 60,]

dat_badq_fl2 <- dat_badq2[dat_badq2$in_floodplain == 1,]
dat_badq_notfl2 <- dat_badq2[dat_badq2$in_floodplain == 0,]

dat_badq_fl2 <- dat_badq_fl2 %>%
  left_join(median_tract_dat,by = "year") %>%
  mutate(
    age_t_bad = ifelse(age_t > age_t_median, 1, 0),
    poverty_t_bad = ifelse(pov_color < poverty_color_t_median, 1, 0),
    pct_white_bad = ifelse(pct_white < pct_white_t_median, 1, 0),
  )

df_badq_fl_age2 <- dat_badq_fl2[dat_badq_fl2$age_t_bad == 1,]
df_badq_fl_pov2 <- dat_badq_fl2[dat_badq_fl2$poverty_t_bad == 1,]
df_badq_fl_white2 <- dat_badq_fl2[dat_badq_fl2$pct_white_bad == 1,]
df_badq_fl_notdis2 <- dat_badq_fl2[dat_badq_fl2$age_t_bad == 0 & dat_badq_fl2$poverty_t_bad == 0 & dat_badq_fl2$pct_white_bad == 0,]

#######################
####NJ and Hudson C####
####Did not use########
p_nj <- ggplot() +
  geom_sf(data = us_mainland, fill = "white", color = "black", linewidth = 0.3) +
  geom_point(data = dat_goodq2, aes(x = LONGITUDE, y = LATITUDE),color = "lightgray", size = 0.5, shape = 16, alpha = 1) +
  geom_point(data = dat_badq_notfl2, aes(x = LONGITUDE, y = LATITUDE),color = "blue", size = 1, shape = 16, alpha = 1) +
  geom_point(data = df_badq_fl_age2, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 2, shape = 16, alpha = 1,fill = "red") +
  geom_point(data = df_badq_fl_pov2, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 2, shape = 25, alpha = 1,fill = "red") +
  geom_point(data = df_badq_fl_white2, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 2, shape = 24, alpha = 1,fill = "red") +
  theme_minimal()+
  coord_sf(xlim = c(-75.6, -73.8), ylim = c(38.8, 41.4), expand = FALSE) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),
    legend.position = "none",
  )
png("map_nj.png", width = 3000, height = 2000,res = 300)  
p_nj
dev.off()  


p_hudson<- ggplot() +
  # Add shapefile polygons with very light blue color
  # geom_sf(data = shapefile_combined, fill = "lightblue", color = NA) +
  geom_sf(data = us_mainland, fill = "white", color = "black", linewidth = 1) +
  # geom_sf(data = shp, fill = "lightblue", color = NA) +
  geom_point(data = dat_goodq2, aes(x = LONGITUDE, y = LATITUDE),color = "lightgray", size = 0.5, shape = 16, alpha = 1) +
  geom_point(data = dat_badq_notfl2, aes(x = LONGITUDE, y = LATITUDE),color = "blue", size = 3, shape = 16, alpha = 1) +
  geom_point(data = df_badq_fl_age2, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 2, shape = 16, alpha = 1,fill = "red") +
  geom_point(data = df_badq_fl_pov2, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 2, shape = 25, alpha = 1,fill = "red") +
  geom_point(data = df_badq_fl_white2, aes(x = LONGITUDE, y = LATITUDE),color = "red", size = 2, shape = 24, alpha = 1,fill = "red") +
  theme_minimal()+
  coord_sf(xlim = c(-74.06, -74.00), ylim = c(40.72, 40.77), expand = FALSE) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),
    legend.position = "none",
  )
p_hudson

png("map_hudson.png", width = 3000, height = 2000,res = 300)  
p_hudson
dev.off()  

###

inspection_score_filtered <- dat %>%
  group_by(DEVELOPMENT_ID,latitude,longitude) %>%
  filter(n_distinct(year) > 1) %>%
  ungroup()

inspection_avg <- inspection_score_filtered %>%
  group_by(latitude,longitude, year) %>%
  summarize(
    mean_score = mean(INSPECTION_SCORE, na.rm = TRUE),
    in_floodplain = mean(in_floodplain),
    .groups = 'drop')

decay_rates <- inspection_avg %>%
  group_by(latitude,longitude) %>%
  summarize(
    decay_rate = coef(lm(mean_score ~ year))[2],
    in_floodplain = mean(in_floodplain),
    .groups = 'drop')

ggplot(decay_rates, aes(x = factor(in_floodplain), y = decay_rate, fill = factor(in_floodplain))) +
  geom_hline(yintercept = 0, col = "grey30", lty = "dashed") +
  geom_boxplot(alpha = 1, outlier.shape = NA, notch = FALSE) +  # Boxplot with transparency
  theme_minimal() +  # Clean theme
  scale_fill_manual(values = c("red", "blue")) +  # Custom fill colors
  coord_cartesian(ylim = c(-15, 10)) +
  theme(
    panel.grid.minor = element_blank(), 
    axis.text = element_text(size = 16),        
    axis.title = element_text(size = 16),
  )

  

decay_rates2 <- inspection_avg %>%
  group_by(LATITUDE, LONGITUDE) %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    min_score = mean_score[year == min(year)],
    max_score = mean_score[year == max(year)],
    slope = (max_score - min_score) / (max_year - min_year),
    in_floodplain = mean(in_floodplain)
  )

ggplot(decay_rates2, aes(x = factor(in_floodplain), y = slope, fill = factor(in_floodplain))) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, notch = FALSE) +  # Boxplot with transparency
  theme_minimal() +  # Clean theme
  scale_fill_manual(values = c("blue", "red")) +  # Custom fill colors
  coord_cartesian(ylim = c(-15, 15))


##
dat_ca <- inspection_score_uniq2[inspection_score_uniq2$STATE == "California",]
dat_ca$bad_housing <- (dat_ca$INSPECTION_SCORE < 60)*1
dat_ca$bad_flooding <- dat_ca$in_floodplain
dat_ca$bad_socio <- 0
for (i in 1:790){
  yr <- dat_ca$year[i]
  med_pov <- median_tract_dat[median_tract_dat$year == yr,9]
  med_age <- median_tract_dat[median_tract_dat$year == yr,3]
  med_white <- median_tract_dat[median_tract_dat$year == yr,10]
  dat_pov <- dat_ca$pov_color[i]
  dat_age <- dat_ca$age_t[i]
  dat_white <- dat_ca$pct_white[i]
  
  dat_ca$bad_socio[i] <- (dat_pov > med_pov | dat_age > med_age | dat_white < med_white)*1
}

##
dat_tx <- inspection_score_uniq2[inspection_score_uniq2$STATE == "Texas",]
dat_tx$bad_housing <- (dat_tx$INSPECTION_SCORE < 60)*1
dat_tx$bad_flooding <- dat_tx$in_floodplain
dat_tx$bad_socio <- 0
for (i in 1:nrow(dat_tx)){
  yr <- dat_tx$year[i]
  med_pov <- median_tract_dat[median_tract_dat$year == yr,9]
  med_age <- median_tract_dat[median_tract_dat$year == yr,3]
  med_white <- median_tract_dat[median_tract_dat$year == yr,10]
  dat_pov <- dat_tx$pov_color[i]
  dat_age <- dat_tx$age_t[i]
  dat_white <- dat_tx$pct_white[i]
  
  dat_tx$bad_socio[i] <- (dat_pov > med_pov | dat_age > med_age | dat_white < med_white)*1
}

##
dat_la <- inspection_score_uniq2[inspection_score_uniq2$STATE == "Louisiana",]
dat_la$bad_housing <- (dat_la$INSPECTION_SCORE < 60)*1
dat_la$bad_flooding <- dat_la$in_floodplain
dat_la$bad_socio <- 0
for (i in 1:nrow(dat_la)){
  yr <- dat_la$year[i]
  med_pov <- median_tract_dat[median_tract_dat$year == yr,9]
  med_age <- median_tract_dat[median_tract_dat$year == yr,3]
  med_white <- median_tract_dat[median_tract_dat$year == yr,10]
  dat_pov <- dat_la$pov_color[i]
  dat_age <- dat_la$age_t[i]
  dat_white <- dat_la$pct_white[i]
  
  dat_la$bad_socio[i] <- (dat_pov > med_pov | dat_age > med_age | dat_white < med_white)*1
}


## block group triple exposure
# inspection_score_uniq2$pct_white_b <- inspection_score_uniq2$total_white_b / inspection_score_uniq2$total_pop_b
inspection_score_uniq2 <- inspection_score_uniq2 %>%
  left_join(median_tract_dat,by = "year") %>%
  mutate(
    age_t_bad = ifelse(age_b > age_t_median, 1, 0),
    poverty_t_bad = ifelse(pov_color_t2 > poverty_color_t_median, 1, 0),
    pct_white_bad = ifelse(pct_white_b < pct_white_t_median, 1, 0),
  )

# coastal_counties <- read.csv("data/coastal_county.csv")
# inspection_score_coast <- inspection_score_uniq2 %>%
#   mutate(coastal = ifelse(paste(STATE, COUNTY_NAME) %in% paste(coastal_counties$STATE, coastal_counties$COUNTY), 1, 0))

cfr_old <- cbind(inspection_score_uniq2[inspection_score_uniq2$age_t_bad == 1 & inspection_score_uniq2$cf_riskscore_t2 > 0,c("cf_riskscore_t2","age_t_bad")],floodrisk = "Coastal")
colnames(cfr_old)[1] <- "riskscore"
cfr_young <- cbind(inspection_score_uniq2[inspection_score_uniq2$age_t_bad == 0 & inspection_score_uniq2$cf_riskscore_t2 > 0,c("cf_riskscore_t2","age_t_bad")],floodrisk = "Coastal")
colnames(cfr_young)[1] <- "riskscore"
rfr_old <- cbind(inspection_score_uniq2[inspection_score_uniq2$age_t_bad == 1,c("rf_riskscore_t2","age_t_bad")],floodrisk = "Riverine")
colnames(rfr_old)[1] <- "riskscore"
rfr_young <- cbind(inspection_score_uniq2[inspection_score_uniq2$age_t_bad == 0,c("rf_riskscore_t2","age_t_bad")],floodrisk = "Riverine")
colnames(rfr_young)[1] <- "riskscore"

age_list <- rbind(cfr_old, cfr_young, rfr_old,rfr_young)

ggplot(age_list, aes(x = as.factor(age_t_bad), y = riskscore, fill = floodrisk)) +
  geom_boxplot(notch = TRUE) +
  labs(x = "Elderly > median", y = "Risk Score", fill = "Flood Risk") +
  theme_minimal()



cfr_pov <- cbind(inspection_score_uniq2[inspection_score_uniq2$poverty_t_bad == 1 & inspection_score_uniq2$cf_riskscore_t2 > 0,c("cf_riskscore_t2","poverty_t_bad")],floodrisk = "Coastal")
colnames(cfr_pov)[1] <- "riskscore"
cfr_rich <- cbind(inspection_score_uniq2[inspection_score_uniq2$poverty_t_bad == 0 & inspection_score_uniq2$cf_riskscore_t2 > 0,c("cf_riskscore_t2","poverty_t_bad")],floodrisk = "Coastal")
colnames(cfr_rich)[1] <- "riskscore"
rfr_pov <- cbind(inspection_score_uniq2[inspection_score_uniq2$poverty_t_bad == 1,c("rf_riskscore_t2","poverty_t_bad")],floodrisk = "Riverine")
colnames(rfr_pov)[1] <- "riskscore"
rfr_rich <- cbind(inspection_score_uniq2[inspection_score_uniq2$poverty_t_bad == 0,c("rf_riskscore_t2","poverty_t_bad")],floodrisk = "Riverine")
colnames(rfr_rich)[1] <- "riskscore"

pov_list <- rbind(cfr_pov, cfr_rich, rfr_pov,rfr_rich)
pov_list <- pov_list %>%
  drop_na(riskscore, poverty_t_bad)

ggplot(pov_list, aes(x = as.factor(poverty_t_bad), y = riskscore, fill = floodrisk)) +
  geom_boxplot(notch = TRUE) +
  labs(x = "Poor of color > median", y = "Risk Score", fill = "Flood Risk") +
  theme_minimal()

cfr_whtx <- cbind(inspection_score_uniq2[inspection_score_uniq2$pct_white_bad == 1 & inspection_score_uniq2$cf_riskscore_t2 > 0,c("cf_riskscore_t2","pct_white_bad")],floodrisk = "Coastal")
colnames(cfr_whtx)[1] <- "riskscore"
cfr_wht <- cbind(inspection_score_uniq2[inspection_score_uniq2$pct_white_bad == 0 & inspection_score_uniq2$cf_riskscore_t2 > 0,c("cf_riskscore_t2","pct_white_bad")],floodrisk = "Coastal")
colnames(cfr_wht)[1] <- "riskscore"
rfr_whtx <- cbind(inspection_score_uniq2[inspection_score_uniq2$pct_white_bad == 1,c("rf_riskscore_t2","pct_white_bad")],floodrisk = "Riverine")
colnames(rfr_whtx)[1] <- "riskscore"
rfr_wht <- cbind(inspection_score_uniq2[inspection_score_uniq2$pct_white_bad == 0,c("rf_riskscore_t2","pct_white_bad")],floodrisk = "Riverine")
colnames(rfr_wht)[1] <- "riskscore"

white_list <- rbind(cfr_whtx, cfr_wht, rfr_whtx,rfr_wht)
white_list <- white_list %>%
  drop_na(riskscore, pct_white_bad)

ggplot(white_list, aes(x = as.factor(pct_white_bad), y = riskscore, fill = floodrisk)) +
  geom_boxplot(notch = TRUE) +
  labs(x = "White < median", y = "Risk Score", fill = "Flood Risk") +
  theme_minimal()

##
cfr_age2 <- cbind(inspection_score_uniq2[inspection_score_uniq2$cf_riskscore_t2 > 0,c("cf_riskscore_t2","age_b")],floodrisk = "Coastal")
colnames(cfr_age2)[1] <- "riskscore"
rfr_age2 <- cbind(inspection_score_uniq2[inspection_score_uniq2$rf_riskscore_t2 > 0,c("rf_riskscore_t2","age_b")],floodrisk = "Riverine")
colnames(rfr_age2)[1] <- "riskscore"

age_list2 <- rbind(cfr_age2,rfr_age2)

ggplot(age_list2, aes(x = age_b, y = riskscore)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ floodrisk) +
  labs(title = "Correlation between Risk Score and Age by Flood Risk Group",
       x = "Age",
       y = "Risk Score") +
  theme_minimal()

cfr_pov2 <- cbind(inspection_score_uniq2[inspection_score_uniq2$cf_riskscore_t2 > 0,c("cf_riskscore_t2","pov_color_t2")],floodrisk = "Coastal")
colnames(cfr_pov2)[1] <- "riskscore"
rfr_pov2 <- cbind(inspection_score_uniq2[inspection_score_uniq2$rf_riskscore_t2 > 0,c("rf_riskscore_t2","pov_color_t2")],floodrisk = "Riverine")
colnames(rfr_pov2)[1] <- "riskscore"

pov_list2 <- rbind(cfr_pov2,rfr_pov2)

ggplot(pov_list2, aes(x = pov_color_t2, y = riskscore)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ floodrisk) +
  labs(title = "Correlation between Risk Score and pov by Flood Risk Group",
       x = "POV_COLOR",
       y = "Risk Score") +
  theme_minimal()


cfr_white2 <- cbind(inspection_score_uniq2[inspection_score_uniq2$cf_riskscore_t2 > 0,c("cf_riskscore_t2","pct_white_b")],floodrisk = "Coastal")
colnames(cfr_white2)[1] <- "riskscore"
rfr_white2 <- cbind(inspection_score_uniq2[inspection_score_uniq2$rf_riskscore_t2 > 0,c("rf_riskscore_t2","pct_white_b")],floodrisk = "Riverine")
colnames(rfr_white2)[1] <- "riskscore"

white_list2 <- rbind(cfr_white2,rfr_white2)

ggplot(white_list2, aes(x = pct_white_b, y = riskscore)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ floodrisk) +
  labs(title = "Correlation between Risk Score and white by Flood Risk Group",
       x = "White % ",
       y = "Risk Score") +
  theme_minimal()

##
data(fips_codes)
state_codes <- unique(fips_codes$state)
state_codes <- state_codes[1:51]
state_full_names <- state.name[match(state_codes, state.abb)]
state_full_names[9] <- "District of Columbia"
states <- data.frame(code = state_codes, name = state_full_names)
states$region <- with(states, case_when(
  name %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
                    "New Jersey", "New York", "Pennsylvania") ~ "Northeast",
  
  name %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
                    "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") ~ "Midwest",
  
  name %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina",
                    "Virginia", "District of Columbia", "West Virginia",
                    "Alabama", "Kentucky", "Mississippi", "Tennessee",
                    "Arkansas", "Louisiana", "Oklahoma", "Texas") ~ "South",
  
  name %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico",
                    "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington") ~ "West",
  
  TRUE ~ NA_character_
))

inspection_score_uniq <- inspection_score_uniq %>%
  left_join(states[,c(1,3)], by = c("STATE_NAME" = "code"))

ggplot(inspection_score_uniq, aes(x = region, y = INSPECTION_SCORE, fill = region)) +
  geom_boxplot(position = position_dodge(width = 0.8),outlier.shape = NA, notch = TRUE) +
  theme_minimal() +
  scale_fill_manual(values = c("Northeast" = "#1f77b4", 
                               "South" = "#ff7f0e", 
                               "Midwest" = "#2ca02c", 
                               "West" = "#d62728"))+
  ylim(c(40,100)) +
  theme_bw()

ggplot(inspection_score_uniq, aes(x = factor(year), y = INSPECTION_SCORE, fill = region)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  scale_fill_manual(values = c("Northeast" = "#1f77b4", 
                               "South" = "#ff7f0e", 
                               "Midwest" = "#2ca02c", 
                               "West" = "#d62728")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


notch_range <- inspection_score_uniq %>%
  group_by(region) %>%
  summarise(
    n = n(),
    med = median(INSPECTION_SCORE, na.rm = TRUE),
    iqr = IQR(INSPECTION_SCORE, na.rm = TRUE),
    notch_lower = med - 1.58 * (iqr / sqrt(n)),
    notch_upper = med + 1.58 * (iqr / sqrt(n))
  )

##
inspection_score_uniq2$renterratio <- inspection_score_uniq2$renter_b / (inspection_score_uniq2$renter_b + inspection_score_uniq2$owner_b)
inspection_score_uniq2$county <- paste0(sprintf("%02d", inspection_score_uniq2$STATE_CODE), sprintf("%03d", inspection_score_uniq2$COUNTY_CODE))

inspection_score_uniq2$dummyYear <- as.factor(inspection_score_uniq2$year)
inspection_score_uniq2$dummyCounty <- as.factor(inspection_score_uniq2$county)
inspection_score_uniq3 <- inspection_score_uniq2[inspection_score_uniq2$STATE != "Florida",]
m1 <- plm(INSPECTION_SCORE ~ in_floodplain + cf_riskscore_t2 + pct_white_b + poverty_pct_b2 + age_b +
            log(per_capita_income_b) + renterratio + pop_density_b + dummyYear + dummyCounty
            ,data = inspection_score_uniq2, model="random", index = c("county","year"), random.method = "walhus")
summary(m1)

m2 <- plm(INSPECTION_SCORE ~ in_floodplain + cf_riskscore_t2 + pct_white_b + poverty_pct_b2 + age_b +
            log(per_capita_income_b) + renterratio + pop_density_b + dummyYear + dummyCounty
          ,data = inspection_score_uniq3, model="random", index = c("county","year"), random.method = "walhus")
summary(m2)
