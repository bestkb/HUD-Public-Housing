library(dplyr)

# Number of decimals to keep
NUM_DECIMAL <- 2

# Header for year
HEADER_YEAR <- c('year', 'count', 'mean', 'std', 'min', '25%', '50%', '75%', 'max')

# Header for state
HEADER_STATE <- c('state', 'count', 'mean', 'std', 'min', '25%', '50%', '75%', 'max')

# Read the dataset
df <- read.csv('data/locations_inspectionscores_forMeri_Nov.csv')

# Inspection score data
inspection_score <- df$INSPECTION_SCORE

# Statistic CSV table -- by year
stat_file <- file('figures/stats_inspection_score_year.csv', 'w')

stat_writer <- write.csv(stat_file)

write.table(header = TRUE, as.data.frame(t(HEADER_YEAR)), row.names = FALSE, sep = ',', col.names = FALSE)

# Overall inspection score statistics
stats <- round(summary(inspection_score), NUM_DECIMAL)
stats <- c('overall', stats)

write.table(as.data.frame(t(stats)), row.names = FALSE, sep = ',', col.names = FALSE)

# Get all available years and get them sorted
all_years <- sort(unique(as.numeric(df$inspection_year)))

# Each-year inspection score statistics
for (yr in all_years) {
  inspection_score_each <- df %>%
    filter(inspection_year == yr) %>%
    select(INSPECTION_SCORE)
  
  stats_each <- round(summary(inspection_score_each), NUM_DECIMAL)
  stats_each <- c(yr, stats_each)
  
  write.table(as.data.frame(t(stats_each)), row.names = FALSE, sep = ',', col.names = FALSE)
}

# Close the file
close(stat_file)

# Get all available states and get them sorted
all_states <- sort(unique(df$STATE_NAME.x))

# Statistic CSV table -- by state
stat_file <- file('figures/stats_inspection_score_state.csv', 'w')

stat_writer <- write.csv(stat_file)

write.table(header = TRUE, as.data.frame(t(HEADER_STATE)), row.names = FALSE, sep = ',', col.names = FALSE)

# Each-state inspection score statistics
for (st in all_states) {
  inspection_score_each <- df %>%
    filter(STATE_NAME.x == st) %>%
    select(INSPECTION_SCORE)
  
  stats_each <- round(summary(inspection_score_each), NUM_DECIMAL)
  stats_each <- c(st, stats_each)
  
  write.table(as.data.frame(t(stats_each)), row.names = FALSE, sep = ',', col.names = FALSE)
}

# Close the file
close(stat_file)
