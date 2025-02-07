

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
trawldir <- "data/trawl_survey/processed"
oceandir <- "data/live_ocean/processed"

# Read data
trawl_orig <- readRDS(file.path(trawldir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))
ocean_orig <- readRDS(file.path(oceandir, "trawl_survey_live_ocean_data.Rds"))


# Build data
################################################################################

# Simplify ocean data
ocean <- ocean_orig %>% 
  # Simplify
  select(-c(date_lag0, year, date, lat_dd, long_dd))

# Merge data
data <- trawl_orig %>% 
  # Add ocean data
  left_join(ocean, by=c("trawl_id")) %>% 
  # Reduce to trawl tows with envi data
  filter(!is.na(temp_c))

# Export data
saveRDS(data, file=file.path(trawldir, "trawl_data_with_envi.Rds"))


# Build data
################################################################################

envi_mat <- data %>% 
  select(pressure_dbar:pco2_uatm)

GGally::ggpairs(envi_mat)






