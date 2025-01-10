

# Clear workspace
rm(list = ls())

# Setup
################################################################################


# Packages
library(tidyverse)

# Directies
datadir <- "data/live_ocean/raw"
outdir <- "data/live_ocean/processed"

# Read data
data_orig <- read.csv(file.path(datadir, "trawl_survey_date_xy_key_with_LiveOcean_model_data_0_lag.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(date_lag0=date_lag_0,
         pressure_dbar=p_dbar,
         temp_c=temp_in_situ,
         ph=p_h,
         pco2_uatm=p_co2_uatm,
         lat_dd_lo=lat_lo,
         long_dd_lo=lon_lo) %>% 
  # Format date
  mutate(date=lubridate::mdy(date),
         date_lag0=lubridate::mdy(date_lag0))

# Inspect
str(data)
 

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "trawl_survey_live_ocean_data.Rds"))


