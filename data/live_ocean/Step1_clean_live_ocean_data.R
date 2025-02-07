

# Clear workspace
rm(list = ls())

# Setup
################################################################################


# Packages
library(tidyverse)

# Directies
datadir <- "data/live_ocean/raw"
outdir <- "data/live_ocean/processed"
trawldir <- "data/trawl_survey/processed"

# Read data
data_orig <- read.csv(file.path(datadir, "trawl_survey_date_xy_key_with_LiveOcean_model_data_0_lag.csv"), as.is=T)

# Read tow key
tow_key <- read.csv(file=file.path(trawldir, "trawl_survey_date_xy_key.csv"), as.is=T) %>% 
  mutate(date=lubridate::ymd(date),
         trawl_id=as.character(trawl_id),
         lat_dd_chr=round(lat_dd, 5),
         long_dd_chr=round(long_dd, 5)) %>% 
  select(-c(lat_dd, long_dd))


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
         date_lag0=lubridate::mdy(date_lag0)) %>% 
  # Add year 
  mutate(year=lubridate::year(date)) %>% 
  filter(year!=2023) %>% 
  # Add trawl id
  mutate(lat_dd_chr=round(lat_dd, 5),
         long_dd_chr=round(long_dd, 5)) %>% 
  left_join(tow_key) %>% 
  select(-c(lat_dd_chr, long_dd_chr)) %>% 
  # Arrange
  select(year, trawl_id, everything())

# Inspect
str(data)
freeR::complete(data)
 

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "trawl_survey_live_ocean_data.Rds"))


