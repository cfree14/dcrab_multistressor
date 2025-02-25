

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# install.packages("remotes")
# remotes::install_github("pfmc-assessments/nwfscSurvey")

# FRAM warehouse
# https://www.webapps.nwfsc.noaa.gov/data/map

# Packages
library(tidyverse)
library(nwfscSurvey)

# Directies
datadir <- "data/trawl_survey/raw/gis_data/WCGBTS_Grid_v2008_dd.shp"
outdir <- "data/trawl_survey/processed"
plotdir <- "data/trawl_survey/figures"

# Read data
data_orig <- sf::st_read(file.path(datadir, "WCGBTS_Grid_v2008_dd.shp"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(station_id=stn_code, 
         zone=lat_34pt5,
         depth_fa_avg=z_avg_f,
         depth_fa_min=z_min_f,
         depth_fa_max=z_max_f,
         depth_m_avg=z_avg_m,
         depth_m_min=z_min_m,
         depth_m_max=z_max_m) %>% 
  # Add area (sqkm)
  mutate(area_sqkm=measurements::conv_unit(area_ha, "hectare", "km2")) %>% 
  # Simplify
  select(centroid_id, station_id,
         zone, area_ha, area_sqkm,
         depth_fa_avg:depth_m_max,
         everything())
  
# Check ids
freeR::which_duplicated(data$centroid_id)
freeR::which_duplicated(data$station_id)
table(data$zone)
table(data$z_strata_b)
table(data$z_strata_i)

# Plot
ggplot(data=data, aes(fill=zone)) +
  geom_sf(size=0.02, color=NA) +
  theme_bw()

# Export data
saveRDS(data, file=file.path(outdir, "nwfsc_wcbts_grid_cells.Rds"))

