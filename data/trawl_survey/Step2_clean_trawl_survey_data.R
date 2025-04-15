

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
datadir <- "data/trawl_survey/raw"
outdir <- "data/trawl_survey/processed"
plotdir <- "data/trawl_survey/figures"

# Read data 
data_orig <- readRDS(file=file.path(datadir, "dcrab_trawl_survey_data_2023_12_09.Rds"))
hauls_orig <- readRDS(file=file.path(datadir, "dcrab_trawl_survey_hauls_2024_03_31.Rds"))


# Format hauls
################################################################################

# Fortmat data
hauls <- hauls_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(datetime=datetime_utc_iso,
         date=date_formatted,
         time1=sampling_start_hhmmss,
         time2=sampling_end_hhmmss,
         # Lat/long
         lat_dd=latitude_dd,
         long_dd=longitude_dd,
         lat_dd1_gear=gear_start_latitude_dd,
         long_dd1_gear=gear_start_longitude_dd,
         lat_dd2_gear=gear_end_latitude_dd,
         long_dd2_gear=gear_end_longitude_dd,
         lat_dd1_vessel=vessel_start_latitude_dd,
         long_dd1_vessel=vessel_start_longitude_dd,
         lat_dd2_vessel=vessel_end_latitude_dd,
         long_dd2_vessel=vessel_end_longitude_dd,
         # Environment
         salinity_psu=salinity_at_gear_psu_der ,
         temp_c=temperature_at_gear_c_der, 
         sst_c=temperature_at_surface_c_der,
         do_ml_l=o2_at_gear_ml_per_l_der,
         turbidity_ntu=turbidity_ntu_der,
         fluorescence_mg_m3=fluorescence_at_surface_mg_per_m3_der,
         # Trawl properties
         swept_ha=area_swept_ha_der,
         depth_m=depth_hi_prec_m, 
         performance_code_old=operation_dim_legacy_performance_code,
         # Net properties
         net_width_m=net_width_m_der, 
         net_height_m=net_height_m_der,
         door_width_m=door_width_m_der,
         # Weights
         inverts_kg=invertebrate_weight_kg,
         fish_kg=vertebrate_weight_kg, 
         organics_kg=nonspecific_organics_weight_kg 
         ) %>% 
  # Format date
  mutate(date=lubridate::ymd(substr(datetime, 1, 10))) %>% 
  # Arrange
  select(project,
         trawl_id, 
         year, date, datetime, time1, time2,
         vessel, pass, leg, station_invalid,
         lat_dd, long_dd, 
         lat_dd1_vessel, long_dd1_vessel, 
         lat_dd2_vessel, long_dd2_vessel, 
         lat_dd1_gear, long_dd1_gear, 
         lat_dd2_gear, long_dd2_gear, 
         net_width_m, net_height_m, door_width_m, 
         performance, performance_code_old, 
         swept_ha, depth_m,
         sst_c, temp_c, do_ml_l, salinity_psu, turbidity_ntu, fluorescence_mg_m3,
         fish_kg, inverts_kg, organics_kg,
         everything()) %>% 
  # Remove useless (empty or redundant)
  select(-c(performance_code_old, datetime, turbidity_ntu, fluorescence_mg_m3))
  

# Inspect
str(hauls)
freeR::complete(hauls)
freeR::complete(hauls) / nrow(hauls) * 100

# Inspect
table(hauls$project)
table(hauls$vessel)
table(hauls$pass)
table(hauls$leg)
table(hauls$performance)
table(hauls$performance_code_old)
table(hauls$station_invalid)

# Temp vs. dissolved oxygen
ggplot(hauls, aes(x=temp_c, y=do_ml_l)) +
  geom_point()


# Format catch data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(comm_name=common_name,
         sci_name=scientific_name,
         datetime=datetime_utc_iso,
         date2=date, 
         lat_dd=latitude_dd,
         long_dd=longitude_dd,
         subsample_n=subsample_count, 
         subsample_kg=subsample_wt_kg, 
         total_n=total_catch_numbers, 
         total_kg=total_catch_wt_kg,
         cpue_kg_ha=cpue_kg_per_ha_der) %>% 
  # Format characters
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Add area swept in km2
  mutate(area_swept_km2=measurements::conv_unit(area_swept_ha, "hectare", "km2")) %>% 
  # Format data
  tidyr::separate(datetime, into=c("date", "time"), sep="T", remove=F) %>% 
  mutate(date=lubridate::ymd(date)) %>% 
  # Remove empty or useless
  select(-c(partition, partition_sample_types, legacy_performance_code, time, datetime, date2)) %>% 
  # Mark state
  mutate(state=cut(lat_dd, breaks=c(0, 42, 46.25, 50), labels=c("California", "Oregon", "Washington"))) %>% 
  # Add enviornmental
  left_join(hauls %>% select(trawl_id, temp_c, temp_c, do_ml_l, salinity_psu)) %>% 
  # Arrange
  select(project, comm_name, sci_name, 
         year, date, 
         trawl_id, performance, vessel, tow, pass, station_invalid, 
         state, lat_dd, long_dd, depth_m, area_swept_ha, area_swept_km2,
         subsample_n, subsample_kg, total_n, total_kg,
         cpue_kg_km2, cpue_kg_ha,
         everything())


# Inspect
str(data)
freeR::complete(data)

# Inspect more
table(data$comm_name)
table(data$sci_name)
table(data$performance)
table(data$project)
table(data$vessel)
table(data$station_invalid)
table(data$state)

# Range
range(data$lat_dd)
range(data$long_dd)
range(data$depth_m)

# Years
table(data$year)


# Add UTM coordinates
################################################################################

# Convert to sf object with WGS84 (EPSG:4326)
data_sf <- sf::st_as_sf(data, coords = c("long_dd", "lat_dd"), crs = 4326)

# Transform to UTM Zone 10N (EPSG:32610)
data_sf_utm <- sf::st_transform(data_sf, crs = 32610)

# Extract UTM coordinates
utm10_easting <- sf::st_coordinates(data_sf_utm)[, 1]
utm10_northing <- sf::st_coordinates(data_sf_utm)[, 2]

# Record
data1 <- data %>% 
  # Add UTM10N coordinates
  mutate(lat_utm10n=utm10_northing,
         long_utm10n=utm10_easting) %>% 
  # Arrange
  select(project:long_dd, lat_utm10n, long_utm10n, everything())

# Inspect
freeR::complete(data1)

# Export
saveRDS(data1, file=file.path(outdir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))


# Lat/long key
################################################################################

# Key
date_xy_key <- data %>% 
  select(trawl_id, date, lat_dd, long_dd) %>% 
  unique()

# Export
write.csv(date_xy_key, file=file.path(outdir, "trawl_survey_date_xy_key.csv"), row.names=F)

xy_key <- data %>% 
  count(state, lat_dd, long_dd)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot
g <- ggplot(xy_key, aes(x=long_dd, y=lat_dd, color=state)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot surveys
  geom_point() +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-126, -116.5), ylim = c(32.5, 48.5)) +
  # Theme
  theme_bw()
g






