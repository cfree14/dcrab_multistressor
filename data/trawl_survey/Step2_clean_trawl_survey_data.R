

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


# Format data
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






