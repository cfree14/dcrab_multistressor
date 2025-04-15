

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


# Add daily LiveOcean values
################################################################################

# Convert 
convert_do <- function(x, to="mg/l"){
  if(to=="mg/l"){
    y <- x / 1e6 * 15.999*2 * 1e3
  }
  return(y)
}
convert_do(62.2, "mg/l")

# Read LiveOcean data
ocean_orig <- readRDS(file.path(oceandir, "trawl_survey_live_ocean_data.Rds"))

# Simplify LiveOcean data
ocean <- ocean_orig %>% 
  # Simplify
  select(-c(date_lag0, year, date, lat_dd, long_dd))

# Add LiveOcean data
data1 <- trawl_orig %>% 
  # Rename
  rename(temp_c_obs=temp_c,
         do_ml_l_obs=do_ml_l,
         salinity_psu_obs=salinity_psu) %>% 
  # Add ocean data
  left_join(ocean, by=c("trawl_id")) %>% 
  # Convert oxygen
  mutate(do_mg_l=convert_do(do_umol_l, to="mg/l"))

# Inspect correlation between LiveOcean and observations
ggplot(data1, aes(x=temp_c_obs, y=temp_c)) +
  geom_point(color="grey80") +
  geom_abline(a=0, b=1) +
  lims(x=c(3, 15), y=c(3, 15)) +
  theme_bw()

# Inspect correlation between LiveOcean and observations
ggplot(data1, aes(x=salinity_psu_obs, y=salinity_psu)) +
  geom_point(color="grey80") +
  geom_abline(a=0, b=1) +
  lims(x=c(30, 35), y=c(30, 35)) +
  theme_bw()

# Inspect correlation between LiveOcean and observations
# https://github.com/GLEON/LakeMetabolizer/issues/101 - 1.42905
ggplot(data1, aes(x=do_ml_l_obs, y=do_mg_l)) +
  geom_point(color="grey80") +
  # geom_abline(a=0, b=1) +
  lims(x=c(0, 6), y=c(0, 6)) +
  theme_bw()



# Add annual GLORYS temperature stats
################################################################################

# Read GLORYS data
glorys_temp_min <- terra::rast("data/glorys/processed/glorys_annual_min_temp.tiff")
glorys_temp_max <- terra::rast("data/glorys/processed/glorys_annual_max_temp.tiff")
glorys_temp_avg <- terra::rast("data/glorys/processed/glorys_annual_avg_temp.tiff")

# Extracts value
temp_min <- terra::extract(x=glorys_temp_min, y=data1 %>% select(long_dd, lat_dd), xy=T) %>% 
  rename(long_dd_glorys=x, lat_dd_glorys=y) %>% 
  select(ID, long_dd_glorys, lat_dd_glorys, everything()) %>% 
  gather(key="year", value="glorys_c_min", 4:ncol(.)) %>% 
  mutate(year=year %>% gsub("X", "", .) %>% as.numeric(.))
temp_max <- terra::extract(x=glorys_temp_max, y=data1 %>% select(long_dd, lat_dd), xy=T) %>% 
  rename(long_dd_glorys=x, lat_dd_glorys=y) %>% 
  select(ID, long_dd_glorys, lat_dd_glorys, everything()) %>% 
  gather(key="year", value="glorys_c_max", 4:ncol(.)) %>% 
  mutate(year=year %>% gsub("X", "", .) %>% as.numeric(.))
temp_avg <- terra::extract(x=glorys_temp_avg, y=data1 %>% select(long_dd, lat_dd), xy=T) %>% 
  rename(long_dd_glorys=x, lat_dd_glorys=y) %>% 
  select(ID, long_dd_glorys, lat_dd_glorys, everything()) %>% 
  gather(key="year", value="glorys_c_avg", 4:ncol(.)) %>% 
  mutate(year=year %>% gsub("X", "", .) %>% as.numeric(.))

# Add to data
data2 <- data1 %>% 
  mutate(ID=1:nrow(.)) %>% 
  left_join(temp_min, by=c("ID", "year")) %>% 
  left_join(temp_max %>% select(-c(long_dd_glorys, lat_dd_glorys)), by=c("ID", "year")) %>% 
  left_join(temp_avg %>% select(-c(long_dd_glorys, lat_dd_glorys)), by=c("ID", "year"))

# Inspect
freeR::complete(data2)


# Add daily GLORYS temperature values
################################################################################

# Read raster
glorys_temps <- terra::rast("data/glorys/raw/glorys.nc")

# GLORYS info
trawl_dates <- sort(unique(data2$date))
glorys_temps_dates <- terra::time(glorys_temps) %>% lubridate::ymd()
glorys_temps_layers <- names(glorys_temps)
glorys_temps_layers_use <- glorys_temps_layers[glorys_temps_dates %in% trawl_dates]
date_key_temps <- tibble(layer=glorys_temps_layers,
                   date=glorys_temps_dates)

# Subset to GLORYS data on trawl dates
glorys_temps_use <- glorys_temps[[glorys_temps_layers_use]]

# Extract temps for all dates at at all points
temps <- terra::extract(x=glorys_temps_use,
                        y=data2 %>% select(long_dd, lat_dd),
                        method="simple",
                        xy=T,
                        ID=T)

# Convert to DF for join
temps_df <-  temps %>% 
  # Rename
  rename(long_dd_glorys=x, lat_dd_glorys=y) %>% 
  # Arrange
  select(ID, long_dd_glorys, lat_dd_glorys, everything()) %>% 
  # Gather
  gather(key="layer", value="temp_c_glorys", 4:ncol(.)) %>% 
  # Add date
  left_join(date_key_temps, by="layer") %>% 
  # Simplify
  select(ID, date, temp_c_glorys)

# Add to data
data3 <- data2 %>% 
  left_join(temps_df, by=c("ID", "date"))

# Inspect
freeR::complete(data3)
range(data3$temp_c_glorys, na.rm=T)


# Add daily GLORYS oxygen values
################################################################################

# Read raster
glorys_dos <- terra::rast("data/glorys/processed/GLORYS_1993_2022_bottom_do.tiff")

# GLORYS info
trawl_dates <- sort(unique(data2$date))
glorys_dos_dates <- seq(ymd("1993-01-01"), ymd("2022-12-31"), by="1 day")
glorys_dos_layers <- names(glorys_dos)
glorys_dos_layers_use <- glorys_dos_layers[glorys_dos_dates %in% trawl_dates]
date_key_dos <- tibble(layer=glorys_dos_layers,
                   date=glorys_dos_dates)

# Subset to GLORYS data on trawl dates
glorys_dos_use <- glorys_dos[[glorys_dos_layers_use]]

# Extract dos for all dates at at all points
dos <- terra::extract(x=glorys_dos_use,
                      y=data2 %>% select(long_dd, lat_dd),
                      method="simple",
                      xy=T,
                      ID=T)

# Convert to DF for join
dos_df <-  dos %>% 
  # Rename
  rename(long_dd_glorys=x, lat_dd_glorys=y) %>% 
  # Arrange
  select(ID, long_dd_glorys, lat_dd_glorys, everything()) %>% 
  # Gather
  gather(key="layer", value="do_umol_l_glorys", 4:ncol(.)) %>% 
  # Add date
  left_join(date_key_dos, by="layer") %>% 
  # Simplify
  select(ID, date, do_umol_l_glorys)

# Add to data
data4 <- data3 %>% 
  left_join(dos_df, by=c("ID", "date"))

# Inspect
freeR::complete(data4)


# Compare GLORYS versus live ovean
################################################################################

# GLORYS vs LiveOcean
ggplot(data4, aes(x=temp_c_glorys, y=temp_c)) +
  geom_point(color="grey50") +
  geom_abline(slope=1) +
  lims(x=c(1,10), y=c(1,10)) +
  theme_bw()

ggplot(data4, aes(x=do_umol_l_glorys, y=do_umol_l)) +
  geom_point(color="grey50") +
  geom_abline(slope=1) +
  # lims(x=c(1,10), y=c(1,10)) +
  theme_bw()


# Export
################################################################################


# Export data
saveRDS(data4, file=file.path(trawldir, "trawl_data_with_envi.Rds"))


# Build data
################################################################################

envi_mat <- data %>% 
  select(pressure_dbar:pco2_uatm)

GGally::ggpairs(envi_mat)




