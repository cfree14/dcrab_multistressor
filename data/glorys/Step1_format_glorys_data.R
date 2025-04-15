

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(terra)
library(tidyverse)

# Directories
indir <- "data/glorys/raw"
outdir <- "data/glorys/processed"

# Read data
data_orig <- terra::rast(file.path(indir, "glorys.nc"))

# Compute annual stats
################################################################################

# Create a time index based on the layer names
dates <- seq(as.Date("1993-01-01"), as.Date("2021-06-30"), by="day")
years <- format(dates, "%Y")  # Extract year

# # Define functions for min, max, and mean
# fun_list <- list(min = min, max = max, mean = mean)

# Compute annual statistics using `tapp`
annual_min  <- terra::tapp(data_orig, index=years, fun=min)
annual_max  <- terra::tapp(data_orig, index=years, fun=max)
annual_avg <- terra::tapp(data_orig, index=years, fun=mean)

# Save rasters
terra::writeRaster(annual_min, file=file.path(outdir, "glorys_annual_min_temp.tiff"), overwrite=T)
terra::writeRaster(annual_max, file=file.path(outdir, "glorys_annual_max_temp.tiff"), overwrite=T)
terra::writeRaster(annual_avg, file=file.path(outdir, "glorys_annual_avg_temp.tiff"), overwrite=T)


# Merge annual stats
################################################################################

# Function to format data
format_data <- function(data, colname){

  # Convert to data frame
  data_df <- data %>% 
    # Convert to dataframe
    as.data.frame(xy=T) %>% 
    # Gather
    gather(key="year", value="temp_c", 3:ncol(.)) %>% 
    # Format year
    mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
    # Rename
    setNames(c("long_dd", "lat_dd", "year", colname))
  
}

# Format data
annual_min_df <- format_data(annual_min, colname="temp_c_min")
annual_max_df <- format_data(annual_max, colname="temp_c_max")
annual_avg_df <- format_data(annual_avg, colname="temp_c_avg")

# Merge data
data <- annual_min_df %>% 
  left_join(annual_max_df) %>% 
  left_join(annual_avg_df) %>% 
  filter(year < 2021)

# Export
saveRDS(data, file=file.path(outdir, "GLORYS_1993_2021_annual_bt_stats.Rds"))

