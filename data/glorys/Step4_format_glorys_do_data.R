

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(terra)
library(tidyverse)

# Directories
indir <- "data/glorys/raw"
outdir <- "data/glorys/processed"

# Read data
data_orig <- terra::rast(file.path(outdir, "GLORYS_1993_2022_bottom_do.tiff"))

# Convert 
convert_do <- function(x, to="mg/l"){
  if(to=="mg/l"){
    y <- x / 1e6 * 15.999*2 * 1e3
  }
  return(y)
}
convert_do(62.2, "mg/l")


# Compute annual stats
################################################################################

# Create a time index based on the layer names
dates <- seq(as.Date("1993-01-01"), as.Date("2022-12-31"), by="day")
years <- format(dates, "%Y")  # Extract year

# Create a time index based on the layer names
# dates <- names(data_orig) %>% gsub("X", "", .) %>% ymd()
# years <- year(dates)  # Extract year

# # Define functions for min, max, and mean
# fun_list <- list(min = min, max = max, mean = mean)

# Compute annual statistics using `tapp`
annual_min  <- terra::tapp(data_orig, index=years, fun=min)
annual_max  <- terra::tapp(data_orig, index=years, fun=max)
annual_avg <- terra::tapp(data_orig, index=years, fun=mean)

# Save rasters
terra::writeRaster(annual_min, file=file.path(outdir, "glorys_annual_min_do.tiff"), overwrite=T)
terra::writeRaster(annual_max, file=file.path(outdir, "glorys_annual_max_do.tiff"), overwrite=T)
terra::writeRaster(annual_avg, file=file.path(outdir, "glorys_annual_avg_do.tiff"), overwrite=T)


# Merge annual stats
################################################################################

# Function to format data
format_data <- function(data, colname){

  # Convert to data frame
  data_df <- data %>% 
    # Convert to dataframe
    as.data.frame(xy=T) %>% 
    # Gather
    gather(key="year", value="do_umol_l", 3:ncol(.)) %>% 
    # Format year
    mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
    # Rename
    setNames(c("long_dd", "lat_dd", "year", colname))
  
}

# Format data
annual_min_df <- format_data(annual_min, colname="do_umol_l_min")
annual_max_df <- format_data(annual_max, colname="do_umol_l_max")
annual_avg_df <- format_data(annual_avg, colname="do_umol_l_avg")

# Merge data
data <- annual_min_df %>% 
  left_join(annual_max_df) %>% 
  left_join(annual_avg_df) %>% 
  # Convert umol/l to mg/l
  mutate(do_umol_l_min=convert_do(do_umol_l_min, "mg/l"),
         do_umol_l_max=convert_do(do_umol_l_max, "mg/l"),
         do_umol_l_avg=convert_do(do_umol_l_avg, "mg/l"))

# Export
saveRDS(data, file=file.path(outdir, "GLORYS_1993_2022_annual_bdo_stats.Rds"))

