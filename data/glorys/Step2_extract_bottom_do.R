

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Library
library(ncdf4)
library(tidync)
library(tidyverse)
library(lubridate)
library(raster)
library(foreach)
library(doParallel)

# Directories
rawdir <- "data/glorys/raw"
outdir <- "data/glorys/raw/do_daily"

# Read data
data_nc <- ncdf4::nc_open(file.path(rawdir, "glorys_DO_1993_2022.nc"))

# Extract latitude, longitude, depth, and oxygen variables
lon <- ncvar_get(data_nc, "longitude")  # Adjust variable names as per your file
lat <- ncvar_get(data_nc, "latitude")
depth <- ncvar_get(data_nc, "depth")  # Depth levels (should be 75)
times <- ncvar_get(data_nc, "time")
#oxygen <- ncvar_get(data_nc, "o2")  # 4D array: lon × lat × depth × time

# Convert into dates
dates <- as.POSIXct(times * 3600, origin = "1950-01-01", tz = "UTC") %>% ymd()


# Helper function
################################################################################

# Function to get bottom value from NetCDF
file <- file.path(rawdir, "glorys_DO_1993_2022.nc")
get_bottom_value <- function(file){
  
  # Open NetCDF
  nc <- nc_open(file)
  
  # Dimension names, units, sizes
  dim_names <- sapply(nc$var$o2$dim, function(x)x$name)
  dim_sizes <- nc$var$o2$size
  dim_units <- sapply(nc$var$o2$dim, function(x)x$units)
  
  # Add dimension names
  names(dim_sizes) <- dim_names
  names(dim_units) <- dim_names
  
  # stopifnot(grepl("months since ", dim.units[4])) # make sure time is in correct units and in right place
  
  # Extract sizes
  ntime <- dim_sizes["time"]
  ndepth <- dim_sizes["depth"]
  
  # Get times
  hours_since_1950 <- nc$var$o2$dim[[4]]$vals
  
  # Convert into dates
  dates <- as.POSIXct(hours_since_1950 * 3600, origin = "1950-01-01", tz = "UTC")
  
  
  # Setup text progress bar
  pb <- txtProgressBar(min=1, max=ntime, style=3)
  
  # Run for loop
  i <- 1
  for(i in 1:ntime){
    
    # Brick of all values for all depth levels on single timestep
    ldata <- raster::brick(file, lvar=4, level=i)
    
    # Extract depth levels
    depths <-  names(ldata) %>% gsub("X", "", .) %>% as.numeric()
    
    # Get the deepest available value in each gridd cell
    # 1. Subset flips the brick so that the deepest layer is first
    # 2. Unstack converts from a brick of raster layers to a list of individual rasters
    # 3. Cover keeps the first value and iteratively replaces NA with subsequent values
    # (in other words, the final value is the first non-NA value, as this value isn't replaced)
    bottom <- do.call(raster::cover, raster::unstack(raster::subset(ldata, length(depths):1)))
    names(bottom) <- dates[i]
    
    # Merge rasters for each time step
    if(i==1){
      data_out <- bottom
    }else{
      data_out <- addLayer(data_out, bottom)
    }
    
    # Update progress bar
    setTxtProgressBar(pb, i)
    
  }
  
  # Close progress bar
  close(pb)
  
  # Return layer
  return(data_out)
  
}


# Apply function
################################################################################

#data_out <- get_bottom_value(file=file.path(rawdir, "glorys_DO.nc"))




# Parallelize
################################################################################

# Open NetCDF
nc <- nc_open(file.path(rawdir, "glorys_DO_1993_2022.nc"))

# Dimension names, units, sizes
dim_names <- sapply(nc$var$o2$dim, function(x)x$name)
dim_sizes <- nc$var$o2$size
dim_units <- sapply(nc$var$o2$dim, function(x)x$units)

# Add dimension names
names(dim_sizes) <- dim_names
names(dim_units) <- dim_names

# Extract sizes
ntime <- dim_sizes["time"]
ndepth <- dim_sizes["depth"]

# Set up parallel backend
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Run for loop
i <- 1
results <- foreach(i = 1:ntime, 
                   .packages = c("dplyr", "raster")) %dopar% {
  
  # Brick of all values for all depth levels on single timestep
  ldata <- raster::brick(file, lvar=4, level=i)
  
  # Extract depth levels
  depths <-  names(ldata) %>% gsub("X", "", .) %>% as.numeric()
  
  # Get the deepest available value in each gridd cell
  # 1. Subset flips the brick so that the deepest layer is first
  # 2. Unstack converts from a brick of raster layers to a list of individual rasters
  # 3. Cover keeps the first value and iteratively replaces NA with subsequent values
  # (in other words, the final value is the first non-NA value, as this value isn't replaced)
  bottom <- do.call(raster::cover, raster::unstack(raster::subset(ldata, length(depths):1)))
  names(bottom) <- dates[i]
  
  # Export
  date <- dates[i]
  outfile <- paste0(date, ".Rds")
  saveRDS(bottom, file=file.path(outdir, outfile))
  
}



