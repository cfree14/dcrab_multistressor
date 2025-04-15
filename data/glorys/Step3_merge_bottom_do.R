

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
indir <- "data/glorys/raw/do_daily"
outdir <- "data/glorys/processed"

# Read data
################################################################################

# Files 2 merge
raster_files <- list.files(indir) %>% sort()

# Load all rasters into a list
raster_list <- lapply(raster_files, function(x) readRDS(file.path(indir, x)))

# Stack the rasters together
raster_stack <- stack(raster_list)

# Convert to a RasterBrick
raster_brick <- brick(raster_stack)

# Check
# plot(raster_brick[[1]], main="1")
# plot(raster_brick[[10957]], main="10957")

# Export
writeRaster(raster_brick, file=file.path(outdir, "GLORYS_1993_2022_bottom_do.tiff"))
#saveRDS(raster_brick, file=file.path(outdir, "GLORYS_1993_2022_bottom_do.Rds"))
