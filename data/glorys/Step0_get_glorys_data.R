
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

# Directories
rawdir <- "data/glorys/raw"

# GLORYS data access site
# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/services
# https://help.marine.copernicus.eu/en/articles/7983226-copernicus-marine-toolbox-cli-get-original-files

# RCMEMS package
# https://github.com/markpayneatwork/RCMEMS

# Example use of RCMEMS package
# https://theoceancode.netlify.app/post/dl_env_data_r/

# Install RCMEMS package
# devtools::install_github("markpayneatwork/RCMEMS")


# Function
################################################################################

# Exampple
# username <- "cfree"
# password <- "Science1234!"
# dataset <- "cmems_mod_glo_phy_my_0.083deg_P1D-m"
# variable <- "bottomT"
# bbox <- c(-126, -124, 32, 34)
# daterange <- c("2020-01-01", "2020-01-08")
# filename <- file.path("~/Desktop/", "example.nc")

# Download GLORYS data
download_glorys <- function(username, password, dataset, variable, bbox, daterange, filename, store=T){
  
  # This is example code that can be put into terminal
  # copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1D-m --username cfree --password Science1234! -v bottomT -x -126 -X -124 -y 32 -Y 34 -t "2020-01-01" -T "2020-01-08" -o "~/Desktop/" -f example.nc --force-download
 
  # This is an example query that can be run with system()
  # query_example <- '/Library/Frameworks/Python.framework/Versions/3.12/bin/copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1D-m --username cfree --password Science1234! -v bottomT -x -126 -X -124 -y 32 -Y 34 -t "2020-01-01" -T "2020-01-08" -o "~/Desktop/" -f example.nc --force-download'
  
  # Retrieve Copernicus location
  # This doesn't work but would improve base_q
  # cm_path <- system(command="which copernicusmarine")
  
  # Build query components
  base_q <- "/Library/Frameworks/Python.framework/Versions/3.12/bin/copernicusmarine subset "
  dataset_q <- paste("-i", dataset)
  login_q <- paste("--username", username, "--password", password)
  variable_q <- paste("-v", variable)
  bbox_q <- paste("-x", bbox[1], "-X", bbox[2], "-y", bbox[3],  "-Y", bbox[4])
  daterange_q <- paste("-t", daterange[1], "-T", daterange[2])
  end_q <- "--force-download --overwrite-output-data"
  
  # Download path
  if(is.null(filename)){
    filename_use <- file.path(tempfile(), "temp.nc")
    file_q <- paste("-o", dirname(filename_temp), "-f", basename(filename_temp))
  }else{
    file_q <- paste("-o", dirname(filename), "-f", basename(filename))
    filename_use <- filename
  }
  
  # Build final query
  query <- paste(base_q, dataset_q, login_q, variable_q, bbox_q, daterange_q, file_q, end_q)

  # Submit query and download data
  system(query, intern=F)
  
  # If reading into environment
  if(store==T){
    # obj <- ncdf4::nc_open(filename_use)
    obj <- raster::brick(filename_use)
  }else{
    obj <- "download complete"
  }
  
  # Return
  return(obj)
  
}

# Get data
data_nc <- download_glorys(username="cfree",
                           password="Science1234!",
                           dataset="cmems_mod_glo_phy_my_0.083deg_P1D-m",
                           variable="bottomT",
                           bbox=c(-126, -115, 32, 49),
                           daterange=c("1993-01-01", "2021-06-30"), 
                           filename=file.path(rawdir, "glorys.nc"))


# Function
################################################################################

# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_BGC_001_029/description

# Exampple
# username <- "cfree"
# password <- "Science1234!"
# dataset <- "cmems_mod_glo_bgc_my_0.25deg_P1D-m"
# variable <- "bottomT"
# bbox <- c(-126, -124, 32, 34)
# daterange <- c("2020-01-01", "2020-01-08")
# filename <- file.path("~/Desktop/", "example.nc")

# Download GLORYS data
download_glorys_do <- function(username, password, dataset, variable, bbox, daterange, depth_range, filename, store=T){
  
  # This is example code that can be put into terminal
  # copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1D-m --username cfree --password Science1234! -v bottomT -x -126 -X -124 -y 32 -Y 34 -t "2020-01-01" -T "2020-01-08" -o "~/Desktop/" -f example.nc --force-download
  
  # This is an example query that can be run with system()
  # query_example <- '/Library/Frameworks/Python.framework/Versions/3.12/bin/copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1D-m --username cfree --password Science1234! -v bottomT -x -126 -X -124 -y 32 -Y 34 -t "2020-01-01" -T "2020-01-08" -o "~/Desktop/" -f example.nc --force-download'
  
  # Retrieve Copernicus location
  # This doesn't work but would improve base_q
  # cm_path <- system(command="which copernicusmarine")
  
  # Build query components
  base_q <- "/Library/Frameworks/Python.framework/Versions/3.12/bin/copernicusmarine subset "
  dataset_q <- paste("-i", dataset)
  login_q <- paste("--username", username, "--password", password)
  variable_q <- paste("-v", variable)
  bbox_q <- paste("-x", bbox[1], "-X", bbox[2], "-y", bbox[3],  "-Y", bbox[4])
  daterange_q <- paste("-t", daterange[1], "-T", daterange[2])
  # depth_q <- paste("-z", depth_range[1], "-Z", depth_range[2])
  end_q <- "--force-download --overwrite-output-data"
  
  # Download path
  if(is.null(filename)){
    filename_use <- file.path(tempfile(), "temp.nc")
    file_q <- paste("-o", dirname(filename_temp), "-f", basename(filename_temp))
  }else{
    file_q <- paste("-o", dirname(filename), "-f", basename(filename))
    filename_use <- filename
  }
  
  # Build final query
  query <- paste(base_q, dataset_q, login_q, variable_q, bbox_q, daterange_q, file_q, end_q) # depth_q,
  
  # Submit query and download data
  system(query, intern=F)
  
  # If reading into environment
  if(store==T){
    # obj <- ncdf4::nc_open(filename_use)
    obj <- raster::brick(filename_use)
  }else{
    obj <- "download complete"
  }
  
  # Return
  return(obj)
  
}

# Get data
data_nc1 <- download_glorys_do(username="cfree",
                             password="Science1234!",
                             dataset="cmems_mod_glo_bgc_my_0.25deg_P1D-m",
                             variable="o2",
                             bbox=c(-126, -115, 32, 49),
                             daterange=c("1993-01-01", "2022-12-31"), # "1993-01-01", "2022-01-01"
                             filename=file.path(rawdir, "glorys_DO_1993_2022.nc"))


# Read full netcdf
#data_nc <- ncdf4::nc_open(file.path(rawdir, "glorys_DO.nc"))
