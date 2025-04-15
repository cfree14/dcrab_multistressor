

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

# Read Kiva's version
load(file.path(datadir, "catch_Cancer magister_NWFSC.Combo_2024-12-03.rdata"))

# Get data
data_orig <- nwfscSurvey::pull_catch(sci_name = 'Cancer magister', survey = 'NWFSC.Combo')
range(data_orig$Date)

# Get hauls
hauls_orig <- nwfscSurvey::pull_haul(survey="NWFSC.Combo")
range(hauls_orig$Date)

# Export
saveRDS(data_orig, file=file.path(datadir, "dcrab_trawl_survey_data_2023_12_09.Rds"))
saveRDS(hauls_orig, file=file.path(datadir, "dcrab_trawl_survey_hauls_2024_03_31.Rds"))

