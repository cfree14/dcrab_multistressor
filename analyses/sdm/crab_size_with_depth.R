

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sdmTMB)
library(visreg)
library(tidyverse)

# Directories
trawldir <- "data/trawl_survey/processed"
oceandir <- "data/live_ocean/processed"
outdir <- "output/sdm"
plotdir <- "figures/sdm"

# Read data
data_orig <- readRDS(file=file.path(trawldir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))

# Build data
################################################################################

data <- data_orig %>% 
  # Only with crab
  filter(total_kg>0) %>% 
  # Average crab weight
  mutate(crab_kg=total_kg/total_n)

ggplot(data, aes(y=crab_kg, x=depth_m)) +
  geom_point() + 
  geom_smooth() +
  # Axes
  lims(y=c(0, 2)) +
  # Theme
  theme_bw()

ggplot(data, aes(y=crab_kg, x=lat_dd)) +
  geom_point() + 
  geom_smooth() +
  # Axes
  lims(y=c(0, 2)) +
  # Theme
  theme_bw()

ggplot(data, aes(x=crab_kg)) + 
  geom_density() +
  lims(x=c(0, 2)) +
  theme_bw()

