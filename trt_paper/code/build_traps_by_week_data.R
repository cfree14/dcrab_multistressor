

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "trt_paper/figures"
outdir <- "trt_paper/output"

# Read data
prawn_orig <- readRDS(file=file.path(outdir, "spot_prawn_traps_by_week.Rds"))
dcrab_or_orig <- readRDS(file=file.path(outdir, "OR_dcrab_traps_by_week.Rds"))

# Build data
################################################################################

# State, species, Year, yweek, date, nvessels, ntraps

# Format prawn
# This is bad b/c need to propogate vessel count from regions and b/c ntraps is lazy
prawn <- prawn_orig %>% 
  # Add
  mutate(state="California", 
         species="Spot prawn") %>% 
  # Sum over regions
  group_by(state, species, year, yweek) %>% 
  summarize(ntraps=sum(ntraps)) %>% 
  ungroup() %>% 
  # Add yday
  mutate(yday=yweek*7+1) %>% 
  # Derive date
  mutate(date=ymd(paste0(year, "-01-01")) + days(yday - 1)) %>% 
  # Simplify
  select(state, species, year, date, ntraps)

# Format OR dcrab
dcrab_or <- dcrab_or_orig %>% 
  rename(ntraps=npots) %>% 
  # Add
  mutate(state="Oregon", 
         species="Dungeness crab") %>% 
  # Derive season day and use it to dervie date
  mutate(sday=sweek*7+1,
         season_yr=substr(season, 1, 4) %>% as.numeric(),
         season_start=paste0(season_yr, "-12-01") %>% ymd(),
         date=season_start + sday,
         year=year(date)) %>% 
  # Simplify
  select(state, species, year, date, ntraps)


# Merge
data <- bind_rows(prawn, dcrab_or) %>% 
  # Filter to years of interest
  filter(year>=2010)

# Export data
saveRDS(data, file = file.path(outdir, "2010_2023_traps_by_state_fishery_week.Rds"))


# Plot data
################################################################################

# Plot data
ggplot(data, aes(x=date, y=ntraps/1e3, color=species)) +
  geom_line() +
  # Labels
  labs(x="", y="Thousands of traps") +
  # Theme
  theme_bw()





