
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "analyses/trt_paper/data"
outdir <- "analyses/trt_paper/output"
plotdir <- "trt_paper/figures"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_closures/data/processed/2015_2023_WC_dcrab_closures.Rds")

# Read time key
time_key_orig <- read.csv(file.path(indir, "days_per_period.csv")) 

# Format data
################################################################################

# Format time key
time_key <- time_key_orig %>% 
  # Add row id
  mutate(row_id=1:n()) %>% 
  # Reduce
  filter(min_date > "2015-12-13")

# Latitude breaks
lat_breaks <- c(32,
                34+27/60,
                36,
                37+11/60,
                38+46.125/60,
                40+10/60,
                42,
                # Oregon
                43.12, # Bandon
                45.06, # Cascade Head
                46.263, # OR/WA border
                # Washington
                46.466, # Klipsan Beach
                47.67, # Destruction Island
                48.29 # Tatoosh Island
)

# Format data
data <- data_orig  %>% 
  # Remove above max lat considered
  filter(lat_dd<=max(lat_breaks)) %>% 
  # Add lat bin
  mutate(lat_bin=cut(lat_dd, breaks=lat_breaks, labels=c("CA6", "CA5", "CA4", "CA3", "CA2", "CA1",
                                                         "OR1", "OR2", "OR3",
                                                         "WA1", "WA2", "WA3")))


# Visualize intersection of weeks/zones with closure
################################################################################





# Loop through time key and calculate closed days by lat zone
################################################################################

# Loop through
row_ids <- time_key$row_id
x <- 31
stats <- purrr::map_df(row_ids, function(x){
  
  # Extract time key
  time_row <- time_key %>% filter(row_id==x)
  date1 <- time_row$min_date
  date2 <- time_row$max_date
  
  # Filter closure data
  sdata <- data_orig %>% 
    # Reduce to period between dates
    filter(date>=date1 & date<=date2) %>% 
    # Summarize number of lat-days with closures
    group_by(lat_bin) %>% 
    summarize(ndays=n_distinct(date),
              lat_range=max(lat_dd)-min(lat_dd),
              latdays_tot=n()*0.01,
              latdays_close=sum(status!='Season open') * 0.01) %>% 
    ungroup() %>% 
    mutate(latdays_close_prop=latdays_close/latdays_tot) %>% 
    # Add row id
    mutate(row_id=x)
  
})


# Format data
################################################################################

# Add time meta-data
stats_out <- stats %>% 
  # Add time key meta-data
  left_join(time_key) %>% 
  # Arrange
  select(row_id, crab_year, period, n_days, min_date, max_date, everything())
  

# Export
################################################################################

# Export
write.csv(stats_out, file=file.path(outdir, "percent_of_zones_under_restriction_by_week_and_zone.csv"), row.names = F)




