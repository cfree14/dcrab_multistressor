
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
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_closures/data/processed/2015_2024_WC_dcrab_closures.Rds")

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

# If Jameal version
version <- "real"
# version <- "jameal"
if(version=="jameal"){

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
  
}else{
  
  # Latitude breaks
  lats_orig <- readxl::read_excel("data/mgmt_areas/WC_dcrab_da_mgmt_zones.xlsx") %>% filter(state!="California")
  lat_breaks <- c(lats_orig$lat_dd_north, lats_orig$lat_dd_south,
                  32,
                  34+27/60,
                  36,
                  37+11/60,
                  38+46.125/60,
                  40+10/60,
                  42) %>% sort() %>% unique() %>% na.omit()
  lat_zones <- c(paste0("CA-", 6:1), 
                 paste0("OR-50-", LETTERS[12:1]), 
                 c("WA-60A-2", "WA-60A-2", "WA-59A-2", "WA-59A-1"))
  
  data <- data_orig  %>% 
    # Remove above max lat considered
    filter(lat_dd<=max(lat_breaks)) %>% 
    # Add lat bin
    mutate(lat_bin=cut(lat_dd, breaks=lat_breaks, labels=lat_zones))
  
}


# Loop through time key and calculate closed days by lat zone
################################################################################

all_statuses <- sort(unique(data_orig$status))

# Status
open_status <- c("Season open")
closed_status <- c("Out-of-season",
                   "Body condition delay", 
                   "Body condition/domoic acid delay",                   
                   "Domoic acid delay",
                   "Whale entanglement closure",
                   "Whale entanglement/domoic acid delay")
restriction_status <- c("Evisceration order",
                        "Evisceration order (+depth restriction/gear reduction)",
                        "30-fathom depth restriction",                          
                        "40-fathom depth restriction",      
                        "40-fathom depth restriction/20% gear reduction",  
                        "30-fathom depth restriction/25% gear reduction",
                        "30-fathom depth restriction/50% gear reduction",
                        "25% gear reduction",
                        "33% gear reduction",                              
                        "50% gear reduction")
merged_statuses <- c(open_status, closed_status, restriction_status)

# Check
all_statuses[!all_statuses %in% merged_statuses]


# Loop through
row_ids <- time_key$row_id
x <- 31
stats <- purrr::map_df(row_ids, function(x){
  
  # Extract time key
  time_row <- time_key %>% filter(row_id==x)
  date1 <- time_row$min_date
  date2 <- time_row$max_date
  
  # Filter closure data
  sdata <- data %>% 
    # Reduce to period between dates
    filter(date>=date1 & date<=date2) %>% 
    # Summarize number of lat-days with closures
    group_by(lat_bin) %>% 
    summarize(ndays=n_distinct(date),
              lat_range=max(lat_dd)-min(lat_dd),
              latdays_tot=n()*0.01,
              latdays_open=sum(status %in% open_status) * 0.01,
              latdays_restricted=sum(status %in% restriction_status) * 0.01,
              latdays_closed=sum(status %in% closed_status) * 0.01,) %>% 
    ungroup() %>% 
    mutate(latdays_open_prop=latdays_open/latdays_tot,
           latdays_restricted_prop=latdays_restricted/latdays_tot,
           latdays_closed_prop=latdays_closed/latdays_tot) %>% 
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
  select(row_id, crab_year, period, n_days, min_date, max_date, everything()) %>% 
  # Check sum
  mutate(prop_check=latdays_open_prop+latdays_restricted_prop+latdays_closed_prop)
  
# Should all be 1
table(stats_out$prop_check)


# Export
################################################################################

outfile <- ifelse(version=="real",
                  "percent_of_zones_under_restriction_by_week_and_zone_real.csv",
                  "percent_of_zones_under_restriction_by_week_and_zone_jameal.csv")

# Export
write.csv(stats_out, file=file.path(outdir, outfile), row.names = F)




