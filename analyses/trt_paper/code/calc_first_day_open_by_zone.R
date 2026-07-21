
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

# Format data
################################################################################

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
  lat_zones <- c("CA6", "CA5", "CA4", "CA3", "CA2", "CA1",
                 "OR1", "OR2", "OR3",
                 "WA1", "WA2", "WA3")
  
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
}
  

# Format data
data <- data_orig  %>% 
  # Remove above max lat considered
  filter(lat_dd<=max(lat_breaks)) %>% 
  # Add julian day
  mutate(yday=lubridate::yday(date)) %>% 
  # Mark season
  mutate(season=ifelse(yday>300, year(date), year(date)-1)) %>% 
  # Add lat bin
  mutate(lat_bin=cut(lat_dd, breaks=lat_breaks, labels=lat_zones))


# Define Status
statuses <- sort(unique(data$status))
open <- c("Season open")
closed <- c("Body condition delay",                                   
            "Body condition/domoic acid delay",                      
            "Domoic acid delay",                                     
            "Out-of-season" ,                                        
            "Whale entanglement closure",                            
            "Whale entanglement/domoic acid delay" )
open_with_restrictions <- c("25% gear reduction",                                    
                            "30-fathom depth restriction",                          
                            "30-fathom depth restriction/25% gear reduction",         
                            "30-fathom depth restriction/50% gear reduction",       
                            "33% gear reduction",                                    
                            "40-fathom depth restriction",                         
                            "40-fathom depth restriction/20% gear reduction",         
                            "50% gear reduction",
                            "Evisceration order",                                
                            "Evisceration order (+depth restriction/gear reduction)",
                            "25% gear reduction",                               
                            "30-fathom depth restriction")
statuses[!statuses %in% c(closed, open_with_restrictions, open)]

  
# Summarize data
stats <- data %>% 
  # What date did it open
  group_by(lat_bin, season) %>% 
  summarize(date_open=min(date[status=="Season open"]),
            date_notclosed=min(date[status %in% c(open, open_with_restrictions )])) %>% 
  ungroup() %>% 
  # Add date it should have opened
  mutate(date_open_scheduled=case_when(grepl("WA", lat_bin) ~ paste0(season, "-12-01"),
                                       grepl("OR", lat_bin) ~ paste0(season, "-12-01"),
                                       lat_bin %in% c("CA1", "CA2", "CA-1", "CA-2") ~ paste0(season, "-12-01"), # Northern CA
                                       lat_bin %in% c(paste0("CA", 3:6), paste0("CA-", 3:6)) ~ paste0(season, "-11-15"), # Southern CA
                                       T ~ NA),
         date_open_scheduled=ymd(date_open_scheduled)) %>% 
  # Calculate difference
  mutate(nday_delay_open=as.numeric(date_open- date_open_scheduled),
         nday_delay_notclosed=as.numeric(date_notclosed - date_open_scheduled)) %>% 
  # Arrange
  select(lat_bin, season, date_open_scheduled, everything())

# Export
################################################################################

outfile <- ifelse(version=="real",
                  "day_zone_opens_by_season.csv",
                  "day_zone_opens_by_season_jameal.csv")

# Export
write.csv(stats, file=file.path(outdir, outfile), row.names = F)
  