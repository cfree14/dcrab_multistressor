

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/nina"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "WilsonRequest_PCRGData.xlsx"), na = c("NA", "-", "n/a", "?", "na"), col_types = "text")
sites_orig <- readxl::read_excel(file.path(datadir, "WilsonRequest_PCRGData.xlsx"), sheet=2)

# TO-DO
# Clean start/end times
# Clean hours fished
# Compute CPUE
# Plot checks

# Clean data
################################################################################

# Clean sites
sites <- sites_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(site=site_name, 
         lat_dd=latitide,
         long_dd=longitude) %>% 
  # Simplify
  select(site_code, site, site_type, organization, lat_dd, long_dd)

# Inspect
str(sites)

# Clean data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(date_orig=date,
         proper_fishing_yn=did_the_trap_fish_properly_y_n,
         time_start=timer_start,  
         time_end=timer_end, 
         n_nights=number_nights_fished,
         n_hours=hours_fished,
         subsample_yn=subsample,
         n_megalope=metacarcinus_magistermegalopae,
         n_instar=metacarcinus_magisterinstar) %>% 
  # Convert to numeric
  mutate(n_nights=as.numeric(n_nights),
         n_megalope=as.numeric(n_megalope),
         n_instar=as.numeric(n_instar)) %>% 
  # Format site
  mutate(site=toupper(site)) %>% 
  # Proper fishing
  mutate(proper_fishing_yn=recode(proper_fishing_yn,
                                  "y"="Y",
                                  "n"="N")) %>% 
  # Subsample
  mutate(subsample_yn=toupper(subsample_yn) %>% substr(., 1, 1)) %>% 
  # Format date
  mutate(date_orig=recode(date_orig,
                          "21090804"="20190804"),
         date_length=nchar(date_orig),
         date1=ifelse(date_length==5, date_orig, NA),
         date2=ifelse(date_length>5, date_orig, NA),
         date1=as.numeric(date1) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.) %>% as.character(),
         date2=ymd(date2) %>% as.character(),
         date=ifelse(!is.na(date1), date1, date2) %>% ymd(.)) %>% 
  # Remove useless
  select(-c(date_length, date1, date2)) %>% 
  select(date_orig, date, everything())

# Inspect
freeR::complete(data)
range(data$date)
str(data)
table(data$site)
table(data$proper_fishing_yn)
table(data$subsample_yn)

# Export data
saveRDS(data, file=file.path(datadir, "nina_data_cleaned.Rds"))
write.csv(data, file=file.path(datadir, "nina_data_cleaned.csv"), row.names = F)


