

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/confidential/oregon/processed/"
plotdir <- "analyses/fishing_grounds/figures"
outdir <- "analyses/fishing_grounds/output"

# Read data
receipts_orig <-  readRDS(file=file.path(datadir, "ODFW_1980_2023_dcrab_fish_tickets.Rds"))

# Read data
load("analyses/sdm/output/index_output_20km_mesh.Rdata")


# Build data
################################################################################

# Index data
index_df <- indexes %>% 
  # Factor
  filter(region=="Oregon") %>% 
  # Convert to metric tons
  mutate(index_mt=est/1000,
         index_mt_lo=lwr/1000,
         index_mt_hi=upr/1000) %>% 
  # Select
  select(year, index_mt) %>% 
  mutate(year_lag1=year+1,
         year_lag2=year+2,
         year_lag3=year+3,
         year_lag4=year+4)

# Landings data
landings <- receipts_orig %>%
  filter(!is.na(season)) %>% 
  group_by(season) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm = T)) %>% 
  ungroup() %>% 
  rename(season_long=season) %>% 
  mutate(season_short=substr(season_long, 1, 4) %>% as.numeric(.)) %>% 
  select(season_long, season_short, landings_lbs, everything(.))

# Plot landings
ggplot(landings, aes(x=season_short, y=landings_lbs/1e6)) +
  geom_bar(stat="identity", fill="grey70") +
  geom_line(data=index_df, mapping=aes(x=year, y=index_mt/1e3/2)) +
  geom_point(data=index_df, mapping=aes(x=year, y=index_mt/1e3/2)) +
  # Labels
  labs(x="Year", y="Landings (millions lbs)") +
  scale_x_continuous(breaks=seq(1980,2025,5)) +
  # Theme
  theme_bw()


# Build data
data <- landings %>% 
  # Add index for that year
  left_join(index_df %>% select(year, index_mt), by=c("season_short"="year")) %>% 
  rename(index_lag0=index_mt) %>% 
  # Add index lag 1
  left_join(index_df %>% select(year_lag1, index_mt), by=c("season_short"="year_lag1")) %>%
  rename(index_lag1=index_mt) %>% 
  # Add index lag 2
  left_join(index_df %>% select(year_lag2, index_mt), by=c("season_short"="year_lag2")) %>%
  rename(index_lag2=index_mt) %>% 
  # Add index lag 3
  left_join(index_df %>% select(year_lag3, index_mt), by=c("season_short"="year_lag3")) %>%
  rename(index_lag3=index_mt) %>% 
  # Add index lag 4
  left_join(index_df %>% select(year_lag4, index_mt), by=c("season_short"="year_lag4")) %>%
  rename(index_lag4=index_mt)


# Lag 1 complete
data_lag1 <- data %>% 
  select(season_short, landings_lbs, index_lag0, index_lag1) %>% 
  filter(season_short>=2006) %>% 
  na.omit()
lmfit1 <- lm(landings_lbs ~  index_lag0 + index_lag1, data_lag1)
summary(lmfit1)

# Lag 2
data_lag2 <- data %>% 
  select(season_short, landings_lbs, index_lag0, index_lag1, index_lag2) %>% 
  filter(season_short>=2006) %>% 
  na.omit()
lmfit2 <- lm(landings_lbs ~  index_lag0 + index_lag1 + index_lag2, data_lag2)
summary(lmfit2)

# Lag 3
data_lag3 <- data %>% 
  select(season_short, landings_lbs, index_lag0, index_lag1, index_lag2, index_lag3) %>% 
  filter(season_short>=2006) %>% 
  na.omit()
lmfit3 <- lm(landings_lbs ~  index_lag0 + index_lag1 + index_lag2 + index_lag3, data_lag3)
summary(lmfit3)

# Lag 4
data_lag4 <- data %>% 
  filter(season_short>=2006) %>% 
  na.omit()
lmfit4 <- lm(landings_lbs ~ index_lag0 + index_lag1 + index_lag2 + index_lag3 + index_lag4, data_lag4)
summary(lmfit4)



# Merge
data <- landings %>% 
  left_join(index_df) %>% 
  mutate(type=ifelse(year %in% c(2015, 2016, 2018, 2019, 2023, 2024), "EL Nino", "Not El Nino"))

ggplot(data, aes(x=index_mt, y=landings_lbs, color=type)) +
  geom_point() +
  geom_text(mapping=aes(label=year))

