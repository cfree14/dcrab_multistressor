

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/megalope/raw"
outdir <- "data/megalope/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Meg and sumamry ocean data (version 1).xlsx"), sheet=1)

# Read Richerson
b0_orig <- readxl::read_excel("data/richerson/crab_model_results_2020422.xlsx") %>% 
  filter(area=="OR") %>% 
  rename(year=season,
         biomass_mt=mean_est_thousands_mt) %>% 
  mutate_at(vars(year:landings_thousands_mt), as.numeric) %>% 
  mutate(biomass_mt=biomass_mt*1000) %>% 
  mutate(year_rec=year+1) %>% 
  select(year_rec, biomass_mt)

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(year=yr,
         n_megalope=mega,
         n_megalope_log=log_mega, 
         landings_mt_or=or_metric_ton,
         landings_mt_or_log=or_log_metric_ton,
         landings_mt_or_log_est=or_log_est_landings_metric_ton,
         pdo_jan_jul=pdo_sum_jan_july,
         pdo_aug_sep=sum_aug_sept_pdo,
         cuti_mar_jul=cuti_index_march_july,
         enso_year=enso_yr_sum_mei_v2,
         enso_jan_jul=enso_jan_july_sum_mei_v2,
         spring_trans_yday=spring_trans_doy,
         n_late=number_late,
         n_late_log=log_late)  %>% 
  # Add B0
  left_join(b0_orig, by=c("year"="year_rec")) %>% 
  # Arrange
  select(year, biomass_mt, n_megalope:landings_mt_or_log_est,
         pdo_jan_jul, pdo_aug_sep,
         enso_year, enso_jan_jul,
         cuti_mar_jul, 
         spring_trans_yday,
         n_late, n_late_log, everything())

# Inspect
str(data)

# Plot data
g1 <- ggplot(data, aes(x=year, y=n_megalope/1e6, fill=enso_year)) +
  # Plot data
  geom_line() +
  geom_point(pch=21, size=2) +
  # Labels
  labs(x="Year", y="Millions of megalope") +
  # Axes
  # Legend
  scale_fill_gradientn(name="ENSO index", colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g1

# Export data
saveRDS(data, file=file.path(outdir, "1997_2023_oregon_megalope_data.Rds"))




# Plot data
g1 <- ggplot(data, aes(x=biomass_mt, y=n_megalope/1e6, fill=spring_trans_yday)) +
  # Plot data
  geom_point(pch=21, size=2) +
  # Labels
  labs(x="Pre-season legal-sized male biomass (mt)", y="Millions of megalope") +
  # Axes
  lims(x=c(0, NA)) +
  # Legend
  scale_fill_gradientn(name="ENSO index", colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g1



