

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
  # Arrange
  select(year:landings_mt_or_log_est,
         pdo_jan_jul, pdo_aug_sep,
         enso_year, enso_jan_jul,
         cuti_mar_jul, 
         spring_trans_yday,
         n_late, n_late_log)

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



