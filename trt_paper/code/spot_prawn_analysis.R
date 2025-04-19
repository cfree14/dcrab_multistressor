

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
cadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landing_receipts_2023/processed/"

# Directories
plotdir <- "trt_paper/figures"
outdir <- "trt_paper/output"

# Read data
data_orig <- readRDS(file=file.path(cadir, "1980_2022_landings_receipts.Rds"))

# Blocks
blocks <- wcfish::blocks %>% 
  sf::st_drop_geometry() %>% 
  select(block_id, block_lat_dd) %>% 
  mutate(region=ifelse(block_lat_dd<=34.577211, "South", "North"))
  

# Build data
################################################################################

# Gear stats
gear_stats <- data_orig %>% 
  filter(comm_name %in% c("Spot prawn")) %>% 
  group_by(comm_name, gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(comm_name, desc(landings_lbs)) %>% 
  group_by(comm_name) %>% 
  mutate(prop=landings_lbs/sum(landings_lbs)*100)

# Gear stats
gear_stats_yr <- data_orig %>% 
  filter(comm_name %in% c("Spot prawn")) %>% 
  group_by(comm_name, year, gear_type, gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(comm_name, desc(landings_lbs)) %>% 
  group_by(comm_name, year) %>% 
  mutate(prop=landings_lbs/sum(landings_lbs)*100)

# Inspect gear over time
ggplot(gear_stats_yr, aes(x=year, y=prop, fill=gear)) +
  geom_bar(stat="identity") +
  theme_bw()

# Gear type over time
# Traps weren't sizeable until 1986
ggplot(gear_stats_yr, aes(x=year, y=prop, fill=gear_type)) +
  geom_bar(stat="identity") +
  theme_bw()

# Sablefish gears
# This was informed by the analysis above
gears_prawn <- c("Prawn trap", "Entrapping")

# Build data
data <- data_orig %>% 
  # Species and gears
  filter(comm_name=="Spot prawn" & gear %in% gears_prawn) %>% 
  # Add region
  left_join(blocks, by="block_id") %>% 
  # Record month and mark whether imputed
  mutate(month=month(date),
         region_imputed_yn=is.na(region)) %>% 
  # Impute the very few missing regions
  #this works b/c 2 regions were never visited in same month
  group_by(vessel_id, year, month) %>% 
  mutate(region=ifelse(is.na(region), unique(region), region)) %>% # 
  ungroup() %>% 
  # Get rid of missing blocks (could impute if you want)
  filter(!is.na(region)) %>% 
  # Add yday and week
  mutate(yday=yday(date),
         yweek=floor(yday/7))
  
# Inspect
freeR::complete(data)
sum(data$region_imputed_yn) / nrow(data)
    
# Summarize
stats <- data %>% 
  # Summarize
  group_by(region, year, yweek) %>% 
  summarize(nvessels=n_distinct(vessel_id)) %>% 
  ungroup()

# Add missing zeros
stats_full <- expand.grid(region=c("South", "North"),
                         year=1980:2022,
                         yweek=0:52) %>% 
  left_join(stats) %>% 
  mutate(nvessels=ifelse(is.na(nvessels), 0, nvessels)) 

ggplot(stats_full, aes(x=yweek, y=nvessels, color=region, group=region)) +
  facet_wrap(~year, ncol=9) +
  geom_line() +
  # Legend
  scale_color_discrete(name="Region") +
  # Labels
  labs(x="Year week", y="Number of vessels") +
  # Theme
  theme_bw() + 
  theme(legend.position = "top")

# Add missing zeros
stats_use <- stats_full %>% 
  filter(year>=1987) %>% 
  mutate(ntraps=nvessels*500)

# Export data
saveRDS(stats_use, file=file.path(outdir, "spot_prawn_traps_by_week.Rds"))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.tag = element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.3, "cm"),
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats_use, aes(x=yweek, y=nvessels*500, color=region, group=region)) +
  facet_wrap(~year, ncol=9) +
  geom_line(linewidth=0.3) +
  # Legend
  scale_color_discrete(name="Region") +
  # Labels
  labs(x="Year week", y="Number of vertical lines") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_spot_prawn_temporal.png"), 
       width=6.5, height=6, units="in", dpi=600)




