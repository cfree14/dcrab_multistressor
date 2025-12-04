

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(exactextractr) # efficient zonal stats

# Directories
datadir <- "data/confidential/washington/processed/"
plotdir <- "analyses/fishing_grounds/figures"
outdir <- "analyses/fishing_grounds/output"

# Read data
fg_orig <- readRDS(file=file.path(outdir, "fishing_grounds.Rds"))

# Read GLORYS
# glorys_raw <- terra::rast("data/glorys/raw/glorys.nc")
glorys_orig <- raster::brick("data/glorys/processed/glorys_annual_avg_temp.tiff")


# Compute average temperature in fishing grounds
################################################################################

# Extract mean temperature per polygon for each year
# exactextractr is fast and handles partial cell overlaps
avg_temps <- exactextractr::exact_extract(glorys_orig, fg_orig, 'mean')

# Combine with fishing ground IDs
avg_temps_df <- bind_cols(fg_orig %>% sf::st_drop_geometry(), avg_temps)

# Reshape to long format if raster has multiple years
avg_temps_long <- avg_temps_df %>%
  # Gather
  gather(key="year", value="bottomT_c", 4:ncol(.)) %>% 
  # Format year
  mutate(year=gsub("mean.X", "", year) %>% as.numeric()) %>% 
  # Arrange
  arrange(state, port, percentile, year)

# Inspect results
str(avg_temps_long)


# Export
################################################################################

# Save results
saveRDS(avg_temps_long, file.path(outdir, "fishing_grounds_glorys_bottomT_avg.Rds"))



# Quick visual
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(avg_temps_long %>% filter(percentile==95), 
       mapping=aes(x=year, y=bottomT_c, color=port)) +
  facet_wrap(~state) +
  geom_line(linewidth=0.4) +
  # Labels
  labs(x="Year", y="Bottom temperature (°C)") +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_fishing_grounds_glorys_bottomT.png"),
       width=6.5, height=3, units="in", dpi=600)






