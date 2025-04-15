

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
# datadir <- "data/confidential/washington/processed/"
datadir <- "data/confidential/oregon/processed/"
plotdir <- "figures/sdm"

# Read data
# data_orig <- readRDS(file=file.path(datadir, "WDFW_2010_2020_dcrab_logbooks.Rds"))
data_orig <- readRDS(file=file.path(datadir, "ODFW_2007_2022_dcrab_logbooks.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Lots to do
# Order ports north to south
# Outliers (longitude outliers hidden below)
# avergage up /down?


# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=7),
                    plot.tag=element_text(size=8),
                    plot.title=element_blank(),
                    plot.subtitle = element_text(size=5, face="italic"),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size=unit(0.3, "cm"),
                    legend.key=element_blank(),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_orig, mapping=aes(x=long_dd_set, y=lat_dd_set)) +
  facet_wrap(~port, ncol=6) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Data
  geom_point() +
  # Crop
  coord_sf(xlim = c(-126, -122), ylim = c(41, 48)) +
  # Theme
  theme_bw() + base_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_fishing_grounds_or.png"),
       width=6.5, height=6.25, units="in", dpi=600)


