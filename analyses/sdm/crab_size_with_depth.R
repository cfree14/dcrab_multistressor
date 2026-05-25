

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sdmTMB)
library(visreg)
library(tidyverse)

# Directories
trawldir <- "data/trawl_survey/processed"
oceandir <- "data/live_ocean/processed"
outdir <- "output/sdm"
plotdir <- "analyses/sdm/figures"

# Read data
data_orig <- readRDS(file=file.path(trawldir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))

# Build data
################################################################################

data <- data_orig %>% 
  # Average crab weight
  mutate(crab_kg=total_kg/total_n) %>% 
  # Only with crab
  filter(crab_kg>0 & is.finite(crab_kg)) %>% 
  # Convert depth to fathomgs
  mutate(depth_fa=measurements::conv_unit(depth_m, "m", "ft")/6)

res <- 0.1
stats <- data %>% 
  mutate(lat_dd_bin=floor(lat_dd/res)*res,
         long_dd_bin=floor(long_dd/res)*res) %>% 
  group_by(lat_dd_bin, long_dd_bin) %>% 
  summarize(ntow=n(), 
            crab_kg=mean(crab_kg, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(crab_kg_cap=pmin(crab_kg,1))





# Build data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    plot.tag = element_text(size=8),
                    plot.title=element_blank(),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size=unit(0.3, "cm"),
                    legend.key=element_blank(),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

g1 <- ggplot() +
  # Data
  geom_tile(stats, mapping=aes(x=long_dd_bin,
                               y=lat_dd_bin, 
                               fill=crab_kg_cap)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Legend
  scale_fill_gradientn(name="Average body size", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -116.5), ylim = c(32.5, 48.5)) +
  # Labels
  labs(tag="A", x="", y="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.2, 0.2))
g1

# Plot relative to depth
g2 <- ggplot(data, aes(y=crab_kg, x=depth_fa)) +
  geom_point() + 
  geom_smooth() +
  # Labels
  labs(x="Depth (fathoms)", y="Average crab size (kg)", tag="B") +
  # Axes
  lims(y=c(0, 2)) +
  # Theme
  theme_bw() + base_theme

# Crab body size distribution
g3 <- ggplot(data, aes(x=crab_kg)) + 
  geom_density() +
  # Labels
  labs(x="Average crab size (kg)", y="Frequency", tag="C") +
  # Axes
  lims(x=c(0, 2)) +
  # Theme
  theme_bw() + base_theme
g3

# Matrix
layout_matrix <- matrix(data=c(1,2,
                               1,3), nrow=2, ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_crab_size_with_depth.png"), 
       width=6.5, height=6.5, units="in", dpi=600, bg="white")



