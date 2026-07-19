
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Read blocks
blocks_orig <- sf::st_read("/Users/cfree/Dropbox/Chris/UCSB/projects/pot-fisheries-dynamics/data/spatial/grid/ten_arcminute_grid_shore_lamb.shp")

# Latitude breaks
# lats_orig <- readxl::read_excel("data/mgmt_areas/WC_dcrab_da_mgmt_zones.xlsx")
# lat_breaks <- c(lats_orig$lat_dd_north, lats_orig$lat_dd_south) %>% sort() %>% unique() %>% na.omit()
lats_orig <- readxl::read_excel("data/mgmt_areas/WC_dcrab_da_mgmt_zones.xlsx") %>% filter(state!="California")
lat_breaks <- c(lats_orig$lat_dd_north, lats_orig$lat_dd_south,
                32,
                34+27/60,
                36,
                37+11/60,
                38+46.125/60,
                40+10/60,
                42) %>% sort() %>% unique() %>% na.omit()

# Latitude breaks - Jameal
lat_breaks_jameal <- c(32,
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

# Setup
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="large")

# Derive polygon centroids
centroids <- blocks_orig %>%
  st_centroid() %>%
  st_transform(crs = sf::st_crs(usa)) %>%
  st_coordinates() |> 
  as_tibble()

# Format blocks
blocks <- blocks_orig |> 
  # Project
  sf::st_transform(crs = sf::st_crs(usa)) |> 
  # Add centroid
  mutate(lat_dd=centroids$Y) |> 
  # Mark real zone
  mutate(zone=cut(lat_dd, lat_breaks, labels=c(paste0("CA-", 6:1), 
                                               paste0("OR-50-", LETTERS[12:1]), 
                                               c("WA-60A-2", "WA-60A-2", "WA-59A-2", "WA-59A-1")))) |> 
  # Mark Jameal zone
  mutate(zone_jameal=cut(lat_dd, lat_breaks_jameal, labels=c("CA6", "CA5", "CA4", "CA3", "CA2", "CA1",
                                                             "OR1", "OR2", "OR3",
                                                             "WA1", "WA2", "WA3")))
  

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
                   legend.key.size=unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot blocks
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, mapping=aes(fill=zone_jameal), color="grey30", lwd=0.2) +
  # Reference lines
  geom_hline(yintercept=lat_breaks_jameal) +
  # Plot land
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  # Crop
  coord_sf(xlim=c(-130, -116), ylim=c(30, 50)) +
  # Labels 
  labs(title="Jameal zones") +
  # Legend
  scale_fill_discrete(name="Zone") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = c(0.8, 0.8))
g1

g2 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, mapping=aes(fill=zone), color="grey30", lwd=0.2) +
  # Reference lines
  geom_hline(yintercept=lat_breaks) +
  # Plot land
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  # Crop
  coord_sf(xlim=c(-130, -116), ylim=c(30, 50)) +
  # Labels 
  labs(title="Chris zones") +
  # Legend
  scale_fill_discrete(name="Zone") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Project back
blocks_out <- blocks |> 
  sf::st_transform(crs=sf::st_crs(blocks_orig))

# Export
sf::st_write(blocks_out, dsn="analyses/trt_paper/output/ten_arcminute_grid_shore_lamb_with_zones.shp")
