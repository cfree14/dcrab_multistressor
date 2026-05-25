



# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directies
datadir <- "data/trawl_survey/processed"
outdir <- "analyses/sdm/output"
plotdir <- "analyses/sdm/figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))
footprint_orig <- readRDS(file=file.path(datadir, "nwfsc_wcbts_grid_cells.Rds"))

load(file.path(outdir, "index_output_20km_mesh.Rdata"))


# Build data
################################################################################

# Ensure both dataframes are in the same CRS
data_sf <- st_as_sf(data_orig, coords = c("long_dd", "lat_dd"), crs = st_crs(footprint_orig))

# Perform spatial join to get the unique ID from footprint_orig
data_joined <- st_join(data_sf, footprint_orig["station_id"])

# Convert back to a dataframe if needed
data <- data_orig %>% 
  mutate(station_id=data_joined$station_id)

# Station id stats
stats <- data %>% 
  count(station_id)

# Add stats to dataframe
footprint <- footprint_orig %>% 
  left_join(stats)


# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_blank(),
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

# Plot
g1 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot surveys
  geom_sf(data=footprint, fill="grey30", color="grey30") +
  # Labels
  labs(x="", y="", tag="A") +
  scale_y_continuous(breaks = seq(30, 50, 2)) +
  # Crop
  coord_sf(xlim = c(-126, -116.5), ylim = c(32.5, 48.5)) +
  # Theme
  theme_bw() + base_theme
g1


# Plot mesh
################################################################################

# 1. Extract the mesh locations (vertices)
locs <- mesh$mesh$loc[, c(1, 2)] *1000
nodes_sf <- st_as_sf(as.data.frame(locs), coords = c("V1", "V2"), crs = 32610) %>% 
  sf::st_transform(crs=sf::st_crs(usa))

# 2. Extract the triangle connections
triangles <- mesh$mesh$graph$tv

# 3. Create spatial polygons for every triangle in the mesh
mesh_polys <- lapply(1:nrow(triangles), function(i) {
  idx <- triangles[i, ]
  # Close the polygon by repeating the first vertex at the end
  pts <- locs[c(idx, idx[1]), ] 
  st_polygon(list(pts))
})

# 4. Turn into an sf object (Specify your project's original CRS/UTM here)
mesh_edges_sf <- st_sf(geometry = st_sfc(mesh_polys, crs = 32610)) %>% 
                         sf::st_transform(crs=sf::st_crs(usa))

g2 <- ggplot() +
    # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # 2. Layer the triangulation lines on top
  geom_sf(data = mesh_edges_sf, fill = NA, linewidth = 0.3, alpha = 0.5) +
  # 3. Optional: Add the knot locations as points
  geom_sf(data =nodes_sf, size = 0.8) +
  # Labels
  labs(x="", y="", tag="B") +
  scale_y_continuous(breaks = seq(30, 50, 2)) +
  # Crop
  coord_sf(xlim = c(-126, -116.5), ylim = c(32.5, 48.5)) +
  # Theme
  theme_bw() + base_theme
g2

g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_mesh.png"), 
       width=5.5, height=5.5, units="in", dpi=600)


