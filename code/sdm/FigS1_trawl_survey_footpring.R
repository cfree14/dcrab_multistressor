



# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directies
datadir <- "data/trawl_survey/raw"
outdir <- "data/trawl_survey/processed"
plotdir <- "data/trawl_survey/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))
footprint_orig <- readRDS(file=file.path(outdir, "nwfsc_wcbts_grid_cells.Rds"))


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

# Boxes
bboxs <- tibble(box=c("A", "B", "C"),
                xmin=c(-126, -125, -122)-0.25,
                xmax=c(-121, -120, -117)+0.25,
                ymin=c(42, 34.5, 32)-0.25,
                ymax=c(48.5, 42, 34.5)+0.25)

# Plot
g1 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot surveys
  geom_sf(data=footprint, fill="grey30", color="grey30") +
  # Boxes
  geom_rect(data=bboxs, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="black", fill=NA) +
  # Labels
  labs(x="", y="", tag="A") +
  scale_y_continuous(breaks = seq(30, 50, 2)) +
  # Crop
  coord_sf(xlim = c(-126, -116.5), ylim = c(32.5, 48.5)) +
  # Theme
  theme_bw() + base_theme
g1

# Plot
g2 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot surveys
  geom_sf(data=footprint, mapping=aes(fill=n), color="grey40", linewidth=0.05) +
  # Labels
  labs(x="", y="", tag="B", title="Washington / Oregon") +
  # Plot legend
  scale_fill_gradientn(name="# tows", na.value = NA,
                        colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.1)) +
  # Crop
  coord_sf(xlim = c(-126, -121), ylim = c(42, 48.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8,0.8))
g2

# Plot
g3 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot surveys
  geom_sf(data=footprint, mapping=aes(fill=n), color="grey40", linewidth=0.05) +
  # Labels
  labs(x="", y="", tag="C", title="N. California") +
  # Plot legend
  scale_fill_gradientn(name="# tows", na.value = NA,
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.1)) +
  # Crop
  coord_sf(xlim = c(-125, -120), ylim = c(34.5, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8,0.8))
g3

# Plot
g4 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot surveys
  geom_sf(data=footprint, mapping=aes(fill=n), color="grey40", linewidth=0.05) +
  # Labels
  labs(x="", y="", tag="D", title="S. California") +
  scale_y_continuous(breaks = seq(32, 35, 1)) +
  # Plot legend
  scale_fill_gradientn(name="# tows", na.value = NA,
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.1)) +
  # Crop
  coord_sf(xlim = c(-122, -117), ylim = c(32, 34.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.1,0.4))
g4

# Merge
layout_matrix <- matrix(data=c(1,2,3,
                               1,2,3,
                               1,4,4), ncol=3, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix)


# Export
ggsave(g, filename=file.path(plotdir, "FigX_trawl_survey_footprint.png"), 
       width=6.5, height=5.5, units="in", dpi=600)


