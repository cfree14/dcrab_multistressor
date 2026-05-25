



# Clear workspace
rm(list = ls())

# Setup
################################################################################

# install.packages("remotes")
# remotes::install_github("pfmc-assessments/nwfscSurvey")

# FRAM warehouse
# https://www.webapps.nwfsc.noaa.gov/data/map

# Packages
library(tidyverse)
library(nwfscSurvey)

# Directies
datadir <- "data/trawl_survey/raw"
outdir <- "data/trawl_survey/processed"
plotdir <- "data/trawl_survey/figures"


# Export
data_orig <- readRDS(data, file=file.path(outdir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))


# Build
################################################################################

# Build
res <- 0.05
data <- data_orig %>% 
  # Reduce
  filter(state=="Oregon") %>% 
  # Bin into raster
  mutate(lat_dd_bin=floor(lat_dd/res)*res,
         long_dd_bin=floor(long_dd/res)*res) %>% 
  group_by(lat_dd_bin, long_dd_bin) %>% 
  # P(occur)
  summarize(ntow=n(), 
            ntow_crab=sum(total_n>0 | total_kg>0)) %>% 
  ungroup() %>% 
  mutate(poccur=ntow_crab/ntow) %>% 
  filter(poccur>0)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

g1 <- ggplot() +
  # Data
  geom_tile(data, mapping=aes(x=long_dd_bin,
                               y=lat_dd_bin, 
                               fill=poccur)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Legend
  scale_fill_gradientn(name="P(occur)", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -122), ylim = c(41, 46.5)) +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw()
g1

# Clauclate areas
df <- data
res_x <- min(diff(sort(unique(df$long_dd_bin))))
res_y <- min(diff(sort(unique(df$lat_dd_bin))))


# Create raster template
r <- terra::rast(
  xmin = min(df$long_dd_bin) - res_x/2,
  xmax = max(df$long_dd_bin) + res_x/2,
  ymin = min(df$lat_dd_bin) - res_y/2,
  ymax = max(df$lat_dd_bin) + res_y/2,
  resolution = c(res_x, res_y),
  crs = "EPSG:4326"
)

xy <- df |>
  dplyr::select(long_dd_bin, lat_dd_bin) |>
  as.data.frame() |>
  as.matrix()

# Convert points to raster cells
cells <- terra::cellFromXY(r, xy)


# Fill only observed cells
terra::values(r) <- NA
r[cells] <- 1

# Calculate cell areas (km2)
a <- terra::cellSize(r, unit = "km")

# Sum area of non-missing cells
total_area_km2 <- terra::global(a * !is.na(r), "sum", na.rm = TRUE)

total_area_km2


total_area_km2 * 1.2
