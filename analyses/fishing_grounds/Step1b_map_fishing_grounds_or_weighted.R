

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/confidential/oregon/processed/"
plotdir <- "analyses/fishing_grounds/figures"
outdir <- "analyses/fishing_grounds/output"

# Read data
data_orig <- readRDS(file=file.path(datadir, "ODFW_2007_2022_dcrab_logbooks.Rds"))
receipts_orig <-  readRDS(file=file.path(datadir, "ODFW_1980_2023_dcrab_fish_tickets.Rds"))

# Read ports
ports_orig <- readxl::read_excel(file.path(datadir, "port_key.xlsx"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Consider level of reporting
################################################################################

# Format data
data <- data_orig %>% 
  mutate(season_short=substr(season, 1, 4) %>% as.numeric(.))

stats <- data %>% 
  group_by(season_short, data_entry_protocol) %>% 
  summarize(n=n()) %>% 
  ungroup()

ggplot(stats, aes(x=season_short, y=n, fill=data_entry_protocol)) +
  geom_col() +
  theme_bw()


# Reduce to seasons with full reporting
################################################################################

# Trips
trips <- data %>% 
  # Years with full entry
  filter(season_short %in% c(2007:2010, 2015, 2018:2021)) %>% 
  # Only trips with GPS points
  filter(!is.na(long_dd) & !is.na(lat_dd))

# Convert to sf object
trips_sf <- sf::st_as_sf(trips, coords = c("long_dd", "lat_dd"), crs = 4326)

# Reproject to a suitable projected CRS for distance calculations (e.g., UTM)
trips_sf <- sf::st_transform(trips_sf, 32610) # Example: UTM Zone 10N for U.S. West Coast

# Convert to sp
trips_sp <- as(trips_sf, "Spatial")

# Estimate kernel utilization distribution
kud <- adehabitatHR::kernelUD(trips_sp, h = "href", grid = 500)  # 'href' is the bandwidth rule of thumb

# Extract the 95% contour polygon
contour50 <- adehabitatHR::getverticeshr(kud, percent = 50)
contour95 <- adehabitatHR::getverticeshr(kud, percent = 95)
contour99 <- adehabitatHR::getverticeshr(kud, percent = 99)

# Convert back to sf for plotting
contour50_sf <- sf::st_as_sf(contour50) %>% mutate(percentile=50)
contour95_sf <- sf::st_as_sf(contour95) %>% mutate(percentile=95)
contour99_sf <- sf::st_as_sf(contour99) %>% mutate(percentile=99)
contours <- bind_rows(contour50_sf, contour95_sf, contour99_sf) %>% 
  sf::st_transform(sf::st_crs(usa))

# Export
saveRDS(contours, file=file.path(outdir, "fishing_grounds_or_full_entry.Rds"))


# Plot data
ggplot() +  
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Countours
  geom_sf(data = contours, mapping=aes(fill = percentile), alpha = 0.3) +
  # Trips
  geom_sf(data = trips_sf, color = "black", size = 0.5, alpha = 0.5, pch="x") +
  # Crop
  coord_sf(xlim = c(-125, -123.5), ylim = c(42, 48)) +
  # Theme
  theme_bw()

# Plot data
ggplot() +  
  # Contours
  geom_sf(data = contours, mapping=aes(fill = as.character(percentile)), alpha = 0.3) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-127, -123), ylim = c(41.5, 47)) +
  # Legend
  scale_fill_ordinal(name="Fishing grounds") +
  # Theme
  theme_bw()


# Reduce to seasons with full reporting and expand based on pots
################################################################################


# Convert points to sf and project
pts <- data %>%
  filter(season_short %in% c(2007:2010, 2015, 2018:2021)) %>% 
  filter(!is.na(long_dd),
         !is.na(lat_dd),
         !is.na(npots),
         npots > 0) %>%
  sf::st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326) %>%
  sf::st_transform(32610)   # UTM zone 10N; change if needed

# Template raster
r <- terra::rast(
  terra::ext(terra::vect(pts)) + 20000,   # add buffer around points
  resolution = 1000,        # meters
  crs = sf::st_crs(pts)$wkt
)

# Rasterize effort
effort <- terra::rasterize(
  terra::vect(pts),
  r,
  field = "npots",
  fun = "sum",
  background = 0
)

# Gaussian smoothing kernel
kernel <- terra::focalMat(
  effort,
  d = 10000,        # bandwidth in meters
  type = "Gauss"
)

# Smooth effort surface
ud <- terra::focal(
  effort,
  w = kernel,
  fun = sum,
  na.policy = "omit",
  fillvalue = 0
)

# Normalize to probability surface
ud <- ud / terra::global(ud, "sum", na.rm = TRUE)[1, 1]

# Convert raster cells to dataframe
ud_df <- as.data.frame(ud, xy = TRUE, na.rm = TRUE)
names(ud_df) <- c("x", "y", "prob")

# Calculate cumulative probability from highest-use cells downward
ud_df <- ud_df %>%
  arrange(desc(prob)) %>%
  mutate(cum_prob = cumsum(prob))

# Convert cumulative probability back to raster
cum_r <- terra::rast(
  ud_df[, c("x", "y", "cum_prob")],
  type = "xyz",
  crs = terra::crs(ud)
)

# Extract 50% and 95% contours
contours <- terra::as.contour(cum_r, levels = c(0.50, 0.95))

# Convert to sf
contours_sf <- sf::st_as_sf(contours)

# 50% and 95% areas as raster masks
hr50 <- cum_r <= 0.50
hr95 <- cum_r <= 0.95

# Convert to polygons
hr50_poly <- terra::as.polygons(hr50, dissolve = TRUE) %>%
  sf::st_as_sf() %>%
  filter(cum_prob == 1) %>%
  mutate(percentile = 50)

hr95_poly <- terra::as.polygons(hr95, dissolve = TRUE) %>%
  sf::st_as_sf() %>%
  filter(cum_prob == 1) %>%
  mutate(percentile = 95)

home_ranges <- bind_rows(hr50_poly, hr95_poly) %>% 
  sf::st_transform(sf::st_crs(usa))

# Plot
ggplot() +
  geom_sf(data = home_ranges, aes(color = percentile), fill=NA, linewidth = 1) +
  # geom_sf(data = pts, aes(size = npots), alpha = 0.4) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-127, -123), ylim = c(41.5, 47)) +
  theme_bw() +
  labs(color = "Contour",
       size = "Number of pots")


saveRDS(home_ranges, file=file.path(outdir, "fishing_grounds_or_weighted.Rds"))




