

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



# Reduce to seasons with full reporting and expand based on pots
################################################################################

# Function to maps home ranges by season
# season_do <- 2007
map_home_range <- function(season_do){

  # Convert points to sf and project
  pts <- data %>%
    filter(season_short == season_do) %>% 
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
  
  # # Extract 50% and 95% contours
  # contours <- terra::as.contour(cum_r, levels = c(0.50, 0.95))
  # # Convert to sf
  # contours_sf <- sf::st_as_sf(contours)
  
  x <- 0.5
  perc <- seq(0, 1,0.05)
  hrs <- purrr::map_df(perc, function(x){
    
    # 50% and 95% areas as raster masks
    hr <- cum_r <= x

    # Convert to polygons
    hr_poly <- terra::as.polygons(hr, dissolve = TRUE) %>%
      sf::st_as_sf() %>%
      filter(cum_prob == 1) %>%
      mutate(percentile = x*100)
    
  })
  
  # Merge
  home_ranges <- hrs %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(sf::st_crs(usa)) %>% 
    mutate(season=season_do) %>% 
    select(season, percentile, everything())
  
  # Plot
  ggplot() +
    geom_sf(data = home_ranges, aes(color = percentile), fill=NA, linewidth = 1) +
    # Plot land
    geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
    geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
    # Crop
    coord_sf(xlim = c(-127, -123), ylim = c(41.5, 47)) +
    theme_bw() +
    labs(color = "Contour")
  
  return(home_ranges)
  
}

# Loop through seasons and build
hrs <- purrr::map_df(2007:2021, function(x){
  map_home_range(x)
})
# hrs_sf <- sf::st_as_sf(hrs)

ggplot() +
  geom_sf(data = hrs %>% filter(percentile%in% c(25,50,95)), aes(color = season), linewidth = 1, fill=NA) +
  facet_wrap(.~percentile, nrow=1) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-127, -123), ylim = c(41.5, 47)) +
  theme_bw() +
  # Legend
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  labs(color = "Year")



saveRDS(hrs, file=file.path(outdir, "fishing_grounds_or_weighted_season.Rds"))




