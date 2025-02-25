

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
plotdir <- "figures/sdm"

# Read data
data_orig <- readRDS(file=file.path(trawldir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))

# Index standardization with sdmTMB
# https://pbs-assess.github.io/sdmTMB/articles/index-standardization.html

# Survey footprint
footprint_orig <- readRDS(file=file.path(trawldir, "nwfsc_wcbts_grid_cells.Rds"))

# USA
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf") %>% 
  sf::st_transform( crs=32610)

# Format survey data
################################################################################

# Build data
data <- data_orig %>% 
  # Rename
  rename(lat_utm10m=lat_utm10n,
         long_utm10m=long_utm10n) %>% 
  # Convert to UTM km
  mutate(lat_utm10km=lat_utm10m/1000,
         long_utm10km=long_utm10m/1000)


# Build survey footprint
################################################################################

# Survey sf
footprint <- footprint_orig %>% 
  sf::st_union() %>% 
  sf::st_transform(crs=32610)

ggplot() +
  geom_sf(data=usa, fill="grey90", color="white") +
  geom_sf(data=footprint, color="black", fill=NA) + 
  coord_sf(xlim=sf::st_bbox(footprint)[c(1,3)],
           ylim=sf::st_bbox(footprint)[c(2,4)]) +
  theme_bw()


# Build grid
################################################################################

# Management regions
########################################

# Region breaks
mgmt_lats_dd <- c(38+46.125/60, 42, 46.25)

# Convert region breaks to UTM10N
mgmt_lats_utm <- tibble(lat_dd=mgmt_lats_dd,
                        long_dd=-123) %>% 
  # Convert to SF
  sf::st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326) %>% 
  # Transform to UTM Zone 10N (EPSG:32610)
  sf::st_transform(., crs = 32610) %>% 
  sf::st_coordinates(.) %>% 
  as_tibble() %>% 
  pull("Y")


# Build full grid
########################################

# Cell size specs
cell_size_m <- 2000
cell_size_m2 <- cell_size_m*cell_size_m
cell_size_km <- cell_size_m / 1000
cell_size_km2 <- cell_size_km*cell_size_km

# Build UTM10 grid
range(data$lat_utm10m)
range(data$long_utm10m)
lats <- seq(3548600, 5369100, cell_size_m)
longs <- seq(279400, 1035600, cell_size_m)
grid_utm_full <- expand.grid(lat_utm10m=lats,
                             long_utm10m=longs)

# Reduce to cells within survey domain
########################################

# Convert to SF
grid_utm_full_sf <- grid_utm_full %>% 
  sf::st_as_sf(., coords = c("long_utm10m", "lat_utm10m"), crs = 32610) 
sf_sample <- grid_utm_full_sf %>% sample_frac(0.01)

# Plot check
ggplot() + 
  geom_sf(data=usa, fill="grey90") +
  geom_sf(data=footprint , fill="red") +
  geom_sf(data=sf_sample, alpha=0.5) +
  coord_sf(xlim=range(grid_utm_full$long_utm10m), ylim=range(grid_utm_full$lat_utm10m)) +
  theme_bw()

# Identify points that fall in land
in_domain_yn <- sf::st_intersects(grid_utm_full_sf, footprint, sparse = F) 
in_domain_yn1 <- in_domain_yn[,1]

# Filter out land points
grid_utm_sf  <- grid_utm_full_sf[in_domain_yn1, ]
sf_sample1 <- grid_utm_sf %>% sample_frac(0.1)

# Plot check
ggplot() + 
 geom_sf(data=usa, fill="grey90") +
  geom_sf(data=footprint , fill="red") +
  geom_sf(data=sf_sample1, alpha=0.1) +
  coord_sf(xlim=range(grid_utm_full$long_utm10m), ylim=range(grid_utm_full$lat_utm10m)) +
  theme_bw()

# Convert back to dataframe
grid_utm <- as.data.frame(sf::st_coordinates(grid_utm_sf)) %>% 
  set_names(c("long_utm10m", "lat_utm10m")) %>% 
  # Mark region
  mutate(region=cut(lat_utm10m, breaks=c(0, mgmt_lats_utm, Inf), labels=c("cCA", "nCA", "OR", "WA"))) %>%
  # Convert to UTM km
  mutate(lat_utm10km=lat_utm10m/1000,
         long_utm10km=long_utm10m/1000)
  
# Make years
yrs <- unique(data$year)
grid_utm_yrs <- purrr::map_df(yrs, function(x){
  df <- grid_utm %>% 
    mutate(year=x) %>% 
    select(year, everything())
})

# Regional grids
grid_utm_yrs_wa <- grid_utm_yrs %>% filter(region=="WA")
grid_utm_yrs_or <- grid_utm_yrs %>% filter(region=="OR")
grid_utm_yrs_nca <- grid_utm_yrs %>% filter(region=="nCA")
grid_utm_yrs_cca <- grid_utm_yrs %>% filter(region=="cCA")


# Model construction
################################################################################

# Build mesh
mesh <- make_mesh(data, c("long_utm10km", "lat_utm10km"), cutoff = 20) # 20 km quick, 10 km slow
plot(mesh)

# Fit model
m <- sdmTMB(
  data = data, 
  formula = cpue_kg_km2 ~ 0 + as.factor(year),
  time = "year", 
  mesh = mesh, 
  family = tweedie(link = "log"))

# Record and inspect residuals
data$resids <- residuals(m, type="mle-mvn")
hist(data$resids)
qqnorm(data$resids)
abline(a = 0, b = 1)


# Coastwide predictions
################################################################################

# Coastwide predictions
preds_tot <- predict(m, newdata = grid_utm_yrs, return_tmb_object = TRUE)

# Extract index
index_tot <- get_index(preds_tot, area = cell_size_km2, bias_correct = TRUE)

# Add CV
index_tot1 <- index_tot %>% 
  mutate(region="Total")

# Plot index
ggplot(index_tot1, aes(year, est)) + 
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  xlab('Year') + ylab('Biomass estimate (kg)')


# Regional predictions
################################################################################

# Region-level predictions
preds_cca <- predict(m, newdata = grid_utm_yrs_cca, return_tmb_object = TRUE)
preds_nca <- predict(m, newdata = grid_utm_yrs_nca, return_tmb_object = TRUE)
preds_or <- predict(m, newdata = grid_utm_yrs_or, return_tmb_object = TRUE)
preds_wa <- predict(m, newdata = grid_utm_yrs_wa, return_tmb_object = TRUE)

# Extract regional indices
index_cca <- get_index(preds_cca, area = cell_size_km2, bias_correct = TRUE)
index_nca <- get_index(preds_nca, area = cell_size_km2, bias_correct = TRUE)
index_or <- get_index(preds_or, area = cell_size_km2, bias_correct = TRUE)
index_wa <- get_index(preds_wa, area = cell_size_km2, bias_correct = TRUE)

# Format regional indices
index_cca1 <- index_cca %>% mutate(region="C. California")
index_nca1 <- index_nca %>% mutate(region="N. California")
index_or1 <- index_or %>% mutate(region="Oregon")
index_wa1 <- index_wa %>% mutate(region="Washington")

# Merge regional indices
indexes <- bind_rows(index_tot1, index_cca1, index_nca1, index_or1, index_wa1) %>% 
  mutate(cv = sqrt(exp(se^2) - 1)) %>% 
  select(region, year, everything())

# Save model output
save(m, mesh,
     grid_utm_yrs, grid_utm_yrs_or, grid_utm_yrs_nca, grid_utm_yrs_cca,
     preds_tot, preds_cca, preds_or, preds_wa, 
     indexes,
     file=file.path(outdir, "index_output_20km_mesh.Rdata"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "none",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot regional estimates
g <- ggplot(indexes, aes(x=year, y=est/1000/1000, color=region)) + 
  # Facet
  facet_wrap(~region, ncol=4, scales="free_y") +
  # Data
  geom_line() +
  geom_ribbon(aes(ymin = lwr/1000/1000, ymax = upr/1000/1000, fill=region), alpha = 0.4, color=NA) +
  lims(y=c(0,NA)) +
  # Labs
  labs(x="Year", y="Index of relative abundance") +
  # Theme 
  theme_bw() + my_theme
g

# Export
# ggsave(g, filename=file.path(plotdir, "FigX_index_of_abundance.png"), 
#        width=6.5, height=2, units="in", dpi=600)





