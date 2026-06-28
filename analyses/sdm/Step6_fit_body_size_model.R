

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
outdir <- "analyses/sdm/output"
plotdir <- "analyses/sdm/figures"

# Read data
data_orig <- readRDS(file=file.path(trawldir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))

# Index standardization with sdmTMB
# https://pbs-assess.github.io/sdmTMB/articles/index-standardization.html

# # Survey footprint
# footprint_orig <- readRDS(file=file.path(trawldir, "nwfsc_wcbts_grid_cells.Rds"))
# 
# # USA
# usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf") %>% 
#   sf::st_transform( crs=32610)


# Format survey data
################################################################################

# Build data
data <- data_orig %>%
  # Rename
  rename(lat_utm10m=lat_utm10n,
         long_utm10m=long_utm10n) %>%
  # Convert to UTM km
  mutate(lat_utm10km=lat_utm10m/1000,
         long_utm10km=long_utm10m/1000) %>%
  # Calculate body size
  mutate(crab_kg=total_kg/total_n) %>%
  filter(crab_kg>0 & is.finite(crab_kg))

data_or <- data %>% 
  filter(state=="Oregon")


# # Build survey footprint
# ################################################################################
# 
# # Survey sf
# footprint <- footprint_orig %>% 
#   sf::st_union() %>% 
#   sf::st_transform(crs=32610)
# 
# ggplot() +
#   geom_sf(data=usa, fill="grey90", color="white") +
#   geom_sf(data=footprint, color="black", fill=NA) + 
#   coord_sf(xlim=sf::st_bbox(footprint)[c(1,3)],
#            ylim=sf::st_bbox(footprint)[c(2,4)]) +
#   theme_bw()
# 
# 
# # Build grid
# ################################################################################
# 
# # Management regions
# ########################################
# 
# # Region breaks
# mgmt_lats_dd <- c(38+46.125/60, 42, 46.25)
# 
# # Convert region breaks to UTM10N
# mgmt_lats_utm <- tibble(lat_dd=mgmt_lats_dd,
#                         long_dd=-123) %>% 
#   # Convert to SF
#   sf::st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326) %>% 
#   # Transform to UTM Zone 10N (EPSG:32610)
#   sf::st_transform(., crs = 32610) %>% 
#   sf::st_coordinates(.) %>% 
#   as_tibble() %>% 
#   pull("Y")
# 
# 
# # Build full grid
# ########################################
# 
# # Cell size specs
# cell_size_m <- 2000
# cell_size_m2 <- cell_size_m*cell_size_m
# cell_size_km <- cell_size_m / 1000
# cell_size_km2 <- cell_size_km*cell_size_km
# 
# # Build UTM10 grid
# range(data$lat_utm10m)
# range(data$long_utm10m)
# lats <- seq(3548600, 5369100, cell_size_m)
# longs <- seq(279400, 1035600, cell_size_m)
# grid_utm_full <- expand.grid(lat_utm10m=lats,
#                              long_utm10m=longs)
# 
# # # Reduce to cells within survey domain
# # ########################################
# # 
# # # Convert to SF
# # grid_utm_full_sf <- grid_utm_full %>% 
# #   sf::st_as_sf(., coords = c("long_utm10m", "lat_utm10m"), crs = 32610) 
# # sf_sample <- grid_utm_full_sf %>% sample_frac(0.01)
# # 
# # # Plot check
# # ggplot() + 
# #   geom_sf(data=usa, fill="grey90") +
# #   geom_sf(data=footprint , fill="red") +
# #   geom_sf(data=sf_sample, alpha=0.5) +
# #   coord_sf(xlim=range(grid_utm_full$long_utm10m), ylim=range(grid_utm_full$lat_utm10m)) +
# #   theme_bw()
# # 
# # # Identify points that fall in land
# # in_domain_yn <- sf::st_intersects(grid_utm_full_sf, footprint, sparse = F) 
# # in_domain_yn1 <- in_domain_yn[,1]
# # 
# # # Filter out land points
# # grid_utm_sf  <- grid_utm_full_sf[in_domain_yn1, ]
# # sf_sample1 <- grid_utm_sf %>% sample_frac(0.1)
# # 
# # # Plot check
# # ggplot() + 
# #  geom_sf(data=usa, fill="grey90") +
# #   geom_sf(data=footprint , fill="red") +
# #   geom_sf(data=sf_sample1, alpha=0.1) +
# #   coord_sf(xlim=range(grid_utm_full$long_utm10m), ylim=range(grid_utm_full$lat_utm10m)) +
# #   theme_bw()
# # 
# # # Convert back to dataframe
# # grid_utm <- as.data.frame(sf::st_coordinates(grid_utm_sf)) %>% 
# #   set_names(c("long_utm10m", "lat_utm10m")) %>% 
# #   # Mark region
# #   mutate(region=cut(lat_utm10m, breaks=c(0, mgmt_lats_utm, Inf), labels=c("cCA", "nCA", "OR", "WA"))) %>%
# #   # Convert to UTM km
# #   mutate(lat_utm10km=lat_utm10m/1000,
# #          long_utm10km=long_utm10m/1000)
# #   
# # # Make years
# # yrs <- unique(data$year)
# # grid_utm_yrs <- purrr::map_df(yrs, function(x){
# #   df <- grid_utm %>% 
# #     mutate(year=x,
# #            pass=2) %>% 
# #     select(year, everything())
# # })
# # 
# # # Regional grids
# # grid_utm_yrs_wa <- grid_utm_yrs %>% filter(region=="WA")
# # grid_utm_yrs_or <- grid_utm_yrs %>% filter(region=="OR")
# # grid_utm_yrs_nca <- grid_utm_yrs %>% filter(region=="nCA")
# # grid_utm_yrs_cca <- grid_utm_yrs %>% filter(region=="cCA")


# Model construction
################################################################################

# Build mesh
mesh <- make_mesh(data, c("long_utm10km", "lat_utm10km"), cutoff = 20) # 20 km quick, 10 km slow
mesh_or <- make_mesh(data_or, c("long_utm10km", "lat_utm10km"), cutoff = 20) # 20 km quick, 10 km slow

plot(mesh)
plot(mesh_or)

# Fit model
m <- sdmTMB(
  data = data, 
  formula = log(crab_kg) ~ s(depth_m),
  mesh = mesh, 
  spatiotemporal = "off",
  family = gaussian())

m_or <- sdmTMB(
  data = data_or, 
  formula = log(crab_kg) ~ s(depth_m),
  mesh = mesh_or, 
  spatiotemporal = "off",
  family = gaussian())

sanity(m)
sanity(m_or)

# Record and inspect residuals
data$resids <- residuals(m)
hist(data$resids)
qqnorm(data$resids, ylim=c(-5,5))
abline(a = 0, b = 1)


# Coastwide predictions
################################################################################

pred_depth <- data.frame(
  state="Coastwide",
  depth_m = seq(min(data$depth_m), max(data$depth_m), length.out = 200),
  long_utm10km = mean(data$long_utm10km, na.rm = TRUE),
  lat_utm10km  = mean(data$lat_utm10km, na.rm = TRUE)
)

pred <- predict(
  m,
  newdata = pred_depth,
  xy_cols = c("long_utm10km", "lat_utm10km"),
  se_fit = TRUE,
  re_form = NA
)

pred_depth$fit <- exp(pred$est)
pred_depth$lwr <- exp(pred$est - 1.96 * pred$est_se)
pred_depth$upr <- exp(pred$est + 1.96 * pred$est_se)


# Oregon

pred_depth_or <- data.frame(
  state="Oregon",
  depth_m = seq(min(data_or$depth_m), max(data_or$depth_m), length.out = 200),
  long_utm10km = mean(data$long_utm10km, na.rm = TRUE),
  lat_utm10km  = mean(data$lat_utm10km, na.rm = TRUE)
)

pred_or <- predict(
  m_or,
  newdata = pred_depth_or,
  xy_cols = c("long_utm10km", "lat_utm10km"),
  se_fit = TRUE,
  re_form = NA
)

pred_depth_or$fit <- exp(pred_or$est)
pred_depth_or$lwr <- exp(pred_or$est - 1.96 * pred_or$est_se)
pred_depth_or$upr <- exp(pred_or$est + 1.96 * pred_or$est_se)


# Merge
pred_depth1 <- bind_rows(pred_depth, pred_depth_or)

# Plot data
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

# Plot data
g <- ggplot(pred_depth1, aes(x=measurements::conv_unit(depth_m, "m", "ft")/6, 
                       y=fit, fill=state, color=state)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color=NA) +
  geom_line() +
  # Ref line
  geom_vline(xintercept=100, linetype="dotted", color="grey30") +
  # Labels
  labs(x = "Depth (fathoms)", y = "Predicted crab weight (kg)") +
  # Limits
  lims(y=c(0,NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank())
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_crab_size_with_depth_modeled.png"), 
       width=4.5, height=3.5, units="in", dpi=600, bg="white")
