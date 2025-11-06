

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/confidential/california/processed/"
plotdir <- "analyses/fishing_grounds/figures"
outdir <- "analyses/fishing_grounds/output"

# Read landings data
data_orig <- readRDS(file.path(datadir, "1980_2022_dcrab_receipts_traps.Rds"))

# Ports
ports_orig <- readxl::read_excel(file.path(datadir, "ca_dcrab_ports.xlsx"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Get blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% 
  sf::st_drop_geometry()


# Step 1. What ports to examine
################################################################################

# Years of interest
years_do <- 2011:2020

# Calculate average landings
stats_landings <- data_orig %>% 
  # Period of interest
  filter(year %in% years_do) %>%
  # Group
  group_by(port) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nreceipts=n(),
            landings_lbs=sum(landings_lbs, na.rm=T)/length(years_do)) %>% 
  ungroup()


# Merge port stats
ports <- stats_landings %>% 
  # Add lat/long
  left_join(ports_orig) %>% 
  # Arrange and calculate cumulative percent
  arrange(desc(landings_lbs)) %>% 
  mutate(landings_cum_perc=cumsum(landings_lbs)/sum(landings_lbs, na.rm=T)) %>% 
  # Remove ports without Dcrab
  filter(!is.na(landings_lbs))



# Plot ports
################################################################################

# Map theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
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
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot ports
g <- ggplot(ports) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Ports
  geom_point(mapping=aes(x=long_dd, y=lat_dd, size=landings_lbs/1e6, color=landings_lbs/1e6), pch=16) +
  geom_text(mapping=aes(x=long_dd, y=lat_dd, label=nvessels), size=1.5) +
  ggrepel::geom_text_repel(mapping=aes(x=long_dd, y=lat_dd, label=port), size=1.8) +
  # Legend
  scale_size_continuous(name="Annnual landings\n(millions lb)") +
  scale_color_gradientn(name="Annnual landings\n(millions lb)", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-125, -116), ylim = c(33.5, 42)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_ca_fishing_ports.png"), 
       width=4.5, height=5.5, units="in", dpi=600)


# Plot block results
################################################################################


# Find the first row where cum_pct exceeds 0.99
cut_row <- which(ports$landings_cum_perc > 0.99)[1]

# Ports to evaluate
ports_vec <- ports %>% 
  # Mark ports contributing to top 99% of landings
  mutate(include_yn=ifelse(seq_len(nrow(.)) <= cut_row, 1, 0)) %>% 
  filter(include_yn==1) %>% 
  # ifelse(seq_len(nrow(df)) <= cut_row, 1, 0)
  # filter(!is.na(nobs_w_gps)) %>% 
  # filter(nvessels>20) %>% 
  pull(port)

# Build data
data <- data_orig %>% 
  # Ports and years of interest
  filter(port %in% ports_vec & year %in% years_do) %>% 
  # Summarize by block
  group_by(port, block_id) %>% 
  summarise(landings_lbs=sum(landings_lbs, na.rm=T)/length(years_do)) %>% 
  ungroup() %>% 
  # Reduce to valid blocks
  filter(block_id %in% blocks$block_id) %>% 
  # Add block 
  left_join(blocks %>% select(block_id, block_type)) %>% 
  sf::st_as_sf()

# Plot data
g <-ggplot(data, mapping=aes(fill=landings_lbs/1e6)) +
  facet_wrap(~port, ncol=5) +
  # Data
  geom_sf() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  # Legend
  scale_fill_gradientn(name="Annual landings (millions lbs)", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim=c(-126, -116), ylim=c(32, 42)) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")
#g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_fishing_grounds_ca.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Lots to do in this fig
# 1. Calculate % of total port so you can see gradient more
# 2. Inshore blocks only with percent ignored marked



# Step 2. Map fishing grounds
################################################################################

# Find the first row where cum_pct exceeds 0.99
cut_row <- which(ports$landings_cum_perc > 0.99)[1]

# Ports to evaluate
ports_vec <- ports %>% 
  # Mark ports contributing to top 99% of landings
  mutate(include_yn=ifelse(seq_len(nrow(.)) <= cut_row, 1, 0)) %>% 
  filter(include_yn==1) %>% 
  # ifelse(seq_len(nrow(df)) <= cut_row, 1, 0)
  # filter(!is.na(nobs_w_gps)) %>% 
  # filter(nvessels>20) %>% 
  pull(port)

# Loop through ports
i <- 1
for(i in 1:length(ports_vec)){
  
  # Subset
  print(i)
  port_do <- ports_vec[i]
  
  # Build data
  trips <- data_orig %>% 
    # Port of interest
    filter(port == port_do) %>% 
    # Only trips in valid blocks
    left_join(blocks_df %>% select(block_type, block_id, block_lat_dd, block_long_dd), by=c("block_id")) %>% 
    filter(block_type=="Inshore")
  
  # Convert to sf object
  trips_sf <- sf::st_as_sf(trips, coords = c("block_long_dd", "block_lat_dd"), crs = 4326)
  
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
    mutate(port=port_do) %>% 
    arrange(port, percentile)
  
  # Plot data
  ggplot() +  
    # Plot land
    geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
    geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
    # Countours
    geom_sf(data = contours, mapping=aes(fill = percentile), alpha = 0.3) +
    # Contour
    # geom_sf(data = trips_sf, color = "black", size = 0.5, alpha = 0.5, pch="x") +
    # Crop
    coord_sf(xlim = c(-125, -116), ylim = c(34, 42)) +
    # Theme
    theme_bw()
  
  # Merge
  if(i==1){
    contours_all <- contours
  }else{
    contours_all <- bind_rows(contours_all, contours)
  }
  
}

# Transform contours
contours_all1 <- contours_all %>% 
  # Transform
  sf::st_transform(crs=sf::st_crs(usa)) %>% 
  # Remove row names
  remove_rownames() %>% 
  # Add state
  mutate(state="California") %>% 
  # Simplify
  select(state, port, percentile)

# Plot fishing grounds
ggplot() +
  facet_wrap(.~port, ncol=5) + 
  # Fishing grounds
  geom_sf(data=contours_all1, mapping=aes(fill=percentile), alpha=0.5) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-125, -116), ylim = c(34, 42)) +
  # Theme
  theme_bw()


# Export data
################################################################################

# Export
saveRDS(ports, file=file.path(outdir, "ports_ca.Rds"))
saveRDS(contours_all1, file=file.path(outdir, "fishing_grounds_ca.Rds"))
















