

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/confidential/washington/processed/"
plotdir <- "figures/sdm"

# Read data
data_orig <- readRDS(file=file.path(datadir, "WDFW_2010_2020_dcrab_logbooks_expanded.Rds"))
receipts_orig <- readRDS(file=file.path(datadir, "WDFW_1980_2023_dcrab_fish_tickets.Rds"))

# Read ports
ports <- readxl::read_excel(file.path(datadir, "wa_dcrab_ports.xlsx"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Steps
# 1. Determine ports to evaluate
# 2. Estimate fishing grounds using various thresholds
# 3. Export for analysis elsewhere
# 4. Export figures here as well

# Lots to do
# Order ports north to south
# Outliers (longitude outliers hidden below)
# avergage up /down?


# Imporant ports
################################################################################

landings <- receipts_orig %>% 
  # Period of interest
  filter(year %in% 2011:2020) %>%
  # Group
  group_by(port) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T)/10) %>% 
  ungroup()

ggplot(landings, aes(x=landings_lbs/1e6, y=reorder(port, desc(landings_lbs)))) +
  geom_bar(stat="identity") +
  labs(x="Average annual landings\n(millions of lbs, 2011-2020)", y="")


# Plot map
################################################################################

# Calculate port stats
port_stats <- data_orig %>% 
  # Period of interest
  filter(year %in% 2011:2020) %>%
  # Number of vessels
  group_by(port) %>% 
  summarize(nvessels=n_distinct(vessel)) %>% 
  ungroup() %>% 
  # Add XY
  left_join(ports) %>% 
  # Order
  arrange(desc(lat_dd))

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
g <- ggplot(port_stats) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Ports
  geom_point(mapping=aes(x=long_dd, y=lat_dd, size=nvessels, color=nvessels), pch=16) +
  geom_text(mapping=aes(x=long_dd, y=lat_dd, label=nvessels), size=1.5) +
  ggrepel::geom_text_repel(mapping=aes(x=long_dd, y=lat_dd, label=port), size=1.8) +
  # Legend
  scale_size_continuous(name="# of vessels") +
  scale_color_gradientn(name="# of vessels", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-125, -122), ylim = c(45.5, 49)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_wa_fishing_ports.png"), 
       width=4.5, height=5.5, units="in", dpi=600)


# Plot data
################################################################################

# Format data
data <- data_orig %>% 
  # Order ports
  mutate(port=factor(port, levels=port_stats$port))

# Theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=7),
                    plot.tag=element_text(size=8),
                    plot.title=element_blank(),
                    plot.subtitle = element_text(size=5, face="italic"),
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
g <- ggplot(data %>% sample_frac(0.05), mapping=aes(x=long_dd_start, y=lat_dd_start)) +
  facet_wrap(~port, ncol=10) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot fishing obs
  geom_point(color="grey70", size=0.8) +
  # Plot port
  geom_point(data=port_stats, mapping=aes(x=long_dd, y=lat_dd), color="black", size=1.5) +
  # Crop
  coord_sf(xlim = c(-126, -122), ylim = c(45, 49)) +
  # Theme
  theme_bw() + base_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_wa_fishing_grounds.png"),
       width=6.5, height=5.5, units="in", dpi=600)



# Plot data
################################################################################

# Loop through ports
ports_vec <- port_stats %>% 
  filter(!is.na(port)) %>% 
  filter(nvessels>20) %>% pull(port)
for(i in 1:length(ports_vec)){
  
  # Subset
  print(i)
  port_do <- ports_vec[i]
  trips <- data %>% 
    # Port of interest
    filter(port == port_do) %>% 
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
    geom_sf(data = trips_sf, color = "black", size = 0.5, alpha = 0.5, pch="x") +
    # Crop
    coord_sf(xlim = c(-125, -123.5), ylim = c(46, 48)) +
    # Theme
    theme_bw()
  
  # Merge
  if(i==1){
    contours_all <- contours
  }else{
    contours_all <- bind_rows(contours_all, contours)
  }
  
}

# 
contours_all1 <- contours_all %>% 
  sf::st_transform(crs=sf::st_crs(usa))

# Plot fishing grounds
ggplot() +
  facet_wrap(.~port, ncol=5) + 
  # Fishing grounds
  geom_sf(data=contours_all1, mapping=aes(fill=percentile), alpha=0.5) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-126, -122), ylim = c(45.5, 48.5)) +
  # Theme
  theme_bw()


# Export data
################################################################################






