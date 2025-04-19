

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
cadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landing_receipts_2023/processed/"
zonedir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/merged/processed"

# Directories
plotdir <- "trt_paper/figures"

# Read data
data_orig <- readRDS(file=file.path(cadir, "1980_2022_landings_receipts.Rds"))

# Read Dcrab zones
zones_orig <- readxl::read_excel(file.path(zonedir, "WC_dcrab_da_mgmt_zones.xlsx"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Do by season not year
# Adsjut for inflation
# Add OR and WA

# Build data
################################################################################

# Gear stats
gear_stats <- data_orig %>% 
  filter(comm_name %in% c("Dungeness crab", "Spot prawn", "Sablefish")) %>% 
  group_by(comm_name, gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(comm_name, desc(landings_lbs)) %>% 
  group_by(comm_name) %>% 
  mutate(prop=landings_lbs/sum(landings_lbs)*100)

# Sablefish gears
# This was informed by the analysis above
gears_dcrab <- c("Crab or lobster trap", "Entrapping")
gears_sablefish <- c("Fish trap", "Entrapping", "Traps, Seattle type (sablefish)")
gears_prawn <- c("Prawn trap", "Entrapping")

# Function to summarize data
build_data <- function(species, gears){
  
  # Build data
  # yrs <- 2011:2022
  df <- data_orig %>% 
    # Species of interest
    filter(comm_name %in% species) %>% 
    # Gears of interest
    filter(gear %in% gears) %>% 
    # Years
    # filter(year %in% yrs) %>% 
    # Group
    group_by(comm_name, year) %>% 
    summarize(nvessels=n_distinct(vessel_id),
              landings_lbs=sum(landings_lbs, na.rm=T),
              value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup()
  
}

# Build
df1 <- build_data("Dungeness crab", gears_dcrab)
df2 <- build_data("Sablefish", gears_sablefish)
df3 <- build_data("Spot prawn", gears_prawn)

# Merge
data <- bind_rows(df1, df2, df3)
# sum(data$nvessels<3) # make sure 0


# Build spatial data
################################################################################

# Build spatial data
species <- "Dungeness crab"; gears <-  gears_dcrab
build_data_xy <- function(species, gears){
  
  # Build data
  yrs <- 2011:2022
  df <- data_orig %>% 
    # Species of interest
    filter(comm_name %in% species) %>% 
    # Gears of interest
    filter(gear %in% gears) %>% 
    # Years
    mutate(year %in%  yrs ) %>% 
    # Group
    group_by(comm_name, block_id) %>% 
    summarize(nvessels=n_distinct(vessel_id),
              landings_lbs=sum(landings_lbs, na.rm=T)/length(yrs),
              value_usd=sum(value_usd, na.rm=T)/length(yrs)) %>% 
    ungroup()
  
}

# Build
df1 <- build_data_xy("Dungeness crab", gears_dcrab)
df2 <- build_data_xy("Sablefish", gears_sablefish)
df3 <- build_data_xy("Spot prawn", gears_prawn)

# Merge
data_xy <- bind_rows(df1, df2, df3) 

# Spatialize
blocks_sf <- wcfish::blocks %>% sf::st_as_sf()
x <- "Dungeness crab"
data_sf <- purrr::map_df(unique(data_xy$comm_name), function(x) {
  
  sdata <- data_xy %>% 
    filter(comm_name==x)
  out <- blocks_sf %>% 
    left_join(sdata) %>% 
    filter(!is.na(landings_lbs)) %>% 
    filter(block_type=="Inshore")
  
})


# Build management zones
################################################################################

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Build zones
zones <- zones_orig %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
  # Alter Zone I lat
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg))

zones_no_ncal_line <- zones %>%
  filter(landmark_north!="Sonoma/Mendocino County Line")

# Zone points
zone_pts <- zones %>%
  mutate(!is.na(lat_dd))

# Borders
border_n <- zones %>% arrange(desc(lat_dd_north)) %>% slice(1) %>% pull(lat_dd_north)
borders_s <- zones %>%
  filter(grepl("border", landmark_south)) %>% pull(lat_dd_south)
borders <- c(border_n, borders_s) %>% unique()


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    axis.title.x=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag = element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.3, "cm"),
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Map theme
map_theme <- base_theme + 
  theme(legend.position=c(0.75, 0.8), 
        plot.title=element_text(size=7), 
        axis.title=element_blank(),
        axis.text = element_blank())

# Colors
# colors <- c("darkorange", "darkorchid4", "darkgreen")
colors <- c(RColorBrewer::brewer.pal(9, "Oranges")[8],
            RColorBrewer::brewer.pal(9, "Purples")[8],
            RColorBrewer::brewer.pal(9, "Greens")[8])

# Vessels
g1 <- ggplot(data, aes(x=year, y=nvessels, color=comm_name)) + 
  geom_line() +
  # Labels
  labs(x="", y="Number of vessels", tag="A") + 
  #scale_x_continuous(breaks=seq(2010, 2024, 2)) +
  # Legend
  scale_color_manual(name="", values=colors) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position = c(0.7, 0.9))
g1

# Landings
g2 <- ggplot(data, aes(x=year, y=landings_lbs/1e6, color=comm_name)) + 
  geom_line() +
  # Labels
  labs(x="", y="Landings (lbs, millions)", tag="B") + 
  #scale_x_continuous(breaks=seq(2010, 2024, 2)) +
  # Legend
  scale_color_manual(name="", values=colors) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position = "none")
g2

# Revenues
g3 <- ggplot(data, aes(x=year, y=value_usd/1e6, color=comm_name)) + 
  geom_line() +
  # Labels
  labs(x="", y="Revenues (2023 USD, millions)", tag="C") + 
  #scale_x_continuous(breaks=seq(2010, 2024, 2)) +
  # Legend
  scale_color_manual(name="", values=colors) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position = "none")
g3


# Dungeness crab
ramp_lines <- c(42, 40+10/60, 38+46.125/60, 37+11/60, 36, 34+27/60, 32+32/60)
ramp_centroids <- zoo::rollmean(ramp_lines, k=2)
ramp_labels <- 1:6
g4 <- ggplot(data_sf %>% filter(comm_name=="Dungeness crab"), mapping=aes(fill=landings_lbs)) +
  geom_sf() +
  # Plot DA management zones
  geom_hline(data=zones_no_ncal_line, mapping=aes(yintercept=lat_dd_north), 
             linetype="dotted", size=0.2) +
  # Plot RAMP mgmt zones
  geom_hline(yintercept = ramp_lines, linetype="dashed", size=0.2) +
  # Plot mgmt zone labels
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), 
            x=-126.8, hjust=0, size=1.5, show.legend = F, inherit.aes=F) +
  # Plot RAMP text labels
  annotate(geom="text", y=ramp_centroids, label=ramp_labels, x=-125.8, fontface="bold", size=1.8) +
  # plot state lines
  geom_hline(yintercept = c(42, 46.25, 32+32/60), linetype="solid", linewidth=0.3) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot management zone points
  geom_text(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=1.5, hjust=0, inherit.aes = F) +
  # Labels
  labs(title="Dungeness crab") +
  # Legend
  scale_fill_gradientn(name="Landings (lbs/yr)", trans="log10", 
                       breaks=c(1, 10, 100, 1000, 10000, 100000),
                       labels=c("1", "10", "100", "1000", "10000", "100000"),
                       colors=RColorBrewer::brewer.pal(9, "Oranges")) + 
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-127, -116), ylim = c(32, 49)) +
  # Theme
  theme_bw() + map_theme
g4

# Sablefish
g5 <- ggplot(data_sf %>% filter(comm_name=="Sablefish"), mapping=aes(fill=landings_lbs)) +
  geom_sf() +
  # Ref lines
  # geom_hline(data=ref_lines, mapping=aes(yintercept=lat_dd), linetype="dashed") +
  # plot state lines
  geom_hline(yintercept = c(42, 46.25, 32+32/60), linetype="solid", linewidth=0.3) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Labels
  labs(title="Sablefish") +
  # Legend
  scale_fill_gradientn(name="Landings (lbs/yr)", trans="log10", 
                       breaks=c(1, 10, 100, 1000, 10000, 100000),
                       labels=c("1", "10", "100", "1000", "10000", "100000"),
                       colors=RColorBrewer::brewer.pal(9, "Purples")) + 
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-127, -116), ylim = c(32, 49)) +
  # Theme
  theme_bw() + map_theme
g5

# Spot prawn
adj <- 0.3
g6 <- ggplot(data_sf %>% filter(comm_name=="Spot prawn"), mapping=aes(fill=landings_lbs)) +
  geom_sf() +
  # Ref lines
  geom_hline(yintercept=34.577211, linetype="dashed", linewidth=0.3) +
  annotate(geom="text", x=-126.5, y=34.577211+adj, label="North", size=2.2) +
  annotate(geom="text", x=-126.5, y=34.577211-adj, label="South", size=2.2) +
  # plot state lines
  geom_hline(yintercept = c(42, 46.25, 32+32/60), linetype="solid", linewidth=0.3) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Labels
  labs(title="Spot prawn") +
  # Legend
  scale_fill_gradientn(name="Landings (lbs/yr)", trans="log10", 
                       breaks=c(1, 10, 100, 1000, 10000, 100000),
                       labels=c("1", "10", "100", "1000", "10000", "100000"),
                       colors=RColorBrewer::brewer.pal(9, "Greens")) + 
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-127, -116), ylim = c(32, 49)) +
  # Theme
  theme_bw() + map_theme
g6
  

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, 
                             g4, g5, g6, nrow=2, heights=c(0.3, 0.7))

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_trap_fishery_time_series.png"), 
       width=6.5, height=6.4, units="in", dpi=600)







# # Function to plot maps
# # species <- "Dungeness crab"
# plot_map <- function(species, colors){ #ref_lines
#   
#   g <- ggplot(data_sf %>% filter(comm_name==species), mapping=aes(fill=landings_lbs)) +
#     geom_sf() +
#     # Ref lines
#     # geom_hline(data=ref_lines, mapping=aes(yintercept=lat_dd), linetype="dashed") +
#     # plot state lines
#     geom_hline(yintercept = c(42, 46.25), linetype="solid", linewidth=0.3) +
#     # Plot land
#     geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
#     geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
#     # Labels
#     labs(title=species) +
#     # Legend
#     scale_fill_gradientn(name="Landings (lbs/yr)", trans="log10", 
#                          breaks=c(1, 10, 100, 1000, 10000, 100000),
#                          labels=c("1", "10", "100", "1000", "10000", "100000"),
#                          colors=RColorBrewer::brewer.pal(9, colors)) + 
#     guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
#     # Crop
#     coord_sf(xlim = c(-127, -116), ylim = c(32, 49)) +
#     # Theme
#     theme_bw() + map_theme
#   
#   return(g)
#   
# }
# 
# # Plot maps
# g4 <- plot_map("Dungeness crab", colors="Oranges"); g4
# g5 <- plot_map("Sablefish", colors="Purples"); g5
# g6 <- plot_map("Spot prawn", colors="Greens"); g6 # ref_lines = ref_prawn








 

