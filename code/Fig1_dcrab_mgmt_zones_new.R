


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
zonedir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/merged/processed"
gisdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/washington/gis_data/processed"
bathydir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/depth/"

# Read zones
zones_orig <- readxl::read_excel(file.path(zonedir, "WC_dcrab_da_mgmt_zones.xlsx"))

# Read SMA polygons
sma_polys <- sf::st_read(file.path(gisdir, "sma_polygons.shp"))

# Read 100 fathoms
fishing_grounds <- readRDS(file.path(bathydir, "100fathoms_depth.Rds"))

# Read OR landings data
landings_or_orig <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/landings/odfw/confidential/processed/ODFW_2000_2020_oregon_landings_data_expanded.csv", as.is=T)
ports_or <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/landings/odfw/public/processed/oregon_ports.xlsx")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Get PACFIN monthly Dcrab landings
dcrab_orig <- wcfish::pacfin_crab2


# Landings data
################################################################################

# Format OR landings
landings_or <- landings_or_orig %>%
  # Dcrab only
  filter(sci_name=="Metacarcinus magister") %>%
  # Sum by port/year
  group_by(port_complex, port, year) %>%
  summarize(landings_mt=sum(landings_mt),
            value_usd=sum(value_usd)) %>%
  ungroup() %>%
  # Average over last 10-years
  filter(year>=2011) %>%
  group_by(port_complex, port) %>%
  summarize(landing_mt=mean(landings_mt),
            value_usd=mean(value_usd)) %>%
  ungroup() %>%
  # Add lat/long
  left_join(ports_or %>% select(port, lat_dd, long_dd), by="port")

# Build management zones
################################################################################

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Build zones
zones <- zones_orig %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
  # Alter Zone I lat
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg)) %>%
  # Remove Cali
  filter(state!="California")

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




# Dcab landings
################################################################################

# Build Dcrab landings
dcrab <- dcrab_orig %>%
  # Sum by month
  group_by(state, season, month, date) %>%
  summarize(landings_mt=sum(landings_mt, na.rm=T)) %>%
  ungroup() %>%
  # Arrange and order
  arrange(state, season, date) %>%
  group_by(state, season) %>%
  mutate(month_num=1:n(),
         landings_prop=landings_mt/max(landings_mt, na.rm=T)) %>%
  ungroup() %>%
  # Add year
  mutate(year1=substr(season, 1, 4) %>% as.numeric()) %>%
  # Arrange
  select(state, season, year1, month, month_num, date, landings_mt, landings_prop, everything()) %>%
  # ORder state
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()))

# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=6),
                    strip.text = element_text(size=7),
                    plot.title=element_blank(),
                    plot.tag = element_text(size=8),
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
g1 <- ggplot(zones) +
  # Plot fishing grounds
  geom_tile(data=fishing_grounds, mapping=aes(x=long_dd, y=lat_dd), fill="grey70") +
  # Plot SMA polygons
  # geom_sf(data=sma_polys, fill="grey60", color=NA) +
  geom_sf(data=sma_polys, fill=NA, color="black") +
  # Plot management zones
  geom_hline(data=zones_no_ncal_line, mapping=aes(yintercept=lat_dd_north), linetype="dotted", size=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=-126.5, hjust=0, size=2, show.legend = F) +
  geom_hline(yintercept=borders, linetype="solid", color="black", size=0.2) +
  # Plot Sonoma-Mendocino country line
  geom_hline(yintercept=son_mend_county, linetype="dashed", size=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot landings
  geom_point(data=landings_or, mapping=aes(x=long_dd, y=lat_dd, size=value_usd/1e6), color="grey40") +
  # Plot SMA labels
  geom_sf_text(data=sma_polys %>% filter(subunit!="Quinault-Split Rock to Raft River"), mapping=aes(label=sma), hjust=-0.1, size=2, fontface = "italic") +
  # Plot management zone points
  geom_text(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=2, hjust=0) +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_size_continuous(name="Seasonal\nrevenue\n(US millions)", range = c(0.2,3.5)) +
  # Crop
  coord_sf(xlim = c(-127, -122), ylim = c(41.8, 49)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = c(0.83, 0.12))
g1

# Plot
g2 <- ggplot(dcrab %>% filter(year1>=2000), aes(x=month_num, y=landings_prop, color=year1, group=year1)) +
  facet_wrap(~state, scales="free_y", ncol=1) +
  geom_line(lwd=0.3) +
  # Labels
  labs(x="Season month", y="Proportion of\nmaximum landings", tag="B") +
  # X-axis
  scale_x_continuous(breaks=1:12) +
  # Legend
  scale_color_gradientn(name="", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.7, 0.52),
        legend.key.size=unit(0.2, "cm"))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.55, 0.45))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_dcrab_mgmt_zones_new.png"),
       width=4, height=4, units="in", dpi=600)




