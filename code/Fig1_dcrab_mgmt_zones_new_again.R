


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
closuredir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/closures/processed"

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

# Read data
data_orig <- readRDS(file.path(closuredir, "2015_2020_WC_dcrab_closures.Rds"))


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

# Build zones data
################################################################################

# # Sonoma-Mendocino county line
# son_mend_county <- 38+46.125/60
#
# # Build zones dataframe
# zones_df <- zones_orig %>%
#   filter(!is.na(lat_dd_north)) %>%
#   select(state, lat_dd_north) %>%
#   rename(y=lat_dd_north) %>%
#   mutate(x1=recode(state,
#                    "Washington"="2014-10-01",
#                    "Oregon"="2017-11-01",
#                    "California"="2020-11-01") %>% ymd(),
#          x2=recode(state,
#                    "Washington"="2021-09-15",
#                    "Oregon"="2021-08-14",
#                    "California"="2021-07-15"),
#          x2=ifelse(y<38.3, "2021-06-30", x2),
#          x2=ymd(x2))
#
# # Build zones
# zones1 <- zones_orig %>%
#   filter(state=="Washington") %>%
#   mutate(season="2015-16 season") %>%
#   select(season, everything())
# zones2 <- zones_orig %>%
#   mutate(season="2020-21 season") %>%
#   select(season, everything())
# zones <- bind_rows(zones1, zones2) %>%
#   mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
#   # Alter Zone I lat
#   mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg))


# Format data
################################################################################

# Fix data
data <- data_orig %>%
  mutate(status=as.character(status),
         status=recode(status, "Whale entanglement closure"="Marine life entanglement closure"),
         status=ifelse(status=="Out-of-season", NA, status),
         status=factor(status, levels=c("Season open", "Body condition delay",
                                        "Body condition/domoic acid delay", "Domoic acid delay",
                                        "Evisceration order", "Marine life entanglement closure")))


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

# Date parameters
date_min_do <- ymd("2014-01-01")
date_max_do <- ymd("2021-01-01")

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

# Plot data
g2 <- ggplot(data, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Management zone lines
  # geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
  #              inherit.aes = F, color="grey50", size=0.2) +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2) +
  # Plot call outs
  # geom_point(stars, mapping=aes(x=date, y=lat_dd), pch=21, fill="white", inherit.aes = F, size=3.5) +
  # geom_text(stars, mapping=aes(x=date, y=lat_dd, label=id), inherit.aes = F, size=2.2) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Date", y="Latitude (°N)", tag="B") +
  # Legends
  scale_fill_manual(name="Season status", values=c("grey90", "pink", "orange", "darkred", "coral", "navy"), na.translate = F) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text = element_text(size=5))
g2

# Plot
g3 <- ggplot(dcrab %>% filter(year1>=2000), aes(x=month_num, y=landings_prop, color=year1, group=year1)) +
  facet_wrap(~state, scales="free_y", nrow=1) +
  geom_line(lwd=0.3) +
  # Labels
  labs(x="Season month", y="Proportion of\nmaximum landings", tag="C") +
  # X-axis
  scale_x_continuous(breaks=1:12) +
  # Legend
  scale_color_gradientn(name="", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.95, 0.7),
        legend.key.size=unit(0.2, "cm"))
g3


# Merge
layout_matrix <- matrix(data=c(1,2,
                               1,3), byrow=T, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g3,
                             layout_matrix=layout_matrix,
                             widths=c(0.33, 0.67), heights=c(0.6, 0.4))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_dcrab_mgmt_zones_new_again.png"),
       width=6.5, height=4, units="in", dpi=600)




