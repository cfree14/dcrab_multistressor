

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/confidential/washington/processed/"
plotdir <- "analyses/fishing_grounds/figures"
outdir <- "analyses/fishing_grounds/output"

# Read spatial-temporal predictions
load("analyses/sdm/output/index_output_20km_mesh.Rdata")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
usa_utm <- usa %>% sf::st_transform(crs=32610)
foreign_utm <- foreign %>% sf::st_transform(crs=32610)

# Read fishing grounds
grounds_orig <- readRDS(file=file.path(outdir, "fishing_grounds_or_weighted_season.Rds"))

grounds <- grounds_orig %>%
  sf::st_difference(usa) %>% 
  st_transform(32610) %>%  # UTM Zone 10N; use 32611 if farther south/east in CA
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_transform(4326) %>% 
  mutate(dcrab_area_km2 = 19595.79,
         dcrab_prop= area_km2 / dcrab_area_km2 )


grounds <- rmapshaper::ms_erase(grounds_orig, usa)

grounds_areas <- grounds %>%
  st_transform(32610) %>%  # UTM Zone 10N; use 32611 if farther south/east in CA
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_transform(4326) %>% 
  mutate(dcrab_area_km2 = 19595.79,
         dcrab_prop= area_km2 / dcrab_area_km2 )



# Setup
################################################################################

grounds_areas_avg <- grounds_areas  %>% 
  group_by(percentile) %>% 
  summarize(dcrab_prop=mean(dcrab_prop)) %>% 
  ungroup()

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

g <- ggplot(grounds_areas %>% filter(percentile!=100), aes(x=percentile/100, y=1-dcrab_prop, fill=percentile/100)) +
  # Ref line
  geom_abline(slope=-1, intercept=1) +
  geom_vline(xintercept = c(0.25, 0.5, 0.95), color="grey70", linetype="dotted") +
  # Data
  geom_point(pch=21, stroke=0.2, size=2) +
  geom_point(data=grounds_areas_avg, pch=21, stroke=0.4, size=4) +
  # Labels
  labs(y="Percent of crab range\noutside of fishing grounds",
       x="Fishing grounds\n(% utilization area)") +
  # Axes
  scale_x_continuous(labels=scales::percent_format(), lim=c(0,1)) +
  scale_y_continuous(labels=scales::percent_format(), lim=c(0,1)) +
  # Label zones
  annotate(geom="text", x=0.75, y=0.75, label="Refugia") +
  annotate(geom="text", x=0.25, y=0.25, label="Hyper-targeting") +
  # Legend
  scale_fill_gradientn(name="", colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g

ggsave(g, filename=file.path(plotdir, "FigX_range_outside_fishing_grounds_scatterplot.png"), 
       width=5, height=4.5, units="in", dpi=600, bg="white")



