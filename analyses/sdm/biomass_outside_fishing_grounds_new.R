

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
grounds_orig <- readRDS(file=file.path(outdir, "fishing_grounds_or_weighted.Rds"))


# Prep fishing grounds
################################################################################

# Prep data
grounds <- grounds_orig %>% 
  sf::st_make_valid() %>% 
  # Transform
  sf::st_transform(crs=32610) 


# Prep SDM
################################################################################

# Extract fits: 2x2km grid
fits <- preds_tot$data %>% 
  filter(region=="OR") %>% 
  mutate(density_kg_km2=exp(est),
         biomass_kg=density_kg_km2*4,
         biomass_mt=biomass_kg/1000)


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


# Plot spatial fixed+random effects
g <- ggplot(data=fits, aes(x= long_utm10m, y= lat_utm10m, fill=exp(est))) +
  # Facet
  facet_wrap(~year, ncol=10) +
  # Data
  geom_tile() +
  # Plot fishing grounds
  geom_sf(data=grounds, mapping=aes(linetype=as.character(percentile)), fill=NA, inherit.aes = F, color="black") +
  # Plot land
  geom_sf(data=foreign_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Labels
  labs(title="Fixed+random effects") +
  # Legend
  scale_linetype_manual(name="Fishing ground\npercentile", values=c("solid", "dashed", "dotted")) +
  scale_fill_gradientn(name="Biomass density (kg/km2)", 
                       trans = "log10", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = range(fits$long_utm10m), ylim = range(fits$lat_utm10m)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size=unit(0.3, "cm"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank())
g

# Export


# Summarize percent of crab outside fishing grous
################################################################################

# Build raster brick
df <- fits %>% 
  select(long_utm10m, lat_utm10m,  year, biomass_mt)
ras <- freeR::df2brick(df, "long_utm10m", "lat_utm10m",  "biomass_mt", "year") 
raster::projection(ras) <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"
ras

stats_all <- fits %>% 
  group_by(year) %>% 
  summarize(biomass_mt_tot=sum(biomass_mt, na.rm=T)) %>% 
  ungroup() 

# Biomass in fishing grounds
stats_fg <- raster::extract(x=ras, y=grounds, fun=sum, na.rm=T) %>% 
  as.data.frame() %>% 
  mutate(percentile=c(50,95)) %>% 
  select(percentile, everything()) %>% 
  gather(key="year", value="biomass_mt_fg", 2:ncol(.)) %>% 
  mutate(year=gsub("X", "", year) %>% as.numeric(.)) %>% 
  left_join(stats_all) %>% 
  mutate(biomass_mt_outside=biomass_mt_tot-biomass_mt_fg,
         perc_outside=biomass_mt_outside/biomass_mt_tot)




ggplot(stats_fg, aes(x=year, y=perc_outside, color=as.character(percentile))) +
  geom_line() +
  # Axes
  labs(x="Year", y="Percent of biomass\noutside fishing grounds") +
  scale_y_continuous(lim=c(0, NA), labels=scales::percent_format()) +
  # Theme
  theme_bw()

