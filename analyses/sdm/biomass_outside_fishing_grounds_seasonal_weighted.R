

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


# Prep fishing grounds
################################################################################

# Prep data
grounds <- grounds_orig %>% 
  sf::st_make_valid() %>% 
  # Transform
  sf::st_transform(crs=32610) %>% 
  rename(year=season) %>% 
  filter(year!=2020)


# Prep SDM
################################################################################

# Extract fits: 2x2km grid
fits <- preds_tot$data %>% 
  filter(region=="OR") %>% 
  mutate(density_kg_km2=exp(est),
         biomass_kg=density_kg_km2*4,
         biomass_mt=biomass_kg/1000) %>% 
  filter(year %in% 2007:2021)


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
  facet_wrap(~year, ncol=7) +
  # Data
  geom_tile() +
  # Plot fishing grounds
  geom_sf(data=grounds %>% filter(percentile %in% c(25,50, 95)), 
          mapping=aes(linetype=as.character(percentile)), fill=NA, inherit.aes = F, color="black") +
  # Plot land
  geom_sf(data=foreign_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Labels
  labs(title="Fixed+random effects") +
  # Legend
  scale_linetype_manual(name="Fishing ground\npercentile", values=c("dashed", "solid", "dotted")) +
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
ggsave(g, filename=file.path(plotdir, "FigX_biomass_fishing_grounds_weighted_season.png"), 
       width=6.5, height=6.5, units="in", dpi=600, bg="white")


# Summarize percent of crab outside fishing grous
################################################################################

# Loop through season and calculate
x <- 2007
years <- unique(fits$year)
stats <- purrr::map_df(years, function(x){
  
  # Build raster
  df <- fits %>% 
    filter(year==x) %>% 
    select(long_utm10m, lat_utm10m,  biomass_mt)
  ras <- raster::rasterFromXYZ(df) 
  raster::projection(ras) <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"
  ras
  
  # Isolate fishing grounds
  fgs <- grounds %>% 
    filter(year==x)
  
  biomass_mt_tot  <- df %>%
    pull(biomass_mt) %>% sum()
  
  # Biomass in fishing grounds
  percs <- fgs$percentile
  stats_fg <- raster::extract(x=ras, y=fgs, fun=sum, na.rm=T) %>% 
    as.data.frame() %>% 
    setNames("biomass_mt_fg") %>% 
    mutate(percentile=percs,
           year=x, 
           biomass_mt_tot=biomass_mt_tot, 
           biomass_mt_outside=biomass_mt_tot-biomass_mt_fg,
           perc_outside=biomass_mt_outside/biomass_mt_tot) %>% 
    select(year, percentile, everything())
  
})




stats_avg <- stats %>% 
  filter(percentile %in% c(25,50, 95)) %>% 
  group_by(percentile) %>% 
  summarize(perc_outside=mean(perc_outside)) %>% 
  ungroup() %>% 
  mutate(label=paste(round(perc_outside*100,0), "%"))


g <- ggplot(stats %>%   filter(percentile %in% c(25,50, 95)), 
            aes(x=year, y=perc_outside, color=as.character(percentile))) +
  # geom_hline(yintercept=c(0.05, 0.5), color="grey80", linetype="dashed") +
  geom_line() +
  geom_hline(data=stats_avg, mapping=aes(yintercept=perc_outside, 
                                         color=as.character(percentile)),
             inherit.aes = F, linetype="dashed") +
  geom_text(data=stats_avg, mapping=aes(y=perc_outside+0.02, 
                                     label=label, 
                                     color=as.character(percentile)), 
            x=2021, inherit.aes=F, show.legend = F, size=2.4) +
  # Axes
  labs(x="Year", y="Percent of biomass\noutside fishing grounds") +
  scale_y_continuous(lim=c(0, NA), labels=scales::percent_format()) +
  # Legend
  scale_color_discrete(name="Fishing ground percentile") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g

ggsave(g, filename=file.path(plotdir, "FigX_biomass_outside_fishing_grounds.png"), 
       width=5.5, height=3.5, units="in", dpi=600, bg="white")


# Frontier figure
################################################################################

stats_avg_perc <- stats %>% 
  group_by(percentile) %>% 
  summarize(perc_outside=mean(perc_outside)) %>% 
  ungroup()

g <- ggplot(stats, aes(x=percentile/100, y=1-perc_outside, fill=percentile)) +
  # Ref lines
  geom_abline(slope=1) +
  geom_vline(xintercept = c(0.25, 0.5, 0.95), color="grey70", linetype="dotted") +
  # Data
  geom_point(pch=21, stroke=0.2, size=2) +
  geom_point(data=stats_avg_perc, pch=21, stroke=0.4, size=4) +
  # Label zones
  annotate(geom="text", x=0.75, y=0.25, label="Refugia") +
  annotate(geom="text", x=0.25, y=0.75, label="Hyper-targeting") +
  annotate(geom="text", x=0.15, y=0.18, angle=42, label="Ideal free distribution") +
  # Labels
  labs(x="Fishing grounds\n(percent utilization area)", 
       y='Percent of biomass\nwithin fishing grounds') +
  # Axes
  scale_x_continuous(labels=scales::percent_format()) +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_gradientn(name="", colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g

ggsave(g, filename=file.path(plotdir, "FigX_biomass_outside_fishing_grounds_scatterplot.png"), 
       width=5, height=4.5, units="in", dpi=600, bg="white")


