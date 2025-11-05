

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
load(file.path(outdir, "index_output_20km_mesh.Rdata"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
usa_utm <- usa %>% sf::st_transform(crs=32610)
foreign_utm <- foreign %>% sf::st_transform(crs=32610)


# Plot mesh
################################################################################

data <- m$data
my_mesh <- make_mesh(data, c("long_utm10km", "lat_utm10km"), cutoff = 20) # 20 km quick, 10 km slow
plot(mesh)

g <- inlabru::gg(my_mesh$mesh)
g

mesh <- fm_mesh_1d(seq(0, 10, by = 0.5))

# Plot it

ggplot() +
  gg(mesh)

gorillas <- inlabru::gorillas_sf

# Invoke ggplot and add geomes for the Gorilla nests and the survey
# boundary

ggplot() +
  inlabru::gg(gorillas$boundary) +
  inlabru::gg(gorillas$nests)

# Plot mesh
################################################################################

# Sanity check
sanity(m)

# Residuals
data$resids <- residuals(m, type="mle-mvn")
hist(data$resids)

# QQ pplot
qqnorm(data$resids)
abline(a = 0, b = 1)

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

# Residuals histogram
g1 <- ggplot(data, aes(x=resids)) +
  geom_histogram(fill="grey50") +
  # Ref line
  geom_vline(xintercept=0) +
  # Labels
  labs(x="Residuals", y="Frequency", tag="A") +
  # Theme
  theme_bw() + my_theme
g1

# QQ plot
qq <- qqnorm(data$resids)
qq_df <- tibble(x=qq$x,
                y=qq$y)
g2 <- ggplot(qq_df, aes(x=x, y=y)) +
  geom_point(pch=1, color="grey50") +
  # Reference line
  geom_abline(slope=1) +
  # Labels
  labs(x="Theoretical quantiles", y="Sample quantiles", tag="B") +
  # Theme
  theme_bw() + my_theme
g2

# Spatial residuals
g3 <- ggplot(data, aes(long_dd, lat_dd, col = resids)) + 
  # Facet
  facet_wrap(~year) + 
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Data
  geom_point(size=0.6) + 
  # Legend
  scale_colour_gradient2(name="Residuals") +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -116), ylim = c(34.5, 47.9)) +
  # Labels
  labs(x="", y="", tag="C") +
  # Theme
  theme_bw() + my_theme +
  theme(strip.text=element_text(size=7),
        legend.key.size=unit(0.5, "cm"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank())
g3

# Merge
layout_matrix <- matrix(data=c(1, 3,
                               2, 3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.4, 0.6))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_index_of_abundance_diagnostics.png"),
       width=6.5, height=4.5, units="in", dpi=600)


# Plot fits
################################################################################

# Extract fits
fits <- preds_tot$data


# Plot spatial fixed+random effects
g <- ggplot(data=fits, aes(x= long_utm10m, y= lat_utm10m, fill=exp(est))) +
  # Facet
  facet_wrap(~year, ncol=10) +
  # Plot land
  geom_sf(data=foreign_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Data
  geom_tile() +
  # Labels
  labs(title="Fixed+random effects") +
  # Legend
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
ggsave(g, filename=file.path(plotdir, "FigX_index_of_abundance_map_fe_re.png"),
       width=6.5, height=3.3, units="in", dpi=600)


# Plot fixed effects
g <- ggplot(data=fits, aes(x= long_utm10m, y= lat_utm10m, fill=exp(est_non_rf))) +
  # Facet
  facet_wrap(~year, ncol=10) +
  # Plot land
  geom_sf(data=foreign_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Data
  geom_tile() +
  # Labels
  labs(title="Fixed effects only") +
  # Legend
  scale_fill_gradientn(trans = "log10", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
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
ggsave(g, filename=file.path(plotdir, "FigX_index_of_abundance_map_fe.png"),
       width=6.5, height=3.3, units="in", dpi=600)

# Plot spatial random effects
g <- ggplot(data=fits, aes(x= long_utm10m, y= lat_utm10m, fill=omega_s)) +
  # Facet
  facet_wrap(~year, ncol=10) +
  # Plot land
  geom_sf(data=foreign_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Data
  geom_tile() +
  # Labels
  labs(title="Spatial random effects only") +
  # Legend
  scale_fill_gradient2() +
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
ggsave(g, filename=file.path(plotdir, "FigX_index_of_abundance_map_re.png"),
       width=6.5, height=3.3, units="in", dpi=600)

# Plot spatiotemporal random effects
g <- ggplot(data=fits, aes(x= long_utm10m, y= lat_utm10m, fill=epsilon_st)) +
  # Facet
  facet_wrap(~year, ncol=10) +
  # Plot land
  geom_sf(data=foreign_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Data
  geom_tile() +
  # Labels
  labs(title="Spatiotemporal random effects") +
  # Legend
  scale_fill_gradient2() +
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
ggsave(g, filename=file.path(plotdir, "FigX_index_of_abundance_map_epsilon.png"),
       width=6.5, height=3.3, units="in", dpi=600)



# Plot indices
################################################################################

# Format index data
index_df <- indexes %>% 
  # Factor
  mutate(region=recode(region, "Total"="Coastwide"),
         region=factor(region, levels=c( "Coastwide", "C. California", "N. California", "Oregon", "Washington"))) %>% 
  # Convert to metric tons
  mutate(index_mt=est/1000,
         index_mt_lo=lwr/1000,
         index_mt_hi=upr/1000)

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
g <- ggplot(index_df, aes(x=year, y=index_mt/1000, color=region)) + 
  # Facet
  # facet_wrap(~region, ncol=3, scales="free_y", repeat.tick.labels=TRUE) +
  lemon::facet_rep_wrap(~region, ncol=3, scales="free_y", repeat.tick.labels=TRUE) +
  # Data
  geom_line() +
  geom_ribbon(aes(ymin = index_mt_lo/1000, ymax = index_mt_hi/1000, fill=region), alpha = 0.4, color=NA) +
  # Limits
  lims(y=c(0,NA)) +
  # Legend
  scale_color_manual(name="State", values=c("grey20", "darkorange", "darkred", "darkgreen", "navy")) +
  scale_fill_manual(name="State", values=c("grey20", "darkorange", "darkred", "darkgreen", "navy")) +
  # Labs
  labs(x="Year", y="Index of relative abundance (1000s mt)") +
  # Theme 
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_index_of_abundance.png"),
       width=6.5, height=3.75, units="in", dpi=600)



