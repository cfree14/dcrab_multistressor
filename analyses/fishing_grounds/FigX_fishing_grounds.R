

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

# Read data
data_orig <- readRDS(file=file.path(outdir, "fishing_grounds.Rds"))

# Read ports
ports_orig <- readRDS(file=file.path(outdir, "ports.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Prep data
################################################################################

# Prep data
data <- data_orig %>% 
  filter(percentile==95)

# Prep ports
ports <- ports_orig %>% 
  filter(port %in% data$port)



# Plot data
################################################################################

# Theme
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

# Plot fishing grounds
g <- ggplot() +
  # Fishing grounds
  geom_sf(data=data, mapping=aes(fill=state), alpha=0.5, show.legend = F) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Ports
  geom_point(data=ports, 
             mapping=aes(x=long_dd, y=lat_dd, size=landings_lbs/1e6), color="grey50") +
  ggrepel::geom_text_repel(data=ports,
                           mapping=aes(x=long_dd, y=lat_dd, label=port), color="black",
                           show.legend = F, size=1.5) +
  # Legend
  scale_size_continuous(name="Annual landings\n(millions lbs)", range = c(0.02, 3)) +
  # Crop 
  coord_sf(xlim = c(-126, -116), ylim = c(34.5, 48.5)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_fishing_grounds.png"), 
       width=3.5, height=5.5, units="in", dpi=600)

