

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
data_orig <- readRDS(file=file.path(outdir, "fishing_grounds_or_full_entry.Rds"))

# Read ports
ports_orig <- readRDS(file=file.path(outdir, "ports_or_use.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Prep data
################################################################################

# Prep data
data <- data_orig %>% 
  filter(percentile %in% c(50, 95)) %>% 
  mutate(percentile=as.character(percentile))

# Prep ports
ports <- ports_orig %>% 
  filter(nvessels>=3)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
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
  geom_sf(data=data, mapping=aes(color=percentile), fill=NA, size=0.6) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Ports
  geom_point(data=ports,
             mapping=aes(x=long_dd, y=lat_dd, size=landings_lbs/1e6), color="black") +
  geom_text(data=ports,
            mapping=aes(x=long_dd+0.1, y=lat_dd, label=port), color="black",
            show.legend = F, size=2.3, hjust=0) +
  # Legend
  scale_size_continuous(name="Annual landings\n(millions lbs)", range = c(0.02, 3)) +
  scale_color_ordinal(name="Fishing ground\npercentile") +
  # Crop 
  coord_sf(xlim = c(-126, -122), ylim = c(41.5, 47)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "right",
        legend.key.size=unit(0.4, "cm"))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_fishing_grounds_or.png"), 
       width=3.5, height=4.0, units="in", dpi=600)

