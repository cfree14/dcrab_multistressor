

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
data_orig <- readRDS(file=file.path(trawldir, "trawl_data_with_envi.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Only with crab
  filter(total_kg>0) %>% 
  # Average crab weight
  mutate(crab_kg=total_kg/total_n) 

# Plot data
################################################################################

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
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))



g <- ggplot(data, aes(x=year, y=crab_kg, group=year)) +
  geom_boxplot() +
  # Limits
  lims(y=c(0, 2.5)) +
  # Labels
  labs(x="Year", y="Average crab size (kg)") +
  # Theme
  theme_bw()
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_trawl_cpue_distribution.png"), 
       width=6.5, height=2.5, units="in", dpi=600)


