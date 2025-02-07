

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

data <- data_orig %>% 
  select(pressure_dbar:pco2_uatm)



# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title = element_blank(),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Pretty names
colnames(data)
pretty_names <- c("Pressure\n(dbar)", "Salinity\n(psu)", "Temperature\n(°C)", 
                  "Dissolved\noxygen\n(umol/l)", "Dissolved\noxygen\n(µmol/kg)",
                  "pH", "pCO2\n(µatm)")

# Plot data
g <- GGally::ggpairs(data, 
                     columnLabels=pretty_names) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_live_ocean_correllogram.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

