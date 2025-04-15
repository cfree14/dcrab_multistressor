



# Clear workspace
rm(list = ls())

# Setup
################################################################################

# install.packages("remotes")
# remotes::install_github("pfmc-assessments/nwfscSurvey")

# FRAM warehouse
# https://www.webapps.nwfsc.noaa.gov/data/map

# Packages
library(tidyverse)
library(nwfscSurvey)

# Directies
datadir <- "data/trawl_survey/raw"
outdir <- "data/trawl_survey/processed"
plotdir <- "data/trawl_survey/figures"


# Export
data <- readRDS(data, file=file.path(outdir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))
range(data$depth_m)


# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    plot.title=element_blank(),
                    # axis.text.y = element_text(angle = 90, hjust = 0.5),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size=unit(0.3, "cm"),
                    legend.key=element_blank(),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot
g <- ggplot(data %>% filter(cpue_kg_ha>0), aes(x=long_dd, y=lat_dd, color=cpue_kg_ha)) +
  # Facet
  facet_wrap(~year, ncol=7) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot surveys
  geom_point() +
  # Labels
  labs(x="", y="") +
  # Plot legend
  scale_color_gradientn(name="CPUE (kg/ha)",
                        trans="log10", 
                        breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                        labels=c("0.001", "0.01", "0.1", "1", "10", "100", "1000"),
                        colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.1)) +
  # Crop
  coord_sf(xlim = c(-126, -116.5), ylim = c(32.5, 48.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.95, 0.15))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_dcrab_trawl_survey_data.png"), 
       width=6.5, height=6.5, units="in", dpi=600)