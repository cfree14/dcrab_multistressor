

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

# Read data 
data_orig <- readRDS(file=file.path(outdir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))


# Format data
################################################################################

# Build data
data <- data_orig %>% 
  mutate(date1=paste("2024", month(date), day(date), sep="-") %>% ymd(.))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_blank(),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.margin = margin(-3, 0, -5, 0),
                   legend.position = "top",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data, aes(x=date1, y=year, color=state)) +
  geom_point() + 
  # Labels
  labs(x="Date", y="Year") +
  scale_y_reverse() +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_trawl_temporal_timing.png"), 
       width=6.5, height=2.75, units="in", dpi=600)









