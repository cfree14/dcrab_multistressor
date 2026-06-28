

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
outdir <- "analyses/sdm/output"
plotdir <- "analyses/sdm/figures"

# Read data
data_orig <- readRDS(file=file.path(trawldir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))

# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Oregon only
  filter(state=="Oregon") %>% 
  # Average crab weight
  mutate(crab_kg=total_kg/total_n) %>% 
  # Only with crab
  filter(crab_kg>0 & is.finite(crab_kg)) %>% 
  # Convert depth to fathoms
  mutate(depth_fa=measurements::conv_unit(depth_m, "m", "ft")/6) %>% 
  # Bin depth
  mutate(depth_fa_bin=cut(depth_fa, c(seq(0, 200, 10), 420))) %>% 
  # Pass
  mutate(pass=ifelse(pass==1, "Summer", "Fall") %>% factor(., levels=c("Summer", "Fall")))


# Build data
################################################################################

base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    plot.tag = element_text(size=8),
                    plot.title=element_text(size=8),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size=unit(0.3, "cm"),
                    legend.key=element_blank(),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=depth_fa_bin, fill=pass, y=crab_kg)) +
  geom_boxplot() +
  # Ref line
  geom_vline(xintercept=7.5, linetype="dashed", color="grey30") +
  # Labels
  labs(x="Depth bin (fa)", y="Crab weight (kg)") +
  # Axes
  scale_y_continuous(lim=c(0,1.6)) +
  # Legend
  scale_fill_discrete(name="Season") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top")
g  

# Export
ggsave(g, filename=file.path(plotdir, "FigX_crab_size_with_passes_and_depth.png"), 
       width=6.5, height=4, units="in", dpi=600, bg="white")  
  
  
