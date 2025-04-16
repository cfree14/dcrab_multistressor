
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/raw"
outdir <- "data/processed"
plotdir <- "trt_paper/figures"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_closures/data/processed/2015_2023_WC_dcrab_closures.Rds")

# Format data
levels(data_orig$status)
levels_use <- c( "Season open", "Out-of-season", "Body condition delay", 
                 "Body condition/domoic acid delay", "Domoic acid delay", "Evisceration order",                                
                 "Evisceration order (+depth/gear restriction)", "Whale entanglement closure",                            
                 "30-fa depth restriction", "40-fa depth restriction",                           
                 "40-fa depth restriction/20% gear reduction", "33% gear reduction", "50% gear reduction" )

data1 <- data_orig %>%
  mutate(status = factor(status, labels = levels_use))


# Plot data
################################################################################

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60
date_min_do <- min(data_orig$date)

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.title=element_text(size=8),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data1, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), linewidth=0.2) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", linewidth=0.2) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="black", size=2.5) +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=35:48, lim=c(35,NA)) +
  # Labels
  labs(x="", y="Latitude (°N)", tag="A", title="Dungeness crab") +
  # Legends
  scale_fill_manual(name="Season status", 
                    values=c("grey85", "white", "pink", "orange", "darkred", "coral", "purple3",
                             "navy", "dodgerblue3", "dodgerblue1", "dodgerblue", "lightblue", "lightblue1"), 
                    drop=F) +
  # Theme
  theme_bw() + my_theme
g1

# Plot empty
g2 <- ggplot() +
  # Labels
  labs(tag="B", title="Sablefish") +
  # Theme
  theme_bw() + my_theme
g2

# Plot empty
g3 <- ggplot() +
  # Labels
  labs(tag="C", title="Spot prawn") +
  # Theme
  theme_bw() + my_theme
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=1, heights=c(0.5, 0.25, 0.25))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_mgmt_timeline.png"),
       width=6.5, height=5.5, units="in", dpi=600)

