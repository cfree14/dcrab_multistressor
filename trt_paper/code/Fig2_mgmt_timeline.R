
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


# Dungeness data
################################################################################


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

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60
date_min_do <- min(data_orig$date)


# Spot prawn
################################################################################

# North: Aug 1 - Apr 30 (closed May 1 to July 31)
# South: Feb 1 - Oct 31 (closed November 1 to January 31)


# Define latitude ranges
north_min_lat <- 34.57
north_max_lat <- 42
south_min_lat <- 32
south_max_lat <- 34.57

# Define year range
years <- 2011:2023  # Open seasons from 2011 to 2023 (inclusive)

# Create open seasons for north area (Aug 1 - Apr 30 of following year)
north_df <- lapply(years, function(y) {
  data.frame(
    xmin = ymd(paste0(y, "-08-01")),
    xmax = ymd(paste0(y + 1, "-04-30")),
    ymin = north_min_lat,
    ymax = north_max_lat,
    area = "North"
  )
}) %>% bind_rows()

# Create open seasons for south area (Feb 1 - Oct 31 of the same year)
south_df <- lapply(years, function(y) {
  data.frame(
    xmin = ymd(paste0(y, "-02-01")),
    xmax = ymd(paste0(y, "-10-31")),
    ymin = south_min_lat,
    ymax = south_max_lat,
    area = "South"
  )
}) %>% bind_rows()


# Sablefish
################################################################################

# Define year range
years <- 2011:2023  # Open seasons from 2011 to 2023 (inclusive)

# Create open seasons for south area (Apr 1 - Oct 31 of the same year)
sable_df <- lapply(years, function(y) {
  data.frame(
    xmin = ymd(paste0(y, "-04-01")),
    xmax = ymd(paste0(y, "-10-31")),
    ymin = 32,
    ymax = 48
  )
}) %>% bind_rows()


# Plot data
################################################################################

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
  # Plot data
  geom_rect(data = sable_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = NA, fill="grey85") +
  # Labels
  labs(x="", y="Latitude (°N)", tag="B", title="Sablefish") +
  # Axes
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c("2011-01-01", "2024-01-01"))) +
  scale_y_continuous(lim=c(32, 48), breaks=seq(32, 48, 4)) +
  # Theme
  theme_bw() + my_theme
g2

# Two oil spills
# Refugio Oil Spill (May 2015): Following a crude oil spill near Refugio and El Capitan State Beaches, the California Department of Fish and Wildlife (CDFW) closed affected fisheries from May 19 to June 29, 2015. This closure impacted various marine species, including spot prawns, to prevent contaminated seafood from entering the market. ​
# OEHHA
# 
# Pipeline P00547 Spill (October 2021): A crude oil spill offshore at Huntington Beach led to the closure of fisheries from October 3 to November 30, 2021. The closure area extended from Surfside Beach to the U.S./Mexico border, affecting spot prawn harvesting among other marine life. ​
# OEHHA

# Spot prawn
mhw1 <- ymd("2014-03-01")
mhw2 <- ymd("2016-07-01")
mhw_mid <- mhw1 + as.numeric(difftime(mhw2, mhw1)) / 2
g3 <- ggplot() +
  # Plot data
  geom_rect(data = open_seasons,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = NA, fill="grey85") +
  # Plot MHW
  geom_rect(mapping=aes(xmin=mhw1,
                        xmax=mhw2,
                        ymin=32.5, 
                        ymax=41.5), fill="red", alpha=0.5) +
  geom_text(mapping=aes(x=mhw_mid, y=37, label="MHW"), color="darkred", hjust=0.5, vjust=0.5) +
  # Axes
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c("2011-01-01", "2024-01-01"))) +
  scale_y_continuous(breaks = seq(32, 42, 2)) +
  # Labels
  labs(x = "", y = "Latitude (°N)", tag="C", title="Spot prawn") +
  # Theme
  theme_bw() + my_theme
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=1, heights=c(0.5, 0.25, 0.25))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_mgmt_timeline.png"),
       width=6.5, height=5.5, units="in", dpi=600)

