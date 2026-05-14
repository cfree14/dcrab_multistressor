
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "analyses/trt_paper/data"
outdir <- "analyses/trt_paper/output"
plotdir <- "analyses/trt_paper/figures"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_closures/data/processed/2015_2024_WC_dcrab_closures.Rds")

# Read time key
time_key_orig <- read.csv(file.path(indir, "days_per_period.csv")) 

# Latitude breaks
lat_breaks <- c(32,
                34+27/60,
                36,
                37+11/60,
                38+46.125/60,
                40+10/60,
                42,
                # Oregon
                43.12, # Bandon
                45.06, # Cascade Head
                46.263, # OR/WA border
                # Washington
                46.466, # Klipsan Beach
                47.67, # Destruction Island
                48.29 # Tatoosh Island
)


# Build data
################################################################################

# Levels
sort(unique(data_orig$status))

# Order levels (use ORIGINAL name)
levels_use <- c("Season open",                                           
                "Out-of-season",   
                "Body condition delay",                                   
                "Body condition/domoic acid delay", 
                "Domoic acid delay",                                     
                "Evisceration order",                                    
                "Evisceration order (+depth restriction/gear reduction)", 
                "Whale entanglement/domoic acid delay",
                "Whale entanglement closure",
                "30-fathom depth restriction",   
                "40-fathom depth restriction", 
                "25% gear reduction",  
                "33% gear reduction",                                    
                "50% gear reduction",
                "40-fathom depth restriction/20% gear reduction",
                "30-fathom depth restriction/25% gear reduction",         
                "30-fathom depth restriction/50% gear reduction")

# Reset order
data <- data_orig %>%
  # Ordeer
  mutate(status = factor(status, levels = levels_use)) %>% 
  # Update level name
  mutate(status = fct_recode(
    status,
    "Evisceration order (+depth/gear restriction)" = "Evisceration order (+depth restriction/gear reduction)"
  ))

# Inspect
str(data)
#freeR::complete(data1)
levels(data$status)
table(data$status)

# Plot data
################################################################################

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60
date_min_do <- min(data_orig$date)

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Jameal zones
  geom_hline(yintercept=lat_breaks, linewidth=0.2, linetype="dotted") +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), linewidth=0.2) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", linewidth=0.2) + # Sonoma/Mendocino
  # Week lines
  # geom_vline(xintercept = lubridate::ymd(time_key_orig$min_date),
  #            linewidth=0.2) +
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="black", size=2.5) +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=35:48, lim=c(35,NA)) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", 
                    values=c("grey85", # Season open
                             "white", # Out-of-season
                             "pink", # Body condition delay
                             "orange", # Body condition/domoic acid delay
                             "darkred", # Domoic acid delay
                             "coral", # Evisceration order
                             "violetred", # Evisceration order (+depth/gear restriction)
                             "purple3", # Whale entanglement/domoic acid delay
                             "navy", # Whale entanglement closure
                             "dodgerblue3", # 30-fathom depth restriction
                             "dodgerblue1", # 40-fathom depth restriction
                             "lightblue1", # 25% gear reduction 
                             "lightblue2", # 33% gear reduction
                             "lightblue3", # 50% gear reduction
                             "springgreen1",  # 30-fathom depth restriction/25% gear reduction
                             "springgreen3",  # 30-fathom depth restriction/50% gear reduction
                             "springgreen4" # 40-fathom depth restriction/20% gear reduction
                             ),
                    drop=F) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "dcrab_season_status_by_jameals_zones.png"),
       width=6.5, height=3.25, units="in", dpi=600)

