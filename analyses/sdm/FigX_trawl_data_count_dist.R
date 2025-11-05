

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

# Build stats
zero_stats <- data_orig %>% 
  mutate(presence_yn=ifelse(cpue_kg_km2>0, "> 0", "0") %>% factor(., levels=c("0", "> 0"))) %>% 
  count(presence_yn)

# Zero and non zero values
data_w0 <- data_orig %>% 
  mutate(type="0s included") %>% 
  select(type, trawl_id, cpue_kg_km2)
data_no0 <- data_orig %>% 
  filter(cpue_kg_km2>0) %>% 
  mutate(type="0s removed") %>% 
  select(type, trawl_id, cpue_kg_km2)
data <- bind_rows(data_w0, data_no0)

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

# Plot presence absence
g1 <- ggplot(zero_stats, aes(fill=presence_yn, y=n, x="")) +
  geom_bar(stat="identity") + 
  # Labels
  labs(y="Number of tows", x="", tag="A") +
  # Legend
  scale_fill_manual(values=c("orange", "lightblue")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.margin = margin(-2,0,0,-2),
        legend.position = "bottom",
        legend.title = element_blank())
g1

# Density
g2 <- ggplot(data, aes(x=cpue_kg_km2, fill=type)) +
  geom_density(alpha=0.4) +
  # Labels
  labs(x="CPUE (kg/km2)", y="Density", tag="B") +
  scale_x_continuous(lim=c(0, 8000),
                     breaks=seq(0, 8000, 2000),
                     labels=c(seq(0,6000, 2000), ">8000")) +
  # Legend
  scale_fill_manual(values=c("orange", "lightblue")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.7, 0.8),
        legend.title = element_blank())
g2

# Non-zero histogram
g3 <- ggplot(data_orig %>% filter(cpue_kg_km2>0), aes(x=cpue_kg_km2)) +
  geom_histogram(fill="lightblue") +
  # Labels
  labs(x="CPUE (kg/km2)", y="Number of tows", tag="C") +
  # Axis
  scale_x_continuous(trans="log10",
                     breaks=c(1, 10, 100, 1000, 10000, 100000),
                     labels=c("1", "10", "100", "1000", "10000", "100000")) +
  # Theme
  theme_bw() + my_theme
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, widths=c(0.2, 0.35, 0.45))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_trawl_cpue_distribution.png"), 
       width=6.5, height=2.5, units="in", dpi=600)


