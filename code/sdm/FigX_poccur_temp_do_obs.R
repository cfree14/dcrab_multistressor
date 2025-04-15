

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

# Read data
data_orig <- readRDS(file=file.path(trawldir, "trawl_data_with_envi.Rds"))


# Build data
################################################################################

# Convert 
convert_do <- function(x, to="mg/l"){
  if(to=="mg/l"){
    y <- x / 1e6 * 15.999*2 * 1e3
  }
  return(y)
}
convert_do(62.2, "mg/l")

# Build data
data <- data_orig %>% 
  # Conver to umol/l to mg/l
  mutate(do_mg_l=convert_do(do_umol_l, to="mg/l"),
         do_mg_l_glorys=convert_do(do_umol_l_glorys, to="mg/l"))

# Temp range
hist(data$temp_c)
range(data$temp_c, na.rm=T)
range(data$temp_c_glorys, na.rm=T)

# DO range
hist(data$do_mg_l)
range(data$do_mg_l, na.rm=T)
range(data$do_mg_l_glorys, na.rm=T)

# Temp bins
temp_bins <- seq(0, 21.5, 0.15)
temp_bins_mid <- zoo::rollmean(x=temp_bins, k=2, na.pad=F)

# DO bins
do_bins <- seq(0, 6.6, 0.1)
do_bins_mid <- zoo::rollmean(x=do_bins, k=2, na.pad=F)

# Calculate stats
stats_lo <- data %>% 
  # Filter to Live Ocean
  filter(!is.na(temp_c)) %>% 
  # Add bins
  mutate(temp_c_bin=cut(temp_c, breaks=temp_bins, labels = temp_bins_mid) %>% 
           as.character() %>% as.numeric(),
         do_mg_l_bin=cut(do_mg_l, breaks=do_bins, labels = do_bins_mid) %>% 
           as.character()  %>% as.numeric()) %>% 
  # Summarize
  group_by(temp_c_bin, do_mg_l_bin) %>% 
  summarize(n=n(),
            ncrab=sum(cpue_kg_km2>0),
            cpue_kg_km2=mean(cpue_kg_km2[cpue_kg_km2>0])) %>% 
  ungroup() %>% 
  mutate(pcrab=ncrab/n)

# Calculate stats
stats_glorys <- data %>% 
  # Filter to Live Ocean
  filter(!is.na(temp_c_glorys) & !is.na(do_mg_l_glorys)) %>% 
  # Add bins
  mutate(temp_c_bin=cut(temp_c_glorys, breaks=temp_bins, labels = temp_bins_mid) %>% 
           as.character() %>% as.numeric(),
         do_mg_l_bin=cut(do_mg_l_glorys, breaks=do_bins, labels = do_bins_mid) %>% 
           as.character()  %>% as.numeric()) %>% 
  # Summarize
  group_by(temp_c_bin, do_mg_l_bin) %>% 
  summarize(n=n(),
            ncrab=sum(cpue_kg_km2>0),
            cpue_kg_km2=mean(cpue_kg_km2[cpue_kg_km2>0])) %>% 
  ungroup() %>% 
  mutate(pcrab=ncrab/n)


# Plot data
################################################################################

# Base theme
my_theme <- theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(stats_lo %>% filter(pcrab>0), aes(x=temp_c_bin, y=do_mg_l_bin, fill=pcrab)) +
  geom_tile(data=stats_lo %>% filter(pcrab==0), fill="grey80") +
  geom_tile() +
  # Reference lines
  geom_hline(yintercept=2, linetype="dashed") +
  geom_vline(xintercept=15, linetype="dashed") +
  # Labels
  labs(x="Temperature (°C)", y="Dissolved oxygen (mg/l)",
       tag="A", title="LiveOcean (OR and WA only)") +
  # Axes
  lims(x=c(0, 22), y=c(0, 7)) +
  # Legend
  scale_fill_gradientn(name="P(occurence)",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.2, 0.8))
g1

# Plot data
g2 <- ggplot(stats_glorys %>% filter(pcrab>0), aes(x=temp_c_bin, y=do_mg_l_bin, fill=pcrab)) +
  geom_tile(data=stats_glorys %>% filter(pcrab==0), fill="grey80") +
  geom_tile() +
  # Reference lines
  geom_hline(yintercept=2, linetype="dashed") +
  geom_vline(xintercept=15, linetype="dashed") +
  # Labels
  labs(x="Temperature (°C)", y="Dissolved oxygen (mg/l)",
       tag="B", title="GLORYS (coastwide)") +
  # Axes
  lims(x=c(0, 22), y=c(0, 7)) +
  # Legend
  scale_fill_gradientn(name="P(occurence)",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.2, 0.8))
g2


# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)






