

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
logdir <- "data/confidential/merged/processed"
trawldir <- "data/trawl_survey/processed"
receiptdir <- "data/confidential/california/processed/"
plotdir <- "figures/sdm"

# Read data
logs_orig <- readRDS(file=file.path(logdir, "OR_WA_logbook_data.Rds"))
trawl_orig <- readRDS(file=file.path(trawldir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))
receipts_orig <- readRDS(file=file.path(receiptdir, "1980_2022_dcrab_receipts_traps.Rds"))
temps_orig <- readRDS("data/glorys/processed/GLORYS_1993_2021_annual_bt_stats.Rds")


# Get blocks
blocks <- wcfish::blocks %>% sf::st_as_sf()


# Trawl data
################################################################################

# Calculate lat/long midpoint (VECTORISE IF YOU HAVE TIME AND UNDO ROWWISE BELOW)
get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
}

# Domain
range(trawl_orig$lat_dd)
range(trawl_orig$long_dd)

# Build data
bin1 <- 0.05
trawl <- trawl_orig %>% 
  # Build lat/long bins
  mutate(lat_dd_bin=cut(lat_dd, breaks=seq(31.5, 49, bin1)),
         long_dd_bin=cut(long_dd, breaks=seq(-127, -116, bin1))) %>% 
  # Average
  group_by(lat_dd_bin, long_dd_bin) %>% 
  summarize(ntows=n(),
            cpue_kg_km2=mean(cpue_kg_km2, na.rm=T)) %>% 
  ungroup() %>% 
  # Add midpoints
  rowwise() %>% 
  mutate(lat_dd=get_midpoint(lat_dd_bin),
         long_dd=get_midpoint(long_dd_bin)) %>% 
  ungroup()

# Trawl dates
dates_trawl <- trawl_orig %>% 
  mutate(state=cut(lat_dd, breaks=c(-Inf, 42, 45, Inf), labels=c("CA", "OR", "WA"))) %>% 
  group_by(state) %>% 
  summarize(date_min=min(date),
            date_max=max(date))


# Temperature
################################################################################

# Temp data
temps <- temps_orig %>% 
  group_by(long_dd, lat_dd) %>% 
  summarize(temp_c=mean(temp_c_max)) %>% 
  ungroup()

# Temp dates
dates_temps <- tibble(state=factor(c("CA", "OR", "WA"), levels=c("CA", "OR", "WA")),
                      date_min=ymd("1993-01-01"),
                      date_max=ymd("2021-06-30"))



# Logbook data
################################################################################

# Domain
range(logs_orig$lat_dd_set, na.rm=T)
range(logs_orig$long_dd_set, na.rm=T)

# Summarize spatial coverage
bin2 <- 0.05
lat_breaks <- seq(39, 50, bin2)
lat_bread_mids <- zoo::rollmean(lat_breaks, 2)
long_breaks <- seq(-141, -121, 0.05)
long_bread_mids <- zoo::rollmean(long_breaks, 2)
logs_stats <- logs_orig %>% 
  # Build lat/long bins
  mutate(lat_dd_bin=cut(lat_dd_set, breaks=lat_breaks, labels=lat_bread_mids ) %>% as.character() %>% as.numeric()) %>% 
  mutate(long_dd_bin=cut(long_dd_set, breaks=long_breaks, labels=long_bread_mids ) %>% as.character() %>% as.numeric()) %>% 
  # Build lat/long bins
  group_by(lat_dd_bin, long_dd_bin) %>% 
  summarize(nvessels=n_distinct(vessel_id), 
            n=n()) %>% 
  ungroup() %>% 
  # Filter for rule of three
  filter(nvessels>=3)

dates_logs <- logs_orig %>% 
  group_by(state) %>% 
  summarize(date_min=min(date, na.rm=T),
            date_max=max(date, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(state=recode(state,
                      "Oregon"="OR",
                      "Washington"="WA"),
         state=factor(state, levels=c("CA", "OR", "WA")))


# Receipt data
################################################################################

# Build data
receipts <- receipts_orig %>% 
  group_by(block_id) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nyr=n_distinct(year),
            catch_lbs=sum(landings_lbs)) %>% 
  ungroup() %>% 
  mutate(catch_lbs_yr=catch_lbs/nyr) %>% 
  filter(nvessels>=3) %>% 
  # Convert
  mutate(catch_kg_yr=measurements::conv_unit(catch_lbs_yr, "lbs", "kg"),
         catch_mt_yr=catch_kg_yr/1000)

# Spatialize
receipts_sf <- blocks %>% 
  # Add data
  left_join(receipts, by="block_id") %>% 
  # Remove blocks without data
  filter(!is.na(catch_lbs_yr)) %>% 
  # Remove large blocks
  filter(block_type=="Inshore")

dates_tix <- receipts_orig %>% 
  mutate(state="CA") %>% 
  group_by(state) %>% 
  summarize(date_min=min(date, na.rm=T),
            date_max=max(date, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(state=factor(state, levels=c("CA", "OR", "WA")))

# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag=element_text(size=8, face="bold"),
                    plot.title=element_text(size=7),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size=unit(0.3, "cm"),
                    legend.key=element_blank(),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Inset theme
inset_theme <- theme(axis.text=element_text(size=6),
                     axis.title=element_blank(),
                      plot.title=element_blank(),
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
g1 <- ggplot(data=trawl %>% filter(cpue_kg_km2>0), aes(x=long_dd, y=lat_dd, fill=cpue_kg_km2)) +
  # Plot data
  geom_tile(data=trawl, aes(x=long_dd, y=lat_dd), fill="grey70", inherit.aes = F) + # 0s
  geom_tile() +
  # Plot management lines
  geom_hline(yintercept=c(38+46.125/60, 42, 46.25), linetype="dashed", color="grey20", linewidth=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Labs
  labs(x="", y="", tag="A", title="NOAA trawl survey") +
  # Legend
  scale_fill_gradientn(name="\nCPUE (kg/km2)", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), 
                       trans="log10", breaks=c(1, 10, 100, 1000, 10000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -116), ylim = c(34.5, 47.9)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.65, 0.78))
g1

g1a <- ggplot(dates_trawl, aes(y=state, x=date_min, xend=date_max)) +
  geom_segment(linewidth=1, color="grey40") +
  # Scales
  scale_y_discrete(drop=F) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2024-01-01"))) +
  # Theme
  theme_bw() + inset_theme
g1a

# Plot data
g2 <- ggplot(data=logs_stats, aes(x=long_dd_bin, y=lat_dd_bin, fill=n)) +
  # Plot data
  geom_tile() +
  # Plot management lines
  geom_hline(yintercept=c(38+46.125/60, 42, 46.25), linetype="dashed", color="grey20", linewidth=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Labs
  labs(x="", y="", tag="B", title="OR/WA logbooks") +
  # Legend
  scale_fill_gradientn(name="\n# of logbooks", 
                      colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), 
                      trans="log10", breaks=c(1, 10, 100, 1000, 10000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -116), ylim = c(34.5, 47.9)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.65, 0.78))
g2

g2a <- ggplot(dates_logs, aes(y=state, x=date_min, xend=date_max)) +
  geom_segment(linewidth=1, color="grey40") +
  # Scales
  scale_y_discrete(drop=F) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2024-01-01"))) +
  # Theme
  theme_bw() + inset_theme
g2a

# Plot data
g3 <- ggplot(data=receipts_sf, aes(fill=catch_mt_yr)) +
  # Plot data
  geom_sf(linewidth=0.1, color="grey30") +
  # Plot management lines
  geom_hline(yintercept=c(38+46.125/60, 42, 46.25), linetype="dashed", color="grey20", linewidth=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Labs
  labs(x="", y="", tag="C", title="CA/OR/WA landing receipts") +
  # Legend
  scale_fill_gradientn(name="\nCatch (mt/yr)", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), 
                       trans="log10", breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                       labels=c("0.01", "0.1", "1", "10", "100", "1000", "10000")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -116), ylim = c(34.5, 47.9)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.65, 0.78))
g3

g3a <- ggplot(dates_tix, aes(y=state, x=date_min, xend=date_max)) +
  geom_segment(linewidth=1, color="grey40") +
  # Scales
  scale_y_discrete(drop=F) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2024-01-01"))) +
  # Theme
  theme_bw() + inset_theme
g3a

# Bottom temperature
g4 <- ggplot(data=temps, aes(x=long_dd, y=lat_dd, fill=temp_c)) +
  # Plot data
  geom_tile() +
  # Plot management lines
  geom_hline(yintercept=c(38+46.125/60, 42, 46.25), linetype="dashed", color="grey20", linewidth=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Labs
  labs(x="", y="", tag="D", title="GLORYS bottom temperature") +
  # Legend
  scale_fill_gradientn(name="Bottom temp.\n(°C, annual max)",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -116), ylim = c(34.5, 47.9)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.65, 0.78))
g4

g4a <- ggplot(dates_temps, aes(y=state, x=date_min, xend=date_max)) +
  geom_segment(linewidth=1, color="grey40") +
  # Scales
  scale_y_discrete(drop=F) +
  scale_x_date(limits = c(ymd("1980-01-01"), ymd("2024-01-01"))) +
  # Theme
  theme_bw() + inset_theme
g4a

# Merge
layout_matrix <- matrix(data=c(1, 2, 3, 4,
                               5, 6, 7, 8), ncol=4, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4,
                             g1a, g2a, g3a, g4a,
                             layout_matrix=layout_matrix,
                             heights=c(0.83, 0.17))

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_data_maps.png"),
       width=6.5, height=3.3, units="in", dpi=600)


