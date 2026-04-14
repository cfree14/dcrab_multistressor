

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "analyses/trt_paper/figures"
outdir <- "analyses/trt_paper/output"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/ca_set_gillnet_bycatch/data/injury_mortality/processed/2007_2022_injury_mortality_data.Rds")



# Build data
################################################################################

# Classify fisheries
dcrab_orwa <- c("Dungeness crab pot fishery (OR)", "Dungeness crab pot fishery (OR/WA)", "Dungeness crab pot fishery (WA-coastal)", "Dungeness crab pot fishery (WA)")
dcrab_unknown <- c("Dungeness crab pot fishery (rec)", "Dungeness crab pot fishery", "Dungeness crab pot fishery (comm)")
unknown_but_could_be_dcrab_ca <- c("Unidentified fishery", "Unidentified pot fishery")
  
# Build data
data <- data_orig %>% 
  # Humpback fishery entanglements
  filter(species=="Humpback whale" & interaction_catg=="Fishery") %>% 
  # Recode fisheries
  mutate(fishery=case_when(interaction_type=="Dungeness crab pot fishery (CA)" ~ "CA Dcrab",
                           interaction_type %in%  dcrab_orwa ~ "OR/WA Dcrab",
                           interaction_type %in%  dcrab_unknown ~ "Unknown Dcrab",
                           interaction_type %in% unknown_but_could_be_dcrab_ca ~ "Unknown fishery",
                           T ~ "Other fishery"))

# Inspect classifications
count(data, fishery, interaction_type)

# Count
stats22 <- data %>% 
  count(year, fishery) %>% 
  filter(fishery %in% c("CA Dcrab", "Unknown Dcrab", "Unknown fishery"))

# 2023 data
# https://storymaps.arcgis.com/stories/267aaa6e6a494d44be437b597f3993a3
# I don't know that all 7 Dcrab entanglements were in CA
stats23 <- tibble(year=2023,
                  fishery=c("CA Dcrab", "Unknown Dcrab", "Unknown fishery"),
                  n=c(7,0,5))

# 2024 data
#https://www.fisheries.noaa.gov/s3/2025-04/2024-whale-entanglements-report.pdf
stats24 <- tibble(year=2024,
                  fishery=c("CA Dcrab", "Unknown Dcrab", "Unknown fishery"),
                  n=c(5,1,16))

# Merge
stats <- bind_rows(stats22, stats23, stats24) %>% 
  mutate(fishery=factor(fishery, levels=c("CA Dcrab", "Unknown Dcrab", "Unknown fishery") %>% rev())) %>% 
  mutate(period=case_when(year %in% c(2014:2018) ~ "Pre",
                          year >=2019 ~ "Post",
                          T ~ NA))

# Calculate averages
avgs <- stats %>% 
  group_by(period) %>% 
  summarise(nyr=n_distinct(year),
            n_ca=sum(n[fishery=="CA Dcrab"])/nyr,
            n_tot=sum(n)/nyr) %>% 
  ungroup() %>% 
  filter(!is.na(period))
  


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
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
g <- ggplot(stats, aes(x=year, y=n, fill=fishery)) +
  geom_bar(stat="identity") +
  # Pre-management
  # geom_segment(x=2014, xend=2018, y=avgs$n_ca[avgs$period=="Pre"], color="darkred") +
  # geom_segment(x=2014, xend=2018, y=avgs$n_tot[avgs$period=="Pre"], color="black") +
  # Post management
  # geom_segment(x=2019, xend=2024, y=avgs$n_ca[avgs$period=="Post"], color="darkred") +
  # geom_segment(x=2019, xend=2024, y=avgs$n_tot[avgs$period=="Post"], color="black") +
  # Reference line
  geom_vline(xintercept=2018.5, linetype="dotted") +
  # Labels
  labs(x="Year", y="Number of entanglements") +
  # Legend
  scale_fill_manual(name="Fishery", values=c("darkorange", "grey50", "grey80") %>% rev()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_humpbacks_in_pots_no_lines.png"), 
       width=6.5, height=3.5, units="in", dpi=600, bg="white")




# Plot data
################################################################################

dcrab <- readxl::read_excel("/Users/cfree/Desktop/Marine Fisheries Data Explorer Extract - 2026-03-24 11.56.32.xlsx") 

average <- dcrab %>% 
  filter(Year>=2014) %>% 
  mutate(period=ifelse(Year<2019, "Pre", "Post")) %>% 
  group_by(period) %>% 
  summarize(yr1=min(Year),
            yr2=max(Year),
            pounds=mean(Pounds))

# Plot data
g <- ggplot(dcrab, aes(x=Year, y=Pounds/1e6)) +
  geom_bar(stat="identity", fill="grey80") +
  geom_segment(data=average, mapping=aes(x=yr1, xend=yr2, y=pounds/1e6)) +
  # geom_hline(mean(dcrab$Pounds)/1e6) +
  geom_vline(xintercept=2018.5, linetype="dotted") +
  # Labels
  labs(x="Year", y="Landings (millions lbs)") +
  scale_x_continuous(breaks=seq(1980,2025,5)) +
  ylim(c(0,NA)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_dcrab_landings.png"), 
       width=6.5, height=3.5, units="in", dpi=600, bg="white")


