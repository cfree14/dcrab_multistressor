

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "trt_paper/figures"
outdir <- "trt_paper/output"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/ca_set_gillnet_bycatch/data/injury_mortality/processed/2007_2022_injury_mortality_data.Rds")

# Read traps
traps_orig <- readRDS(file = file.path(outdir, "2010_2023_traps_by_state_fishery_week.Rds"))


# Build vertical line data
################################################################################

# Format
traps <- traps_orig %>% 
  mutate(species=factor(species, levels=c("Dungeness crab", "Sablefish", "Spot prawn")))



# Build entanglement data
################################################################################

# Types
types <- data_orig %>% 
  count(interaction_type)
types_do <- c(types$interaction_type[grepl("Dungeness", types$interaction_type)], 
              "CA spot prawn trap fishery",
              "Sablefish pot fishery (WA/OR/CA)",
              # Unidentified trap fisheries
              "Unidentified pot fishery", "Pot fishery (tribal)", "Pot fishery (catch shares)", "Crab pot fishery/hook and line fishery")

# Build data
data1 <- data_orig %>% 
  # Fisheries of interest
  filter(interaction_type %in% types_do)  %>% 
  # Categorigze interaction types
  mutate(fishery=case_when(grepl("Dungeness", interaction_type) ~ "Dungeness crab",
                           grepl("Sablefish", interaction_type) ~ "Sablefish",
                           grepl("prawn", interaction_type) ~ "Spot prawn",
                           T ~ "Unidentified trap fishery")) %>% 
  # Remove surviving?
  # filter(msi>0)
  # Summarize
  group_by(fishery, species, year, month) %>% 
  summarize(n=n()) %>% 
  ungroup() 

# Fishery colors
# colors <- c("darkorange", "darkorchid4", "darkgreen", "grey80)
colors <- c(RColorBrewer::brewer.pal(9, "Oranges")[8],
            RColorBrewer::brewer.pal(9, "Purples")[8],
            RColorBrewer::brewer.pal(9, "Greens")[8],
            "grey80")

# Look at all species
ggplot(data1, aes(x=year, y=n, fill=fishery)) +
  facet_wrap(~species) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="", y="Number of entanglements") +
  # Legend
  scale_fill_manual(name="Fishery", values=colors) +
  # Theme
  theme_bw()

# Format data for plotting
species_do <- c("Humpback whale", "Gray whale", "Blue whale", "Minke whale", "Unidentified whale")
data2 <- data1 %>% 
  # Reduce to whales
  filter(species %in% species_do) %>% 
  # Recode into other/unidentified
  mutate(species=ifelse(!species %in% c("Humpback whale", "Gray whale"), "Other/unidentified whale", species)) %>% 
  mutate(species=factor(species, levels=c("Humpback whale", "Gray whale", "Other/unidentified whale"))) %>% 
  # Summarize other/unidentified
  group_by(fishery, species, year, month) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  # Add date
   mutate(date=paste(year, month, 1, sep="-") %>% ymd())


# Plot data
################################################################################



# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    axis.title.x=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag = element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.3, "cm"),
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Vertical lines
g1 <- ggplot(traps, aes(x=date, y=ntraps/1e3, color=species)) +
  geom_line() +
  # Labels
  labs(x="", y="Thousands of vertical lines", tag="A") +
  scale_x_date(breaks=seq(ymd("2010-01-01"), 
                          ymd("2024-01-01"), by="1 years"),
               date_label="%Y") +
  # Legend
  scale_color_manual(name="Fishery", values=colors[1:3], drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g1


# Entanglements
g2a <- ggplot(data2, aes(x=year, y=n, fill=fishery)) +
  facet_wrap(~species) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="", y="Number of entanglements", tag="B") +
  # Legend
  scale_fill_manual(name="Fishery", values=colors) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.85, 0.8))
g2a

# Entanglements
g2b <- ggplot(data2, aes(x=year, y=month, size=n, color=fishery)) +
  facet_grid(fishery~species) +
  geom_point() +
  # Labels
  labs(x="", y="Month", tag="B") +
  scale_y_continuous(breaks=1:12, lim=c(1,12)) +
  # Legend
  scale_color_manual(name="Fishery", values=colors) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2b

# Merge
g <- gridExtra::grid.arrange(g1, g2a, ncol=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_vert_lines_and entanglements.png"), 
       width=6.5, height=5, units="in", dpi=600)





