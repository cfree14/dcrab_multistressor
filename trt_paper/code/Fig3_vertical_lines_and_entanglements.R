

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

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/ca_set_gillnet_bycatch/data/injury_mortality/processed/2007_2022_injury_mortality_data.Rds")


# Build data
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
data <- data_orig %>% 
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


# Plot data
################################################################################

# Fishery colors
# colors <- c("darkorange", "darkorchid4", "darkgreen", "grey80)
colors <- c(RColorBrewer::brewer.pal(9, "Oranges")[8],
            RColorBrewer::brewer.pal(9, "Purples")[8],
            RColorBrewer::brewer.pal(9, "Greens")[8],
            "grey80")

# Look at all species
ggplot(data, aes(x=year, y=n, fill=fishery)) +
  facet_wrap(~species) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="", y="Number of entanglements") +
  # Legend
  scale_fill_manual(name="Fishery", values=colors) +
  # Theme
  theme_bw()

# Format data for plotting
species_do <- c("Humpback whale", "Gray whale", "Blue whale")
data_plot <- data %>% 
  filter(species %in% species_do) %>% 
  mutate(species=factor(species, species_do))

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
g1 <- ggplot() +
  geom_point(mapping=aes(x=2010:2024, y=rep(10000, 15)), color="white") +
  # Labels
  lims(y=c(0, 10000)) +
  labs(y="Number of vertical lines", tag="A") +
  scale_x_continuous(breaks=seq(2010, 2024, 2)) +
  # Theme
  theme_bw() + base_theme
g1

# Entanglements
g2 <- ggplot(data_plot, aes(x=year, y=n, fill=fishery)) +
  facet_wrap(~species) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="", y="Number of entanglements", tag="B") +
  # Legend
  scale_fill_manual(name="Fishery", values=colors) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.85, 0.8))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_vert_lines_and entanglements.png"), 
       width=6.5, height=5, units="in", dpi=600)





