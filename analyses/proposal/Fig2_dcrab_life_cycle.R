
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel("figures/Fig2_dcrab_life_history.xlsx", sheet=2)


# Build data
################################################################################

# Generate column names
cols <- c("stage",
          paste("0", 10:12, sep="_"),
          paste("1", 1:12, sep="_"),
          paste("2", 1:12, sep="_"),
          paste("3", 1:12, sep="_"),
          paste("4", 1:12, sep="_"),
          paste("5", 1:12, sep="_"))

# Format data
data <- data_orig %>%
  # Set names
  setNames(cols) %>%
  # Gather
  gather(key="year_month", value="value", 2:ncol(.)) %>%
  # Seperate
  tidyr::separate(year_month, sep="_", into=c("year", "month")) %>%
  # Format month/year
  mutate(year=paste("Year", year),
         year=recode(year,
                     "Year 0"=" ",
                     "Year 5"="Year 5+"),
         month=factor(month, levels=1:12)) %>%
  # Reduce
  filter(!is.na(value)) %>%
  # Order stages
  mutate(stage=recode(stage, "Adult (mature)"="Adult"),
         stage=factor(stage, levels=c("Eggs", "Pre-zoea", "Zoea (5 stages)", "Megalope", "Juvenile",
                                       "Adult", "Mating", "Vulnerable to fishing") %>% rev())) %>%
  # Add stage text
  mutate(stage_label=recode_factor(stage,
                                  "Eggs"="Deposition",
                                  "Pre-zoea"="Hatching",
                                  "Zoea (5 stages)"="Dispersal",
                                  "Megalope"="Settlement",
                                  "Juvenile"="Growing/molting",
                                  "Adult"="Growing/molting",
                                  "Mating"="Mating",
                                  "Vulnerable to fishing"="Fishing"))

# Build process key
process_key <- data %>%
  # Reduce to one per stage / year
  group_by(stage, stage_label, year) %>%
  arrange(stage, stage_label, year, month) %>%
  slice(1) %>%
  ungroup() %>%
  # Remove select ones
  filter(!(stage=="Juvenile" & year=="Year 2")) %>%
  filter(!(stage=="Adult" & year %in% c( "Year 4", "Year 5+"))) %>%
  filter(!(stage=="Vulnerable to fishing" & year %in% c( "Year 4")))




# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.x=element_text(size=5),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Colors
colors <- RColorBrewer::brewer.pal(nlevels(data$stage_label)+1, "YlOrBr")[2:(nlevels(data$stage_label)+1)]

# Plot data
g <- ggplot(data, aes(x=month, y=stage, fill=stage_label)) +
  facet_grid(.~year, scales="free_x", space="free_x") +
  # Plot period
  geom_tile(show.legend = F) +
  # Plot process text
  geom_text(data=process_key, mapping=aes(x=month, y=stage, label=stage_label),
            inherit.aes = F, hjust=0, size=1.8, nudge_x = -0.3) +
  # Reference line
  geom_hline(yintercept=2.5, linetype="dashed") +
  # Labels
  labs(y="Life stage", x="Month of year") +
  # X-axis
  scale_x_discrete(breaks=1:12, labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  # Legend
  scale_fill_manual(name="", values=colors) +
  # Theme
  theme_bw() + my_theme +
  theme(panel.spacing = unit(0, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_dcrab_life_cycle.png"),
       width=6.5, height=2, units="in", dpi=600)


