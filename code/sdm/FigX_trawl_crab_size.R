

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
data_orig <- readRDS(file=file.path(trawldir, "dcrab_trawl_survey_data_2023_12_09_cleaned.Rds"))

# LW-at-age key
lw_age_key <- read.csv("data/life_history/data/dcrab_length_weight_at_age_detailed.csv", as.is=T) %>% 
  mutate(width_cm=measurements::conv_unit(cw_mm, "mm", "cm"))


# Build data
################################################################################

# Length-weight parameters (Zhang et al. 2004)
# wt_g <- a*cw^b
lw_a <- 0.000102
lw_b <- 3.10

# Function to find age
sex <- "Male"; width_mm=152.23
age_at_width <- function(sex, width_mm){
  
  sex_do <- sex
  width_mm_do <- width_mm
  age_yr <- lw_age_key %>% 
    filter(sex==sex_do) %>% 
    mutate(mm_off=abs(cw_mm-width_mm_do)) %>% 
    arrange(mm_off) %>% 
    filter(mm_off==min(mm_off)) %>% 
    filter(age_yr==min(age_yr)) %>% 
    pull(age_yr)
  
}


# Build data
data <- data_orig %>% 
  # Only with crab
  filter(total_kg>0) %>% 
  # Average crab weight
  mutate(crab_kg=total_kg/total_n) %>% 
  # Convert to average width
  mutate(crab_g=crab_kg*1000,
         crab_mm=(crab_g/lw_a)^(1/lw_b)) %>% 
  # Add estimated age
  rowwise() %>% 
  mutate(age_yr_f=age_at_width(sex="Female", width_mm=crab_mm),
         age_yr_m=age_at_width(sex="Male", width_mm=crab_mm)) %>% 
  ungroup()

# Summary stats
data %>% 
  group_by(comm_name) %>% 
  summarize(crab_kg=median(crab_kg),
            crab_mm=median(crab_mm),
            age_yr_f=median(age_yr_f),
            age_yr_m=median(age_yr_m))

# Build sex age data
data_sex <- data %>% 
  select(crab_kg, age_yr_f, age_yr_m) %>% 
  gather(key="sex", value="age_yr", 2:ncol(.)) %>% 
  mutate(sex=recode(sex, 
                    "age_yr_f"="Female", 
                    "age_yr_m"="Male"))
  

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


# Plot average weight over time
g1 <- ggplot(data, aes(x=year, y=crab_kg, group=year)) +
  geom_boxplot(outlier.shape=1, outlier.size = 1.5) +
  # Limits
  lims(y=c(0, 2)) +
  # Labels
  labs(x="Year", y="Average weight (kg)") +
  # Theme
  theme_bw() + my_theme
g1

# Plot average weight
g2 <- ggplot(data %>% filter(crab_kg<2), aes(x="All years", y=crab_kg)) +
  geom_boxplot(outlier.shape=1, outlier.size = 1.5) +
  # Limits
  lims(y=c(0, 2)) +
  # Labels
  labs(x="", y="Average weight (kg)") +
  # Theme
  theme_bw() + my_theme
g2

# Plot average length 
g3 <- ggplot(data %>% filter(crab_kg<2), aes(x="All years", y=crab_mm)) +
  geom_boxplot(outlier.shape=1, outlier.size = 1.5) +
  # Limits
  lims(y=c(0, NA)) +
  # Labels
  labs(x="", y="Average width (mm)") +
  # Theme
  theme_bw() + my_theme
g3

# Plot average length 
g4 <- ggplot(data_sex %>% filter(crab_kg<2), aes(x=sex, y=age_yr, fill=sex)) +
  geom_boxplot(outlier.shape=1, outlier.size = 1.5) +
  # Limits
  lims(y=c(0, NA)) +
  # Labels
  labs(x="", y="Average age (yr)") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g4

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, widths=c(0.4, 0.15, 0.15, 0.3))
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_trawl_crab_size_age.png"), 
       width=6.5, height=3, units="in", dpi=600)

