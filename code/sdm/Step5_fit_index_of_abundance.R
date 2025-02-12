

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

# Index standardization with sdmTMB
# https://pbs-assess.github.io/sdmTMB/articles/index-standardization.html

qcs_grid <- qcs_grid

# Build data
################################################################################

# Build data
data <- data_orig

# Build grid
range(data$lat_dd)
range(data$long_dd)
lats <- seq(31.9, 48.5, 0.1)
longs <- seq(-126.0, -117.2, 0.1)
yrs <- unique(data$year)
grid_yrs <- expand.grid(lat_dd=lats,
                        long_dd=longs,
                        year=yrs) %>% 
  # Mark region
  mutate(region=cut(lat_dd, breaks=c(0, 38+46.125/60, 42, 46.25, Inf), labels=c("cCA", "nCA", "OR", "WA")))

# Regional grids
grid_yrs_wa <- grid_yrs %>% filter(region=="WA")
grid_yrs_or <- grid_yrs %>% filter(region=="OR")
grid_yrs_nca <- grid_yrs %>% filter(region=="nCA")
grid_yrs_cca <- grid_yrs %>% filter(region=="cCA")


# Model construction
################################################################################

# Build mesh
mesh <- make_mesh(data, c("long_dd", "lat_dd"), cutoff = 0.2)
plot(mesh)

# Fit model
m <- sdmTMB(
  data =data, 
  formula = cpue_kg_km2 ~ 0 + as.factor(year),
  time = "year", 
  mesh = mesh, 
  family = tweedie(link = "log"))

# Record and inspect residuals
data$resids <- residuals(m, type="mle-mvn")
hist(data$resids)
qqnorm(data$resids)
abline(a = 0, b = 1)

# Make predictions
predictions <- predict(m, newdata = grid_yrs, return_tmb_object = TRUE)

# Extract index
index <- get_index(predictions, area = 0.1*0.1, bias_correct = TRUE)

# Add CV
index1 <- index %>% 
  mutate(cv = sqrt(exp(se^2) - 1))

# Plot index
ggplot(index, aes(year, est)) + 
  geom_line() +
  # geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  xlab('Year') + ylab('Biomass estimate (kg)')

# Region-level predictions
preds_cca <- predict(m, newdata = grid_yrs_cca, return_tmb_object = TRUE)
preds_nca <- predict(m, newdata = grid_yrs_nca, return_tmb_object = TRUE)
preds_or <- predict(m, newdata = grid_yrs_or, return_tmb_object = TRUE)
preds_wa <- predict(m, newdata = grid_yrs_wa, return_tmb_object = TRUE)

# Extract regional indices
index_cca <- get_index(preds_cca, area = 0.1*0.1, bias_correct = TRUE)
index_nca <- get_index(preds_nca, area = 0.1*0.1, bias_correct = TRUE)
index_or <- get_index(preds_or, area = 0.1*0.1, bias_correct = TRUE)
index_wa <- get_index(preds_wa, area = 0.1*0.1, bias_correct = TRUE)

# Format regional indices
index_cca1 <- index_cca %>% mutate(region="C. California")
index_nca1 <- index_nca %>% mutate(region="N. California")
index_or1 <- index_or %>% mutate(region="Oregon")
index_wa1 <- index_wa %>% mutate(region="Washington")

# Merge regional indices
indexes <- bind_rows(index_cca1, index_nca1, index_or1, index_wa1)

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "none",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot regional estimates
g <- ggplot(indexes, aes(x=year, y=est/10000, color=region)) + 
  # Facet
  facet_wrap(~region, ncol=4, scales="free_y") +
  # Data
  geom_line() +
  # geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  lims(y=c(0,NA)) +
  # Labs
  labs(x="Year", y="Index of relative abundance") +
  # Theme 
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_index_of_abundance.png"), 
       width=6.5, height=2, units="in", dpi=600)





