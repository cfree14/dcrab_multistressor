

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
data$resids <- residuals(m, type="'mle-mvn'")
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







