

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


# Example data
pcod <- sdmTMB::pcod
qcs_grid <- sdmTMB::qcs_grid
grid_yrs <- replicate_df(qcs_grid, "year", unique(pcod$year))


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce
  filter(!is.na(cpue_kg_km2) & !is.na(glorys_c)) %>% 
  # Add presence/absence
  mutate(present=ifelse(cpue_kg_km2>0, 1, 0)) %>% 
  # Scale variables
  mutate(depth_m_scaled=scale(depth_m) %>% as.numeric(),
         glorys_c_scaled=scale(glorys_c) %>% as.numeric())

# Calculate lat/long midpoint
get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
}

# Build temporary grid
grid_temp <- data %>% 
  # Add lat/long bins
  mutate(lat_dd_bin=cut(lat_dd, breaks=seq(41, 51, 0.2)),
         long_dd_bin=cut(long_dd, breaks=seq(-130, -110, 0.2))) %>% 
  # Simplify
  select(year, date, trawl_id, tow, pass, lat_dd_bin, long_dd_bin,  
         depth_m_scaled, glorys_c_scaled) %>% 
  # Gather
  gather(key="variable", value="value", 8:ncol(.)) %>% 
  # Summarize
  group_by(year, lat_dd_bin, long_dd_bin, variable) %>%
  summarize(value_avg=mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  # Spread
  spread(key="variable", value="value_avg") %>% 
  # Add midpoints
  rowwise() %>% 
  mutate(lat_dd=get_midpoint(lat_dd_bin),
         long_dd=get_midpoint(long_dd_bin)) %>% 
  ungroup() %>% 
  # Arrange
  select(year:long_dd_bin, lat_dd, long_dd, everything()) %>%
  # Simplify
  select(year, lat_dd, long_dd, 
         depth_m_scaled, glorys_c_scaled)

freeR::complete(grid_temp)
range(grid_temp$lat_dd)


# Model construction
################################################################################

# Build mesh
# mesh <- make_mesh(data, c("long_dd", "lat_dd"), cutoff = 0.1)
mesh <- make_mesh(data, c("long_dd", "lat_dd"), cutoff = 0.2)
plot(mesh)

# Density w/ autocorrelated spatial random effects and smoothers
m4 <- sdmTMB(
  data = data,
  formula = cpue_kg_km2 ~ s(depth_m_scaled) + s(glorys_c_scaled),
  mesh = mesh,
  # family = tweedie(link = "log"),
  family = delta_lognormal(),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID"
)
m4
sanity(m4)

# Export
# saveRDS(m4, file.path(outdir, "sdm_trawl_tweedie_glorys.Rds"))
saveRDS(m4, file.path(outdir, "sdm_trawl_delta_glorys.Rds"))


# Model diagnostics
################################################################################

# Compute residuals
data$resids <- residuals(m4) # randomized quantile residuals

# Visualize residuals
qqnorm(data$resids)
qqline(data$resids)

# Spatial residuals
ggplot(data, aes(long_dd, lat_dd, col = resids)) +
  scale_colour_gradient2() +
  geom_point() +
  facet_wrap(~year) +
  coord_fixed()


# Marginal effects
################################################################################

visreg_delta(m4, xvar = "depth_m_scaled", model = 1, gg = TRUE)
visreg_delta(m4, xvar = "depth_m_scaled", model = 2, gg = TRUE)

visreg_delta(m4, xvar = "glorys_c_scaled", model = 1, gg = TRUE)
visreg_delta(m4, xvar = "glorys_c_scaled", model = 2, gg = TRUE)





p <- predict(m4, newdata = grid_yrs)

# Marginal effects with visreg
# https://pbs-assess.github.io/sdmTMB/articles/visreg.html


# Loop through scaled parameters and extract marginal effects
vars <- c("depth_m_scaled", "glorys_c_scaled")
for(i in 1:length(vars)){
  
  # Compute marginal effects
  var <- vars[i]
  print(paste(i, var))
  d <- visreg(m4, xvar = var, scale="linear", plot=F)
  
  # Extract fit and residuals
  fit <- d[["fit"]]
  res <- d[["res"]]
  
  # Format fit
  fit1 <- fit %>% 
    select(c(var, "visregFit", "visregLwr", "visregUpr")) %>% 
    setNames(c("value", "est", "est_lo", "est_hi")) %>% 
    mutate(variable=var) %>% 
    select(variable, everything())
  
  # Format residuals
  res1 <- res %>% 
    select(c(var, "visregRes", "visregPos")) %>% 
    setNames(c("value", "residual", "positive_yn")) %>% 
    mutate(variable=var) %>% 
    select(variable, everything())
    
  
  # Merge with other variables
  if(i == 1){
    fit_out <- fit1 
    res_out <- res1
  }else{
    fit_out <- bind_rows(fit_out, fit1)
    res_out <- bind_rows(res_out, res1)
  }
  
}

# Calculate scale parameters
scale_key <- data %>% 
  # Simplify
  select(trawl_id, depth_m, glorys_c) %>% 
  # Calculate variable average/sd
  gather(key="variable", value="value", 2:ncol(.)) %>% 
  group_by(variable) %>% 
  summarize(value_avg=mean(value),
           value_sd=sd(value)) %>% 
  ungroup() %>% 
  # Rename variable for merging
  mutate(variable=paste0(variable, "_scaled"))

# Format fits
fit_out1 <- fit_out %>% 
  # Add scale parameters
  left_join(scale_key) %>% 
  rename(value_scaled=value) %>% 
  # Calculate original value
  mutate(value=value_scaled * value_sd + +value_avg) %>% 
  # Rename
  mutate(variable=recode_factor(variable, 
                               "depth_m_scaled"="Depth (m)", 
                               "glorys_c_scaled"="Temperature (°C)")) %>% 
  # Arrange
  select(variable, value, value_scaled, est, est_lo, est_hi, everything())

# Format residuals
res_out1 <- res_out %>% 
  # Add scale parameters
  left_join(scale_key) %>% 
  rename(value_scaled=value) %>% 
  # Calculate original value
  mutate(value=value_scaled * value_sd + +value_avg) %>% 
  # Rename
  mutate(variable=recode_factor(variable, 
                                "depth_m_scaled"="Depth (m)", 
                                "glorys_c_scaled"="Temperature (°C)")) %>% 
  # Arrange
  select(variable, value, value_scaled, residual, positive_yn, everything())

# Setup theme
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
ggplot(fit_out1, aes(x=value, y=est)) +
  facet_wrap(~variable, scales="free") +
  # Plot fit
  geom_ribbon(mapping=aes(x=value, ymin=est_lo, ymax=est_hi), fill="grey80") +
  geom_line() +
  # Plot residuals
  geom_point(data=res_out1, mapping=aes(x=value, y=residual),
             alpha=0.2, shape=1, color="grey10") +
  # Labels
  labs(x="Value", y="Marginal effect") +
  # Theme
  theme_bw() + my_theme

range(data$depth_m)
range(data$glorys_c)





