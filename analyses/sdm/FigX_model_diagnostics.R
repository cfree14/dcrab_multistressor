

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

# Read model
model <- readRDS(file.path(outdir, "sdm_trawl_tweedie_scaled_vars.Rds"))

# Extract data
data <- model$data

# sdmTMB article
# https://pbs-assess.github.io/sdmTMB/articles/residual-checking.html


# Model diagnostics
################################################################################

# Sanity check
sanity(model)

# Compute residuals
data$resids <- residuals(model, type="mle-mvn") # randomized quantile residuals

# Visualize residuals
qq_list <- qqnorm(data$resids)
qq_df <- tibble(x=qq_list[["x"]],
                y=qq_list[["y"]])
qq_line <- qqline(data$resids)


hist(data$resids)

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# QQ plot
g1 <- ggplot(qq_df, mapping=aes(x=x, y=y)) +
  geom_point() +
  # Labels
  labs(x="Theoretical quantiles", y="Sample quantiles", tag="A") +
  # Theme
  theme_bw() + my_theme
g1

# Get USA
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")

# Spatial residuals
g2 <- ggplot(data, aes(long_dd, lat_dd, col = resids)) +
  # Facet
  facet_wrap(~year, ncol=10) +
  # Plot land
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes=F) +
  # Data
  geom_point() +
  # Legend
  scale_colour_gradient2(name="Residuals") +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2,)) +
  # Labs
  labs(x="", y="", tag="B") +
  # Crop
  coord_sf(xlim = c(-126, -123), ylim = c(41.8, 48)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.position = c(0.98, 0.2))
g2

# Merge
layout_matrix <- matrix(data=c(1,NA,NA,
                               2,2,2), byrow=T, ncol=3)
g <- gridExtra::grid.arrange(g1,g2, layout_matrix=layout_matrix, heights=c(0.33, 0.67))


# Plot data
################################################################################

# Export
ggsave(g, filename=file.path(plotdir, "FigX_model_diagnostics.png"),
       width=5.5, height=4.5, units="in", dpi=600)

