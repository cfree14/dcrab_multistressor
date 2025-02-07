

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

# Read marginal effects
output <- readRDS(file.path(outdir, "sdm_trawl_tweedie_scaled_vars_me_link.Rds"))

# Extract components
fit <- output[[1]]
residuals <- output[[2]]


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   axis.title.x = element_blank(),
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
g <- ggplot(fit, aes(x=value, y=est)) +
  facet_wrap(~variable, scales="free") +
  # Plot residuals
  geom_point(data=residuals, mapping=aes(x=value, y=residual),
             alpha=0.2, shape=1, color="grey10", size=0.5) +
  # Plot fit
  geom_ribbon(mapping=aes(x=value, ymin=est_lo, ymax=est_hi), fill="grey80", alpha=0.7) +
  geom_line() +
  # Labels
  labs(x="", y="Marginal effect\non the link-scale") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_marginal_effects_link.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

