

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

# Read model
model <- readRDS(file.path(outdir, "sdm_trawl_tweedie_scaled_vars.Rds"))

# Extract data
data <- model$data


# Function to extract marginal effects
################################################################################

# Extract marginal effects
scale_type <- "response"
extract_effects <- function(model, scale_type){
  
  # Marginal effects with visreg
  # https://pbs-assess.github.io/sdmTMB/articles/visreg.html
  
  # Variables
  vars <- c("depth_m_scaled", "temp_c_scaled", "salinity_psu_scaled", "pco2_uatm_scaled", "do_umol_l_scaled")
  
  # Loop through scaled parameters and extract marginal effects
  for(i in 1:length(vars)){
    
    # Compute marginal effects
    var <- vars[i]
    print(paste(i, var))
    me <- visreg(model, xvar = var, scale=scale_type, plot=F)
    
    # Extract fit and residuals
    fit <- me[["fit"]]
    res <- me[["res"]]
    
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
  
  # Extract data
  data <- model$data
  
  # Calculate scale parameters
  scale_key <- data %>% 
    # Simplify
    select(trawl_id, depth_m, salinity_psu:pco2_uatm) %>% 
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
                                  "temp_c_scaled"="Temperature (°C)",
                                  "salinity_psu_scaled"="Salinity (psu)", 
                                  "pco2_uatm_scaled"="pCO2 (µatm)", 
                                  "do_umol_l_scaled"="Dissolved oxygen (µmol/l)")) %>% 
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
                                  "temp_c_scaled"="Temperature (°C)",
                                  "salinity_psu_scaled"="Salinity (psu)", 
                                  "pco2_uatm_scaled"="pCO2 (µatm)", 
                                  "do_umol_l_scaled"="Dissolved oxygen (µmol/l)")) %>% 
    # Arrange
    select(variable, value, value_scaled, residual, positive_yn, everything())
  
  # Package
  out <- list(fit_out1, res_out1)
  
  # Return
  return(out)
  
}


# Function to plot effect
plot_effects <- function(output){
  
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
  
  # Extract
  fit_out1 <- output[[1]]
  res_out1 <- output[[2]]
  
  # Plot data
  ggplot(fit_out1, aes(x=value, y=est)) +
    facet_wrap(~variable, scales="free") +
    # Plot fit
    geom_ribbon(mapping=aes(x=value, ymin=est_lo, ymax=est_hi), fill="grey80") +
    geom_line() +
    # Plot residuals
    # geom_point(data=res_out1, mapping=aes(x=value, y=residual),
    #            alpha=0.2, shape=1, color="grey10") +
    # Labels
    labs(x="Value", y="Marginal effect") +
    # Theme
    theme_bw() + my_theme
  
}


# Extract, plot, export
################################################################################

out_link <- extract_effects(model=model, scale_type="linear")
out_response <- extract_effects(model=model, scale_type="response")

plot_effects(out_link)
plot_effects(out_response)

saveRDS(out_link, file.path(outdir, "sdm_trawl_tweedie_scaled_vars_me_link.Rds"))
saveRDS(out_response, file.path(outdir, "sdm_trawl_tweedie_scaled_vars_me_response.Rds"))



