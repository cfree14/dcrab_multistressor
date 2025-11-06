

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/confidential/washington/processed/"
plotdir <- "analyses/fishing_grounds/figures"
outdir <- "analyses/fishing_grounds/output"

# Read data
wa_orig <- readRDS(file=file.path(outdir, "fishing_grounds_wa.Rds"))
wa_ports_orig <- readRDS(file=file.path(outdir, "ports_wa.Rds"))
or_orig <- readRDS(file=file.path(outdir, "fishing_grounds_or.Rds"))
or_ports_orig <- readRDS(file=file.path(outdir, "ports_or.Rds"))
ca_orig <- readRDS(file=file.path(outdir, "fishing_grounds_ca.Rds"))
ca_ports_orig <- readRDS(file=file.path(outdir, "ports_ca.Rds"))

# Merge
################################################################################

# # Clean WA
# wa <- wa_orig %>% 
#   remove_rownames() %>% 
#   mutate(state="Washington") %>% 
#   select(state, port, percentile)
# 
# # Clean OR
# or <- or_orig %>% 
#   remove_rownames() %>% 
#   mutate(state="Oregon") %>% 
#   select(state, port, percentile)

# Merge
all <- bind_rows(wa_orig, or_orig, ca_orig)
ports <- bind_rows(wa_ports_orig, or_ports_orig, ca_ports_orig)

# Export
################################################################################

# Export
saveRDS(all, file=file.path(outdir, "fishing_grounds.Rds"))
saveRDS(ports, file=file.path(outdir, "ports.Rds"))





