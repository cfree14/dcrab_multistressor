

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/confidential/oregon/processed/"
plotdir <- "analyses/sdm/figures"

# Read data
receipts_orig <-  readRDS(file=file.path(datadir, "ODFW_1980_2023_dcrab_fish_tickets.Rds"))

# Read data
load("analyses/sdm/output/index_output_20km_mesh.Rdata")


# Build data
################################################################################

# Index data
index_df <- indexes %>% 
  # Factor
  filter(region=="Oregon") %>% 
  # Convert to metric tons
  mutate(index_mt=est/1000,
         index_mt_lo=lwr/1000,
         index_mt_hi=upr/1000) %>% 
  # Select
  select(year, index_mt) %>% 
  mutate(year_lag1=year+1,
         year_lag2=year+2,
         year_lag3=year+3,
         year_lag4=year+4)

# Landings data
landings <- receipts_orig %>%
  filter(!is.na(season)) %>% 
  group_by(season) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm = T)) %>% 
  ungroup() %>% 
  rename(season_long=season) %>% 
  mutate(season_short=substr(season_long, 1, 4) %>% as.numeric(.)) %>% 
  select(season_long, season_short, landings_lbs, everything(.))


# Plot landings vs. index
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    plot.tag = element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size=unit(0.3, "cm"),
                    legend.key=element_blank(),
                    legend.background = element_rect(fill=alpha('blue', 0)))


# Plot landings
g <- ggplot(landings, aes(x=season_short, y=landings_lbs/1e6)) +
  geom_bar(stat="identity", fill="grey70") +
  geom_line(data=index_df, mapping=aes(x=year, y=index_mt/1e3/2)) +
  geom_point(data=index_df, mapping=aes(x=year, y=index_mt/1e3/2)) +
  # Labels
  labs(x="Year", y="Landings (millions lbs)") +
  scale_x_continuous(breaks=seq(1980,2025,5)) +
  # Theme
  theme_bw() + base_theme
g

#
ggsave(g, filename=file.path(plotdir, "FigX_landings_vs_index.png"), 
       width=6.5, height=3.5, units="in", dpi=600, bg="white")

# Analysis
################################################################################

# Build data
data <- landings %>% 
  # Add index for that year
  left_join(index_df %>% select(year, index_mt), by=c("season_short"="year")) %>% 
  rename(index_lag0=index_mt) %>% 
  # Add index lag 1
  left_join(index_df %>% select(year_lag1, index_mt), by=c("season_short"="year_lag1")) %>%
  rename(index_lag1=index_mt) %>% 
  # Add index lag 2
  left_join(index_df %>% select(year_lag2, index_mt), by=c("season_short"="year_lag2")) %>%
  rename(index_lag2=index_mt) %>% 
  # Add index lag 3
  left_join(index_df %>% select(year_lag3, index_mt), by=c("season_short"="year_lag3")) %>%
  rename(index_lag3=index_mt) %>% 
  # Add index lag 4
  left_join(index_df %>% select(year_lag4, index_mt), by=c("season_short"="year_lag4")) %>%
  rename(index_lag4=index_mt)

# Gather
data_long <- data %>% 
  gather(key="lag", value="index", 4:ncol(.)) %>% 
  filter(!is.na(index)) %>% 
  mutate(lag=gsub("index_lag", "Lag ", lag)) 

g <- ggplot(data_long, aes(x=index/1e3, y=landings_lbs/1e6)) +
  facet_wrap(~lag) +
  geom_smooth(method="lm", fill = "grey80", color="black") +
  # geom_point(color="grey40") +
  geom_text(mapping=aes(label=season_short), size=2) +
  # Labels
  labs(y="Landings (millions of lbs)", x="Index of relative abundance (1000s of mt)") +
  # Theme
  theme_bw() + base_theme
g

ggsave(g, filename=file.path(plotdir, "FigX_landings_vs_index_with_lags_regressions.png"), 
       width=6.5, height=4.5, units="in", dpi=600, bg="white")

# Regressions
################################################################################

# Lag 1 complete
data_lag1 <- data %>% 
  select(season_short, landings_lbs, index_lag0, index_lag1) %>% 
  filter(season_short>=2006) %>% 
  na.omit()
lmfit1 <- lm(landings_lbs ~  index_lag0 + index_lag1, data_lag1)
summary(lmfit1)

# Lag 2
data_lag2 <- data %>% 
  select(season_short, landings_lbs, index_lag0, index_lag1, index_lag2) %>% 
  filter(season_short>=2006) %>% 
  na.omit()
lmfit2 <- lm(landings_lbs ~  index_lag0 + index_lag1 + index_lag2, data_lag2)
summary(lmfit2)

# Lag 3
data_lag3 <- data %>% 
  select(season_short, landings_lbs, index_lag0, index_lag1, index_lag2, index_lag3) %>% 
  filter(season_short>=2006) %>% 
  na.omit()
lmfit3 <- lm(landings_lbs ~  index_lag0 + index_lag1 + index_lag2 + index_lag3, data_lag3)
summary(lmfit3)

# Lag 4
data_lag4 <- data %>% 
  filter(season_short>=2006) %>% 
  na.omit()
lmfit4 <- lm(landings_lbs ~ index_lag0 + index_lag1 + index_lag2 + index_lag3 + index_lag4, data_lag4)
summary(lmfit4)



# Reverse: landings impact on future abundance
################################################################################

# Build data
data1 <- index_df %>% 
  mutate(year_lag1=year-1,
         year_lag2=year-2,
         year_lag3=year-3,
         year_lag4=year-4,
         year_lag5=year-5,
         year_lag6=year-6) %>% 
  # Add index lag 1
  left_join(landings %>% select(season_short, landings_lbs), by=c("year_lag1"="season_short")) %>%
  rename(landings_lbs1=landings_lbs) %>% 
  # Add index lag 2
  left_join(landings %>% select(season_short, landings_lbs), by=c("year_lag2"="season_short")) %>%
  rename(landings_lbs2=landings_lbs) %>% 
  # Add index lag 3
  left_join(landings %>% select(season_short, landings_lbs), by=c("year_lag3"="season_short")) %>%
  rename(landings_lbs3=landings_lbs) %>% 
  # Add index lag 4
  left_join(landings %>% select(season_short, landings_lbs), by=c("year_lag4"="season_short")) %>%
  rename(landings_lbs4=landings_lbs) %>% 
  # Add index lag 5
  left_join(landings %>% select(season_short, landings_lbs), by=c("year_lag5"="season_short")) %>%
  rename(landings_lbs5=landings_lbs) %>% 
  # Add index lag 6
  left_join(landings %>% select(season_short, landings_lbs), by=c("year_lag6"="season_short")) %>%
  rename(landings_lbs6=landings_lbs) 
  
# Gather
data1_long <- data1 %>% 
  select(-c(year_lag1:year_lag6)) %>% 
  gather(key="lag", value="landings_lbs", 3:ncol(.)) %>% 
  filter(!is.na(landings_lbs)) %>% 
  mutate(lag=gsub("landings_lbs", "Lag ", lag)) 

# Get regression stats
x <- 1
stats <- purrr::map_df(1:6, function(x){
  
  
  lmfit <- lm(data1$index_mt ~ data1[,paste0("landings_lbs", x)])
  summary(lmfit)
  
  df <- tibble(lag=paste("Lag", x),
               pvalue=freeR::pval(lmfit),
               slope=freeR::slope(lmfit))
  
})


# Plot
g <- ggplot(data1_long, aes(x=landings_lbs/1e6, y= index_mt/1e3)) +
  facet_wrap(~lag) +
  geom_smooth(method="lm", fill = "grey80", color="black") +
  geom_point(color="grey40") +
  geom_text(data=stats, aes(x=max(data1_long$landings_lbs/1e6), 
                            y=max(data1_long$index_mt/1e3), 
                            label=paste0("p=", round(pvalue, 3))),
            hjust=1, size=2.2) +
  # Labels
  labs(x="Landings (millions of lbs)", y="Index of relative abundance (1000s of mt)") +
  # Theme
  theme_bw() + base_theme
g

ggsave(g, filename=file.path(plotdir, "FigX_landings_vs_index_with_lags_regressions.png"), 
       width=6.5, height=4.5, units="in", dpi=600, bg="white")
