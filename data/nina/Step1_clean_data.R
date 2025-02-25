

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/nina"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "WilsonRequest_PCRGData.xlsx"), na = c("NA", "-", "n/a", "?", "na"), col_types = "text")
sites_orig <- readxl::read_excel(file.path(datadir, "WilsonRequest_PCRGData.xlsx"), sheet=2)

# TO-DO
# Clean start/end times
# Plot checks
# What is site PWH?

# Clean data
################################################################################

# Clean sites
sites <- sites_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(site=site_name, 
         lat_dd=latitide,
         long_dd=longitude) %>% 
  # Simplify
  select(site_code, site, site_type, organization, lat_dd, long_dd) %>% 
  # Arrange
  arrange(desc(lat_dd))

# Inspect
str(sites)

# Clean data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(site_code=site,
         date_orig=date,
         proper_fishing_yn=did_the_trap_fish_properly_y_n,
         time_start=timer_start,  
         time_end=timer_end, 
         n_nights=number_nights_fished,
         n_hours=hours_fished,
         subsample_yn=subsample,
         n_megalope=metacarcinus_magistermegalopae,
         n_instar=metacarcinus_magisterinstar) %>% 
  # Convert to numeric
  mutate(n_nights=as.numeric(n_nights),
         n_megalope=as.numeric(n_megalope),
         n_instar=as.numeric(n_instar)) %>% 
  # Format site
  mutate(site_code=toupper(site_code)) %>% 
  # Add site data
  left_join(sites, by="site_code") %>% 
  # Proper fishing
  mutate(proper_fishing_yn=recode(proper_fishing_yn,
                                  "y"="Y",
                                  "n"="N")) %>% 
  # Subsample
  mutate(subsample_yn=toupper(subsample_yn) %>% substr(., 1, 1)) %>% 
  # Format date
  mutate(date_orig=recode(date_orig,
                          "21090804"="20190804"),
         date_length=nchar(date_orig),
         date1=ifelse(date_length==5, date_orig, NA),
         date2=ifelse(date_length>5, date_orig, NA),
         date1=as.numeric(date1) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.) %>% as.character(),
         date2=ymd(date2) %>% as.character(),
         date=ifelse(!is.na(date1), date1, date2) %>% ymd(.)) %>% 
  # Remove useless date info
  select(-c(date_length, date1, date2, date_orig)) %>% 
  # Format number of hours
  mutate(n_hours=recode(n_hours,
                        "21:00"="21",
                        "24:00"="24", 
                        "25:00"="25",           
                        "25:30"="25.5", 
                        "30:00"="30",            
                        "31:30"="31.5") %>% as.numeric(.)) %>% 
  # Remove negative hours
  mutate(n_hours=ifelse(n_hours<0, NA, n_hours)) %>% 
  # Add CPUE
  mutate(megalope_hr=n_megalope/n_hours,
         instar_hr=n_instar/n_hours) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Arrange
  select(site_code, site, site_type, organization, lat_dd, long_dd,
         year, date, time_start, time_end, n_nights, n_hours, 
         proper_fishing_yn, subsample_yn, 
         n_megalope, n_instar, megalope_hr, instar_hr,
         everything())

# Inspect
freeR::complete(data)
str(data)

# Inspect more
range(data$date)
table(data$site_code)
table(data$proper_fishing_yn)
table(data$subsample_yn)

# Export data
saveRDS(data, file=file.path(datadir, "nina_data_cleaned.Rds"))
write.csv(data, file=file.path(datadir, "nina_data_cleaned.csv"), row.names = F)



# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
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


# Plot map
g1 <- ggplot(sites, aes(x=long_dd, y=lat_dd)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot sites
  geom_point(size=1.5) +
  ggrepel::geom_text_repel(aes(label=site_code), size=1.8) +
  # Crop
  coord_sf(xlim = c(-123.5, -122), ylim = c(47, 49)) +
  # Labels
  labs(x="", y="", tag="A") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text=element_blank())
g1

# Format data for plotting
data1 <- data %>% 
  # Factor sites
  mutate(site_code=factor(site_code, levels=c(sites$site_code) %>% rev())) %>% 
  # New date
  mutate(date1=paste("2024", lubridate::month(date), lubridate::day(date), sep="-") %>% ymd())

# Slit data
data_non0s <- data1 %>% filter(megalope_hr>0)
data_0s <- data1 %>% filter(megalope_hr==0)

# Plot time series
g2 <- ggplot(data_non0s, aes(y=site_code, x=date1, fill=megalope_hr, size=megalope_hr)) +
  facet_wrap(~year, ncol=1) +
  geom_point(pch=21, alpha=0.5) +
  geom_point(data=data_0s, mapping=aes(y=site_code, x=date1), pch="x", inherit.aes = F) +
  # Scale
  scale_size_continuous(name="Megalope per hour", trans="log10",
                        breaks=c(0.1, 1, 10, 100, 1000), 
                        labels=c("0.1", "1", "10", "100", "1000")) +
  scale_fill_gradientn(name="Megalope per hour", trans="log10",
                       breaks=c(0.1, 1, 10, 100, 1000),
                       labels=c("0.1", "1", "10", "100", "1000"),
                      colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Labels
  labs(x="", y="", tag="B") +
  # X-axis
  scale_x_date(breaks=seq(ymd("2024-04-01"), 
                          ymd("2024-10-01"), by="1 months"),
               date_label="%b") +
  # Theme
  theme_bw() + my_theme
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.2, 0.8))


# Export
ggsave(g, filename=file.path(datadir, "FigX_megalope_data.png"), 
       width=6.5, height=2.5, units="in", dpi=600)

