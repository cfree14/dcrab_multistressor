

# install.packages("remotes")
# remotes::install_github("pfmc-assessments/nwfscSurvey")

# FRAM warehouse
# https://www.webapps.nwfsc.noaa.gov/data/map

# Packages
library(nwfscSurvey)

# 
catch = pull_catch(
  common_name = "Pacific ocean perch", 
  survey = "NWFSC.Combo")

bio = pull_bio(
  common_name = "Pacific ocean perch", 
  survey = "NWFSC.Combo")

catch1 = pull_catch(
  common_name = "Dungeness crab", 
  survey = "NWFSC.Combo")

ls("package:nwfscSurvey")

spp <- nwfscSurvey::PullSpp

# I normally access the trawl survey using this package: https://github.com/pfmc-assessments/nwfscsurvey
# However, nothing is coming up for crab
# I think the data is technically stored on the FRAM data warehouse here: https://www.webapps.nwfsc.noaa.gov/data/map
# Which does have data for dungness
# So I am wondering if the nwfscSurvey package is only set up to query the database for species in the groundfish FMP
# It could also be user error
