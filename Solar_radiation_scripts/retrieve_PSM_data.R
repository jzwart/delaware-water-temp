# Code source: https://github.com/dazhiyang/SolarData
# https://github.com/dazhiyang/SolarData/blob/master/examples/PSMExample.r

# Install and load devtools package
install.packages("devtools")

# Install and load the SolMod package
install_github("dazhiyang/SolarData")

#load libraries
library("devtools")
library("SolarData")


# set working directory
dir_PSM <- "C:/Users/Hayley/Documents/USGS/R/delaware-water-temp"
setwd(dir_PSM)

# Get PSM data at location of DEOS Westchester, PA (DWCH) station
# See https://developer.nrel.gov/docs/solar/nsrdb/psm3_data_download/ for request parameter explanations
# Requires API key (https://developer.nrel.gov/docs/api-key/). sign up here: https://developer.nrel.gov/signup/
# DWCH coordinates: (39.93333333, -75.55000000)
DEOS_lat = 39.93333333
DEOS_lon = -75.55000000
PSM.get(lon = DEOS_lon, lat = DEOS_lat, api.key <- 'F5EcW2D6gb8ilF8Stftz1JjgE0DPnunH8HPb5ckx', attributes <- 'air_temperature,ghi,dhi,dni,clearsky_dhi,clearsky_dni,clearsky_ghi,relative_humidity,solar_zenith_angle', name = 'Hayley+CorsonDosch', affiliation = 'UW_Madison', year = '2015', interval = '60', leap.year = 'false', utc = 'false', reason = 'research', email = 'corsondosch@wisc.edu', mailing.list = 'false')

# read psm data

PSM_tmp <- read.csv("data-raw/39.93333333_-75.55_2015.csv", header = TRUE, skip = 2)
# get date.time for the PSM data
# Note - as currently written this produces a warning that 1 failed to parse
PSM_tmp <- PSM_tmp %>%
  mutate(., Date = lubridate::ymd(paste(Year, Month, Day, sep = "-"))) %>%
  mutate(., Time = lubridate::ymd_hm(paste(paste(Year, Month, Day, sep = '-'), paste(Hour, Minute, sep = ":"), sep = " "), tz = "America/Chicago")) %>%
  dplyr::select(., -(1:5))

# write new data file
write.csv(PSM_tmp, 'data-raw/PSM_hourly_39.93333333_-75.55_2015.csv', row.names = FALSE)

# reduce to daily values

PSM_dat_daily <- PSM_tmp %>%
  group_by(Date) %>%
  summarize(temp_mean = mean(as.numeric(Temperature)),
            rel_humidity_mean = mean(as.numeric(Relative.Humidity)),
            solar_radiation_mean = mean(as.numeric(GHI)),
            solar_radiation_max = max(as.numeric(GHI)),
            solar_radiation_sum = sum(as.numeric(GHI)))

write.csv(PSM_dat_daily, 'data-raw/data_daily/PSM_daily_39.93333333_-75.55_2015.csv', row.names = FALSE)


