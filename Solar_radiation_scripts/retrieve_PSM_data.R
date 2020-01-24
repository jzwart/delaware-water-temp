# Code source: https://github.com/dazhiyang/SolarData
# https://github.com/dazhiyang/SolarData/blob/master/examples/PSMExample.r

# Install and load devtools package
#install.packages("devtools")

# Install and load the SolMod package
install_github("dazhiyang/SolarData")

#load libraries
library(devtools)
library(SolarData)
library(gridExtra)
library(XML)
library(dplyr)
library(lubridate)
library(tidyverse)
library("data.table")

##### Retrieve PSM data at location of all DEOS stations that are within the DRB #####

### define function

# See https://developer.nrel.gov/docs/solar/nsrdb/psm3_data_download/ for request parameter explanations
retrieve_PSM <- function(station_info, user_api, user_name, user_affiliation, yr, user_email, yr_leap, folder_path){
  # Iterate through stations to...
  for (row in 1:nrow(station_info)){
    # Set input coordinates
    longitude = station_info[row, "Lon_dd"]
    latitude = station_info[row, "Lat_dd"]
    # Retreive PSM data
    PSM.get(lon = longitude, lat = latitude, api.key = user_api,
              attributes <- 'air_temperature,ghi,dhi,dni,clearsky_dhi,clearsky_dni,clearsky_ghi,relative_humidity,solar_zenith_angle', name = user_name, affiliation = user_affiliation,
              year = yr, interval = '60', leap.year = yr_leap, utc = 'false', reason = 'research',
              email = user_email, mailing.list = 'false', directory = folder_path)
  }
}


### Run fuction
# Load .csv of DEOS stations within DRB
DEOS_stations <- read.csv('data-raw/DEOS/DEOS_DRB.csv', header = TRUE)

# Define parameters
# See https://developer.nrel.gov/docs/solar/nsrdb/psm3_data_download/ for request parameter explanations
# Requires API key (https://developer.nrel.gov/docs/api-key/). sign up here: https://developer.nrel.gov/signup/
in_api <- 'F5EcW2D6gb8ilF8Stftz1JjgE0DPnunH8HPb5ckx'
in_name <- 'Hayley+CorsonDosch'
in_affiliation <- 'UW_Madison'
in_year <- '2015'
in_email <- 'corsondosch@wisc.edu'
in_leap_yr <- 'false'
in_dir <- "data-raw/PSM/PSM_raw"

# Call function
retrieve_PSM(station_info = DEOS_stations, user_api = in_api, user_name = in_name, user_affiliation = in_affiliation, yr = in_year, user_email = in_email, yr_leap = in_leap_yr, folder_path = in_dir)



##### Read and format PSM data #####

format_PSM <- function(file_list, DEOS_station_info, yr){
  # Iterate through files in folder to...
  for (file in file_list){
    # read raw data (skip first two rows of .csv)
    PSM_tmp <- read.csv(file, header = TRUE, skip = 2)
    # get date.time for the PSM data
    # Note - as currently written this produces a warning that 1 failed to parse, but seems to work ok?
    PSM_tmp <- PSM_tmp %>%
      mutate(., Date = lubridate::ymd(paste(Year, Month, Day, sep = "-"))) %>%
      mutate(., Time = lubridate::ymd_hm(paste(paste(Year, Month, Day, sep = '-'), paste(Hour, Minute, sep = ":"), sep = " "), tz = "America/Chicago")) %>%
      mutate(., Data_source = "PSM") %>%
      dplyr::select(., -(1:5))
    # Iterate through list of DEOS stations to match and assign correct DEOS station name to data extracted from PSM
    for (row in 1:nrow(DEOS_station_info)){
      # If station latitude matches file name, and...
      if (grepl(DEOS_station_info[row, "Lat_dd"], file, fixed=TRUE)){
        # station longitude matches file name, then...
        if (grepl(DEOS_station_info[row, "Lon_dd"], file, fixed=TRUE)){
          # assign the station latitude and longitude to the extracted PSM data
          PSM_tmp <- PSM_tmp %>%
            mutate(.,Station = DEOS_station_info[row, "Station"], Latitude = DEOS_station_info[row, "Lat_dd"], Longitude = DEOS_station_info[row, "Lon_dd"])
          # set file name for hourly data
          hourly_output_name <- paste0('data-raw/PSM/PSM_hourly/PSM_hourly_', DEOS_station_info[row, "Station"], "_", yr, '.csv')
          # set file name for daily data
          daily_output_name <- paste0('data-raw/PSM/PSM_daily/PSM_daily_', DEOS_station_info[row, "Station"], "_", yr, '.csv')
          # set station name
          station_name <- DEOS_station_info[row, "Station"]
        } else {
          next
        }
      } else {
        next
      }
    }
    # Write csv of hourly data for each station
    message(paste0('Exporting all hourly data retrieved for ', station_name))
    write.csv(PSM_tmp, hourly_output_name, row.names = FALSE)
    # Group data into daily data
    message(paste0('Formatting all hourly data retrieved for ', station_name, ' as daily data'))
    PSM_dat_daily <- PSM_tmp %>%
      group_by(Date, Station, Latitude, Longitude, Data_source) %>%
      summarize(temp_mean = mean(as.numeric(Temperature)),
                rel_humidity_mean = mean(as.numeric(Relative.Humidity)),
                solar_radiation_mean = mean(as.numeric(GHI)),
                solar_radiation_max = max(as.numeric(GHI)),
                solar_radiation_sum = sum(as.numeric(GHI)))
    # Write csv of daily data for each station
    message(paste0('Exporting all formatted daily data retrieved for ', station_name))
    write.csv(PSM_dat_daily, daily_output_name, row.names = FALSE)
  }
  #Bind all daily PSM data into one dataframe and export to csv
  message('Compiling all daily data retrieved for all stations into a single dataframe')
  PSM_daily_files <- list.files("data-raw/PSM/PSM_daily/", pattern="*.csv", full.names = TRUE)
  PSM_daily_all <- map_df(PSM_daily_files, ~read_csv(.), .id = NULL)
  PSM_daily_all_filename <- paste0('data-raw/Formatted_daily_data/PSM_daily_', in_year, '.csv')
  # Export all daily PSM data as .csv
  message('Exporting all formatted daily data for all stations as a single .csv file')
  write.csv(PSM_daily_all, PSM_daily_all_filename, row.names = FALSE)
}


### Call function
# Get parameters for function
PSM_raw_files <- list.files(path="data-raw/PSM/PSM_raw", pattern = "*.csv", full.names = TRUE)
DEOS_stations <- read.csv('data-raw/DEOS/DEOS_DRB.csv', header = TRUE)
in_year <- '2015'
# Call function
format_PSM(file_list = PSM_raw_files, DEOS_station_info = DEOS_stations, yr = in_year)



