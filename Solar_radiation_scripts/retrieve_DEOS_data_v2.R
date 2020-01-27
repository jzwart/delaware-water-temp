library(XML)
library(dplyr)
library(lubridate)
library(tidyverse)

##### Retrieve DEOS data from all DEOS stations within DRB #####

# Define function to retrieve data
retrieve_DEOS <- function(input_dataframe, start_yr, end_yr){
  # get a sequence of dates
  all_days <- seq(as.Date(paste0(start_yr,"-01-01")), as.Date(paste0(end_yr,"-12-31")), by="days")
  # create empty data frame for data from all stations
  raw_DEOS <- data.frame()
  # set base url
  base_url <- 'http://deos.udel.edu/odd-divas/station_daily.php?network=DEOS&'
  # iterate through rows to....
  for (row in 1:nrow(input_dataframe)){
    # create empty data frame for data from each station
    raW_DEOS_station <- data.frame()
    # Set station extension
    this_station <- input_dataframe[row, "Station"]
    station_extension <- sprintf('station=%s&', this_station)
    # set latitude and longitude
    this_latitude <- input_dataframe[row, "Lat_dd"]
    this_longitude <- input_dataframe[row, "Lon_dd"]
    # Then, iterate through all days in year to...
    for (i in 1:length(all_days)) {
      # build final url
      date_extension <- sprintf('month=%s&day=%s&year=%s', lubridate::month(all_days[i]), lubridate::day(all_days[i]), lubridate::year(all_days[i]))
      final_url <- paste0(base_url, station_extension, date_extension)
      # export message so you know what's going on
      message('Retrieving hourly data for station ', this_station, ": ", all_days[i])
      # get that day's data
      dat_list <- XML::readHTMLTable(final_url, stringsAsFactors = FALSE, na.strings = c('NA', 'N/A'))
      dat <- dat_list[[2]][c(1,3,6,17,19)] %>%
        mutate(Date = all_days[i]) %>%
        mutate(Station = this_station, Latitude = this_latitude, Longitude = this_longitude, Data_source = "DEOS")
      raw_DEOS <- rbind(dat, raw_DEOS)
      raW_DEOS_station <- rbind(dat, raW_DEOS_station)
    }
    # export hourly data for each station
    message('Exporting all hourly data retrieved for station ', this_station)
    names(raW_DEOS_station) <- c('Hour', 'Temp_degC', 'Relative_humidity_p', 'Solar_radiation_wattsperm2', 'Rainfall_mm', 'Date', 'Station', 'Latitude', 'Longitude', 'Data_source')
    raW_DEOS_station <- raW_DEOS_station %>%
      arrange(Date)
    if (start_yr == end_yr){
      station_output_filename <- paste0("data-raw/DEOS/DEOS_hourly/", this_station, "_", start_yr, "_hourly.csv")
    } else {
      station_output_filename <- paste0("data-raw/DEOS/DEOS_hourly/", this_station, "_", start_yr, "-", end_yr, "_hourly.csv")
    }
    write.csv(raW_DEOS_station, station_output_filename, row.names = FALSE)
  }
  browser()
  # Export all hourly retrieved data for all stations
  message('Exporting all hourly data retrieved for all stations')
  names(raw_DEOS) <- c('Hour', 'Temp_degC', 'Relative_humidity_p', 'Solar_radiation_wattsperm2', 'Rainfall_mm', 'Date', 'Station', 'Latitude', 'Longitude', 'Data_source')
  raw_DEOS <- raw_DEOS %>%
    arrange(Date)
  if (start_yr == end_yr){
    output_filename <- paste0('data-raw/DEOS/DEOS_weather_data_hourly_raw_', start_yr, '.csv')
  } else {
    output_filename <- paste0('data-raw/DEOS/DEOS_weather_data_hourly_raw_', start_yr, "-", end_yr, '.csv')
  }
  write.csv(raw_DEOS, output_filename, row.names = FALSE)
  # return data frame with all retrieved data
  return(raw_DEOS)
}

### Call function
# Load .csv of DEOS stations within DRB
DEOS_stations <- read.csv('data-raw/DEOS/DEOS_DRB.csv', header = TRUE)
# Set parameters
first_yr <- 2015
last_yr <- 2015
# Call function
retrieve_DEOS(DEOS_stations, first_yr, last_yr)


##### Function to format data #####
format_DEOS <- function(file_list, DEOS_station_info, start_yr, end_yr){
  # Iterate through raw hourly data files to...
  for (file in file_list) {
    # Open file
    DEOS_hourly_tmp <- read.csv(file, na.strings = c("NA", "N/A"), stringsAsFactors = FALSE)
    for (row in 1:nrow(DEOS_station_info)){
      # If station name matches file name
      if (grepl(DEOS_station_info[row, "Station"], file, fixed=TRUE)){
        # set station name
        station_name <- DEOS_station_info[row, "Station"]
      } else {
        next
      }
    }
    # Group data into daily data
    message(paste0('Formatting all hourly data retrieved for ', station_name, ' as daily data'))
    DEOS_daily_tmp <- DEOS_hourly_tmp %>%
      group_by(Date, Station, Latitude, Longitude, Data_source) %>%
      summarize(solar_radiation_mean = mean(as.numeric(Solar_radiation_wattsperm2)),
                solar_radiation_max = max(as.numeric(Solar_radiation_wattsperm2)),
                solar_radiation_sum = sum(as.numeric(Solar_radiation_wattsperm2)),
                temp_mean = mean(as.numeric(Temp_degC)),
                rel_humidity_mean = mean(as.numeric(Relative_humidity_p)),
                rainfall_sum = sum(as.numeric(Rainfall_mm)),)
    # Export daily data
    message(paste0('Exporting all formatted daily data retrieved for ', station_name))
    if (start_yr == end_yr){
      daily_output_filename <- paste0('data-raw/DEOS/DEOS_daily/DEOS_daily_', station_name, "_",start_yr, '.csv')
    } else {
      daily_output_filename <- paste0('data-raw/DEOS/DEOS_daily/DEOS_daily_', station_name, "_",start_yr, "-", end_yr, '.csv')
    }
    write.csv(DEOS_daily_tmp, daily_output_filename, row.names = FALSE)
  }
  #Bind all daily DEOS data into one dataframe and export to csv
  message('Compiling all daily data retrieved for all stations into a single dataframe')
  DEOS_daily_files <- list.files("data-raw/DEOS/DEOS_daily/", pattern="*.csv", full.names = TRUE)
  DEOS_daily_all <- map_df(DEOS_daily_files, ~read_csv(.), .id = NULL)
  # Export all daily DOES data as .csv
  if (start_yr == end_yr){
    DEOS_daily_all_filename <- paste0('data-raw/Formatted_daily_data/DEOS_daily_', start_yr, '.csv')
  } else {
    DEOS_daily_all_filename <- paste0('data-raw/Formatted_daily_data/DEOS_daily_', start_yr, "-", end_yr, '.csv')
  }
  message('Exporting all formatted daily data for all stations as a single .csv file')
  write.csv(DEOS_daily_all, DEOS_daily_all_filename, row.names = FALSE)
}


# call function
DEOS_raw_files <- list.files(path="data-raw/DEOS/DEOS_hourly", pattern = "*.csv", full.names = TRUE)
DEOS_stations <- read.csv('data-raw/DEOS/DEOS_DRB.csv', header = TRUE)
first_yr <- 2015
last_yr <- 2015
format_DEOS(file_list = DEOS_raw_files, DEOS_station_info = DEOS_stations, first_yr, last_yr)


