library(XML)
library(dplyr)
library(lubridate)

##### Retrieve DEOS data from all DEOS stations within DRB #####
# Load .csv of DEOS stations within DRB
DEOS_stations <- read.csv('data-raw/DEOS/DEOS_DRB.csv', header = TRUE)

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
      message('Retrieving data for station ', this_station, ": ", all_days[i])
      # get that day's data
      dat_list <- XML::readHTMLTable(final_url, stringsAsFactors = FALSE)
      dat <- dat_list[[2]][c(1,3,6,17,19)] %>%
        mutate(Date = all_days[i]) %>%
        mutate(Station = this_station, Latitude = this_latitude, Longitude = this_longitude)
      raw_DEOS <- rbind(dat, raw_DEOS)
      raW_DEOS_station <- rbind(dat, raW_DEOS_station)
    }
    # export data for each station
    message('Exporting all data retrieved for station ', this_station)
    if (start_yr == end_yr){
      station_output_filename <- paste0("data-raw/DEOS/", this_station, "_", start_yr, "_daily.csv")
    } else {
      station_output_filename <- paste0("data-raw/DEOS/", this_station, "_", start_yr, "-", end_yr, "_daily.csv")
    }
    write.csv(raW_DEOS_station, station_output_filename, row.names = FALSE)
  }
  # Export all retrieved data for all stations
  message('Exporting all data retrieved for all stations')
  names(raw_DEOS) <- c('Hour', 'Temp_degC', 'Relative_humidity_p', 'Solar_radiation_wattsperm2', 'Rainfall_mm', 'Date', 'Station', 'Latitude', 'Longitude')
  if (start_yr == end_yr){
    output_filename <- paste0('data-raw/DEOS/DEOS_weather_data_hourly_raw_', start_yr, '.csv')
  } else {
    output_filename <- paste0('data-raw/DEOS/DEOS_weather_data_hourly_raw_', start_yr, "-", end_yr, '.csv')
  }
  write.csv(raw_DEOS, output_filename, row.names = FALSE)
  # return data frame with all retrieved data
  return(raw_DEOS)
}

# Call function
first_yr <- 2015
last_yr <- 2015
DEOS_DRB <- retrieve_DEOS(DEOS_stations, first_yr, last_yr)

##### Function to format data #####
format_data <- function(raw_DEOS, start_yr, end_yr){
  # create temporary dataframe of hourly data retrieved from DEOS
  hourly_tmp <- raw_DEOS
  # Process data to get in daily timestep
  dat_daily <- hourly_tmp %>%
    group_by(Station, Date, Latitude, Longitude) %>%
    summarize(solar_radiation_mean = mean(as.numeric(Solar_radiation_wattsperm2)),
              solar_radiation_max = max(as.numeric(Solar_radiation_wattsperm2)),
              solar_radiation_sum = sum(as.numeric(Solar_radiation_wattsperm2)),
              temp_mean = mean(as.numeric(Temp_degC)),
              rel_humidity_mean = mean(as.numeric(`Relative_humidity_p`)),
              rainfall_sum = sum(as.numeric(Rainfall_mm)),)
  # Export daily data
  if (start_yr == end_yr){
    daily_output_filename <- paste0('data-raw/data_daily/DEOS_weather_data_daily_', start_yr, '.csv')
  } else {
    daily_output_filename <- paste0('data-raw/data_daily/DEOS_weather_data_daily_', start_yr, "-", end_yr, '.csv')
  }
  write.csv(dat_daily, daily_output_filename, row.names = FALSE)
  # Return formatted daily data
  return(dat_daily)
}

# call function
DEOS_daily <- format_data(DEOS_DRB, first_yr, last_yr)




