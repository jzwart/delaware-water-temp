
# load necessary libraries
library(dplyr)
library(tidyverse)

##### Read and Format SNTemp data #####

format_SNTemp <- function(file_list, DEOS_station_info, yr) {
  # Iterate through raw files to...
  for (file in file_list) {
    SNTemp_tmp <- readRDS(file)
    # Add, select,and rename columns AND filter data for year of interest
    SNTemp_tmp <- SNTemp_tmp %>%
      mutate(., Date = date, rel_humidity_mean = seg_humid*100) %>%
      select(Date, rel_humidity_mean, seg_tave_air, seginc_swrad) %>%
      rename(temp_mean = seg_tave_air, solar_radiation_mean = seginc_swrad) %>%
      filter(Date %like% yr)
    # Iterate through list of DEOS stations to...
    for (row in 1:nrow(DEOS_station_info)){
      # If station name matches file name...
      if (grepl(DEOS_station_info[row, "Station"], file, fixed=TRUE)){
        # assign the station name, latitude, and longitude to the extracted SNTemp data
        SNTemp_tmp <- SNTemp_tmp %>%
          mutate(.,Station = DEOS_station_info[row, "Station"], Latitude = DEOS_station_info[row, "Lat_dd"], Longitude = DEOS_station_info[row, "Lon_dd"], Data_source = "SNTemp")
        # set file name for daily data
        daily_output_name <- paste0('data-raw/PRMS_SNTemp/SNTemp_daily/SNTemp_daily_', DEOS_station_info[row, "Station"], "_", yr, '.csv')
        station_name <- DEOS_station_info[row, "Station"]
      } else {
        next
      }
    }
    # Write csv of daily SNTemp data for each station
    message(paste0('Exporting all formatted daily SNTemp data extracted for the location of DEOS station ', station_name))
    write.csv(SNTemp_tmp, daily_output_name, row.names = FALSE)
  }
  #Merge all daily SNTemp data into one dataframe
  message('Compiling all daily SNTemp data extracted for the locations of all DEOS stations into a single dataframe')
  SNTemp_daily_files <- list.files("data-raw/PRMS_SNTemp/SNTemp_daily/", pattern="*.csv", full.names = TRUE)
  SNTemp_daily_all <- map_df(SNTemp_daily_files, ~read_csv(.), .id = NULL)
  # Export all daily SNTemp data as .csv
  message('Exporting all formatted daily SNTemp data extracted for the locations of all DEOS stations as a single .csv file')
  SNTemp_daily_all_filename <- paste0('data-raw/Formatted_daily_data/SNTemp_daily_', yr, '.csv')
  write.csv(SNTemp_daily_all, SNTemp_daily_all_filename, row.names = FALSE)
}


##### Call function #####
# Get parameters
SNTemp_daily_files <- list.files("data-raw/PRMS_SNTemp/SNTemp_raw", pattern="*.rds", full.names = TRUE)
DEOS_stations <- read.csv('data-raw/DEOS/DEOS_DRB.csv', header = TRUE)
data_yr <- 2015
# Call function
format_SNTemp(file_list = SNTemp_daily_files, DEOS_station_info = DEOS_stations, yr = data_yr)

# check by reading in raw file
#SNTemp_DHOC <- readRDS('data-raw/PRMS_SNTemp/SNTemp_raw/SNTemp_sw_rad_station_location_DHOC_39.79_-75.7.rds')

