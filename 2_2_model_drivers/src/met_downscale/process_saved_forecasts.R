# -----------------------------------
# Information
# -----------------------------------
# Purpose: Combine saved daily NOAA forecasts into dataframes
# Creator: Laura Puckett, December 14 2018
# Contact: plaura1@vt.edu
# -----------------------------------
# Description
# -----------------------------------
# Take the first day of each 16-day forecast and combine into single flux (longwave, shortwave, precipitation) and state (temperature, wind speed, relative humidity) dataframes
# -----------------------------------
# Input
# -----------------------------------
# @param data.path: path to SCCData-noaa-data folder, which contains 16-day NOAA forecasts (.csv) saved on many days
# -----------------------------------
process_saved_forecasts <- function(data.path,working_directory, local_tzone){
  
  # -----------------------------------
  # 0. Load data, initialize variables
  # -----------------------------------
  forecast.files.list = list.files(data.path, "*00z.csv")
  flux.forecasts = NULL
  state.forecasts = NULL
  for(i in 1:length(forecast.files.list)){
    tmp.data = read.csv(paste0(data.path, "/", forecast.files.list[i]))
    if(as_datetime(tmp.data$forecast.date[1]) < as_datetime('2018-12-07 00:00:00')){
      input_tz = "EST5EDT"
    }else{input_tz = "GMT"}
    #input_tz = "GMT"
    tmp.data <- tmp.data %>%
      dplyr::mutate(forecast.date = as_datetime(forecast.date, tz = input_tz))
    tmp.data$forecast.date = with_tz(tmp.data$forecast.date, local_tzone)
    tmp.min.time = min(tmp.data$forecast.date)
    tmp.state <- tmp.data %>%
      filter(forecast.date <= tmp.min.time + 18*60*60) %>%
      select(ensembles, tmp2m, rh2m, vgrd10m, ugrd10m, forecast.date)
    tmp.flux <- tmp.data %>%
      filter(forecast.date <= tmp.min.time + 24*60*60 & forecast.date > tmp.min.time) %>%
      select(ensembles, forecast.date,pratesfc, dlwrfsfc, dswrfsfc)
    flux.forecasts = rbind(flux.forecasts, tmp.flux)
    state.forecasts = rbind(state.forecasts, tmp.state)
  }
  saveRDS(flux.forecasts, file = paste(working_directory,"/NOAA.flux.forecasts", sep = ""))
  saveRDS(state.forecasts, file = paste(working_directory,"/NOAA.state.forecasts", sep = ""))
}

