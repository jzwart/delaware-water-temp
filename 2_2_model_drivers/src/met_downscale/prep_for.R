prep_for <- function(NOAA.data, input_tz, local_tzone, weather_uncertainty){
  # --------------------------------------
  # purpose: convert forecasts dataframe to units/names for comparison with observations
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  NOAA.na.value = 999900000000000000000
  forecast.data <- NOAA.data %>%
    dplyr::mutate(timestamp = as.character(forecast.date)) %>%
    dplyr::mutate(timestamp = as_datetime(timestamp,
                                         tz = input_tz)) %>%
    plyr::rename(c("ensembles" = "NOAA.member")) %>%
    dplyr::mutate(AirTemp = tmp2m,
                  WindSpeed = sqrt(vgrd10m^2 + ugrd10m^2),
                  LongWave = ifelse(dlwrfsfc==NOAA.na.value, NA, dlwrfsfc),
                  ShortWave = ifelse(dswrfsfc==NOAA.na.value, NA, dswrfsfc),
                  RelHum = rh2m,
                  Rain = pratesfc*60*60*24/1000) %>% # (convert from mm/s to total m over 6 hrs)
    select(NOAA.member, timestamp, AirTemp, LongWave, ShortWave, RelHum, WindSpeed, Rain)
  forecast.data$timestamp <- with_tz(forecast.data$timestamp, local_tzone)
  
  if(weather_uncertainty == FALSE){
    forecast.data <- forecast.data %>% 
      group_by(timestamp) %>% 
      mutate(AirTemp = mean(AirTemp), 
             LongWave = mean(LongWave), 
             ShortWave = mean(ShortWave), 
             RelHum = mean(RelHum), 
             WindSpeed = mean(WindSpeed), 
             Rain = mean(Rain)) %>% 
     ungroup(timestamp)
  }
  return(forecast.data)
}