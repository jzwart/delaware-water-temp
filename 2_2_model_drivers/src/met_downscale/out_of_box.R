out_of_box <- function(forecasts,VarNames){
  forecasts.hrly <- forecasts %>%
    dplyr::mutate(Snow = 0) %>%
    select(NOAA.member, timestamp, Rain, Snow, ShortWave, LongWave) %>% 
    repeat_6hr_to_hrly() %>%
    full_join(forecasts %>% select(-Rain, -ShortWave, -LongWave), by = c("timestamp","NOAA.member")) %>%
    dplyr::arrange(NOAA.member, timestamp) %>%
    dplyr::group_by(NOAA.member) %>%
    dplyr::mutate(AirTemp = na.interpolation(AirTemp, option = "linear"),
           RelHum = na.interpolation(RelHum, option = "linear"),
           WindSpeed = na.interpolation(WindSpeed, option = "linear")) %>%
    ungroup() %>%
    select(timestamp, NOAA.member, VarNames, Rain, Snow)
  
  return(forecasts.hrly)
  
}

