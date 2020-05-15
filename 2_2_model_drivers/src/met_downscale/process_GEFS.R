# --------------------------------------
# purpose: process GEFS forecasts and save as input for lake downscaling
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: processes GEFS forecasts for three scenarios:
# (1) downscaled without noise addition
# (2) downscaled with noise addition
# (3) not downscaled ("out of box")
# Then, the output for each ensemble member is saved as a .csv file
# The function returns: (1) a list of the names of the .csv files and (2) a datframe of the processed output for all ensembles 
# --------------------------------------

process_GEFS <- function(file_name,
                         n_ds_members,
                         n_met_members,
                         in_directory,
                         out_directory,
                         local_tzone,
                         VarInfo,
                         replaceObsNames,
                         hrly.observations,
                         DOWNSCALE_MET,
                         FIT_PARAMETERS,
                         met_downscale_uncertainty,
                         WRITE_FILES,
                         downscaling_coeff,
                         full_time_local,
                         weather_uncertainty,
                         obs_met_glm){
  # -----------------------------------
  # 1. read in and reformat forecast data
  # -----------------------------------
  f <- paste0(in_directory,'/',file_name,'.csv')
  if(!file.exists(f)){
    print('Missing forecast file!')
    print(f)
    stop()
  }
  
  d <- read_csv(paste0(in_directory,'/',file_name,'.csv')) 
  
  for.input_tz <- "GMT"

  
  #for.input_tz = "GMT"
  
  #full_time_local <- seq(begin_step, end_step, by = "1 hour", tz = local_tzone) # grid
  
  forecasts <- prep_for(d, 
                        input_tz = for.input_tz, 
                        local_tzone, 
                        weather_uncertainty)
  
  time0 <- min(forecasts$timestamp)
  time_end <- max(forecasts$timestamp)
  
  # -----------------------------------
  # 2. process forecast according to desired method
  # -----------------------------------
  
  if(DOWNSCALE_MET == TRUE){
    ## Downscaling option
    print("Downscaling option")
    if(is.na(downscaling_coeff)){
      load(file <- paste(out_directory,
                         "/debiased.coefficients.RData", 
                         sep = ""))
    }else{
      load(file <- downscaling_coeff)
    }
    output <- downscale_met(forecasts,
                            debiased.coefficients,
                            VarInfo,
                            PLOT = FALSE,
                            local_tzone = local_tzone,
                            debiased.covar,
                            n_ds_members,
                            n_met_members,
                            met_downscale_uncertainty)
    output <- output %>% 
      mutate(AirTemp = AirTemp - 273.15) # from Kelvin to Celsius 
    
  }else{
    ## "out of box" option
    print("out of box option")
    out.of.box <- out_of_box(forecasts, VarInfo$VarNames) %>%
      dplyr::mutate(AirTemp = AirTemp - 273.15,
                    RelHum = ifelse(RelHum <0, 0, RelHum),
                    RelHum = ifelse(RelHum > 100, 100, RelHum),
                    ShortWave = ifelse(ShortWave < 0, 0, ShortWave)) %>%
      arrange(timestamp)
    output <- out.of.box %>%
      dplyr::mutate(dscale.member = 1)
  }
  
  hrly.observations <- hrly.observations %>%
    mutate(AirTemp = AirTemp - 273.15)
  
  obs.time0 <- hrly.observations %>% filter(timestamp == time0)
  
  VarNamesStates = VarInfo %>%
    filter(VarType == "State")
  VarNamesStates = VarNamesStates$VarNames
  
  # replace the first measurements of the downscaled output with observations 
  #so that the model has a smooth transition from past observations to future 
  # forecast if missing observation then it skips this step
  for(i in 1:length(VarNamesStates)){
    if(nrow(obs.time0[VarNamesStates[i]]) == 1){
      output[which(output$timestamp == time0),
             VarNamesStates[i]] = obs.time0[VarNamesStates[i]]
    }
  }
  
  output.time0.6.hrs <- output %>% 
    filter(timestamp == time0 | timestamp == time0 + 6*60*60)
  states.output0.6.hrs <- spline_to_hourly(output.time0.6.hrs,VarNamesStates)
  output <- output %>% 
    full_join(states.output0.6.hrs, 
              by = c("NOAA.member","dscale.member","timestamp"), 
              suffix = c("",".splined")) %>%
    mutate(AirTemp = ifelse(is.na(AirTemp.splined), AirTemp, AirTemp.splined),
           WindSpeed = ifelse(is.na(WindSpeed.splined), WindSpeed, WindSpeed.splined),
           RelHum = ifelse(is.na(RelHum.splined), RelHum, RelHum.splined)) %>%
    select(-AirTemp.splined, WindSpeed.splined, RelHum.splined) %>% 
    filter(timestamp < time_end) %>% 
    mutate(timestamp = with_tz(timestamp, local_tzone))
  
  #output$timestamp <- with_tz(output$timestamp, local_tzone)
  
  #------------------------------------
  #  Merge with historical observations
  #------------------------------------
  
  output <- output %>% 
    mutate(forecasted = 1) %>% 
    mutate(Snow = 0) %>% 
    rename(time = timestamp) %>% 
    select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain, Snow, NOAA.member, dscale.member, forecasted)
  
  obs_met_glm <- obs_met_glm %>% 
    mutate(forecasted = 0,
           NOAA.member = 0,
           dscale.member = 0) %>% 
    filter(time < first(output$time)) %>% 
    select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain, Snow, NOAA.member, dscale.member, forecasted)
  
  combined_output <- rbind(obs_met_glm, output)
  
  #combined_output %>% 
  #  filter(dscale.member == 1 | dscale.member == 0) %>% 
  #  select(time, NOAA.member, AirTemp) %>% 
  #  ggplot(aes(x = time, y = AirTemp, color = factor(NOAA.member))) +  
  #  geom_line()
  
  
  # -----------------------------------
  # 3. Produce output files
  # -----------------------------------
  met_file_list = NULL
  if(WRITE_FILES){
    print("Write Output Files")
    
    write_file <- function(df){
      # formats GLM_climate, writes it as a .csv file, and returns the filename
      GLM_climate[,"time"] <- strftime(GLM_climate$time, format="%Y-%m-%d %H:%M", tz = attributes(GLM_climate$time)$tzone)
      colnames(GLM_climate) <-  noquote(c("time", 
                                          "ShortWave",
                                          "LongWave", 
                                          "AirTemp", 
                                          "RelHum", 
                                          "WindSpeed", 
                                          "Rain", 
                                          "Snow"))
      
      current_filename <- paste0('met_NOAA',NOAA.ens,'_ds',dscale.ens,'.csv')
      write_csv(GLM_climate,path = paste0(out_directory, "/", current_filename), quote_escape = "none")
      return(current_filename)
    }
    
    for (NOAA.ens in 1:21) {
      for (dscale.ens in 1:n_ds_members) {
        if (met_downscale_uncertainty == FALSE) { # downscale met with noise addition
          dscale.ens = 1
        }
        GLM_climate <- combined_output %>%
          filter((NOAA.member == NOAA.ens & dscale.member == dscale.ens) | 
                   (NOAA.member == 0 & dscale.member == 0)) %>%
          arrange(time) %>%
          select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain, Snow)
        current_filename <- write_file(GLM_climate)
        met_file_list <- append(met_file_list, current_filename)
      }
    }
    
  }
  
  return(list(met_file_list, output))
}