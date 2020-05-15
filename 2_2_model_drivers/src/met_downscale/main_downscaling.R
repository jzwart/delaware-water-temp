# --------------------------------------
# purpose: run downscaling processes
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: this calls all other functions required to downscale future GEFS forecasts to a specific site using the relationship between saved forecasts and site observations
# --------------------------------------

# -----------------------------------
# 0. Source necessary files
# -----------------------------------
rm(list = ls())
path.working <- "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/"
setwd(path.working)
# used for all scenarios
library(tidyr)
source('process_GEFS.R')
source('repeat_6hr_to_hrly.R')
source("prep_for.R")
# only used if FIT_PARAMETERS is TRUE
source('process_saved_forecasts.R') 
source('get_daily_debias_coeff.R')
source('fit_downscaling_parameters.R')
source('prep_obs.R')
# only used if DOWNSCALE_MET is TRUE
source('downscale_met.R') 
source('daily_debias_from_coeff.R')
source('spline_to_hourly.R')
source('solar_geom.R')
source('ShortWave_to_hrly.R')
source('aggregate_to_daily.R')
source('daily_to_6hr.R')
# only used if DOWNSCALE_MET is TRUE and ADD_NOISE is TRUE
source('add_noise.R')
# only used if DOWNSCALE_MET is FALSE
source('out_of_box.R')

source('compare_output_to_obs.R')
source('check_CI.R')
source('aggregate_obs_to_hrly.R')

# library(imputeTS) # for out-of-box
# library(stringr) # for out-of-box
# library(lubridate)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)

# -----------------------------------
# 1. Setup
# -----------------------------------

in_directory = "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/SCCData-noaa-data/"
out_directory = "/Users/laurapuckett/Documents/Research/Fall 2018/my_files/met_output_files"
start_date = "2018-11-18"
VarNames = c("AirTemp",
             "WindSpeed",
             "RelHum",
             "ShortWave",
             "LongWave")
VarNamesStates = c("AirTemp",
                   "WindSpeed",
                   "RelHum")
FIT_PARAMETERS = TRUE
DOWNSCALE_MET = TRUE
ADD_NOISE = TRUE

if(FIT_PARAMETERS){
  replaceObsNames = c("AirTC_Avg" = "AirTemp",
                      "WS_ms_Avg" = "WindSpeed",
                      "RH" = "RelHum",
                      "SR01Up_Avg" = "ShortWave",
                      "IR01UpCo_Avg" = "LongWave")
}

output_tz = "US/Eastern" 
nmembers = 15 # members for downscaled ensembles

# -----------------------------------
# 1. Fit Parameters
# -----------------------------------

if(FIT_PARAMETERS){
  fit_downscaling_parameters(obs.file.path = paste(path.working, "FCRmet.csv", sep = ""),
                             for.file.path = paste(path.working, "SCCData-noaa-data/", sep = ""),
                             VarNames,
                             VarNamesStates,
                             USE_ENSEMBLE_MEAN = FALSE,
                             PLOT = TRUE)
}

# -----------------------------------
# 2. Process GEFS
# -----------------------------------
files = process_GEFS(start_date, DOWNSCALE_MET, FIT_PARAMETERS, ADD_NOISE, WRITE_FILES = TRUE)[[1]]

# -----------------------------------
# 3. Plot processed GEFS forecasts
# -----------------------------------
output = process_GEFS(start_date = start_date,
                      DOWNSCALE_MET = DOWNSCALE_MET,
                      FIT_PARAMETERS = FIT_PARAMETERS,
                      ADD_NOISE = ADD_NOISE,
                      WRITE_FILES = FALSE)[[2]]
if("dscale.member" %in% colnames(output) == FALSE){
  output = output %>%
    dplyr::mutate(dscale.member = 0)
}
  ggplot(data = output, aes(x = timestamp)) +
    geom_line(aes(y = AirTemp, group = interaction(NOAA.member, dscale.member)))
  ggplot(data = output, aes(x = timestamp)) +
    geom_line(aes(y = WindSpeed, group = interaction(NOAA.member, dscale.member)))
  ggplot(data = output, aes(x = timestamp)) +
    geom_line(aes(y = RelHum, group = interaction(NOAA.member, dscale.member)))
  ggplot(data = output, aes(x = timestamp)) +
    geom_line(aes(y = ShortWave, group = interaction(NOAA.member, dscale.member)))
  ggplot(data = output, aes(x = timestamp)) +
    geom_line(aes(y = LongWave, group = interaction(NOAA.member, dscale.member)))
  
# -----------------------------------
# 4. Evaulate Downscaling Process
# -----------------------------------

summary.table = compare_output_to_obs(output = output,
                        obs.file.path = paste(path.working, "FCRmet.csv", sep = ""),
                        PLOT = TRUE)
print(summary.table)

  
