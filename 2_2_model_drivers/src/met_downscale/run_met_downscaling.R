## script to call all meterological downsclaing processes
SCC_folder_location = "/Users/laurapuckett/Desktop/SCC_forecasting/"

library(tidyverse)
library(lubridate)
library(imputeTS)
library(mvtnorm)

VarInfo <- data.frame("VarNames" = c("AirTemp", # Names of variables to be downscaled
                                       "WindSpeed",
                                       "RelHum",
                                       "ShortWave",
                                       "LongWave",
                                       "Rain"),
                        "VarType" = c("State", # Whether each variable is a "State" or "Flux"
                                      "State",
                                      "State",
                                      "Flux",
                                      "Flux",
                                      "Flux"),
                        "ds_res" = c("hour", # the reoslution to downscale to. "hour" or "6hr"
                                     "hour",
                                     "hour",
                                     "hour",
                                     "6hr",
                                     "6hr"),
                        "debias_method" = c("lm", # the method for debiasing observations. "lm" for linear model or "compare_totals". Other models could eventually be substituted in
                                            "lm",
                                            "lm",
                                            "lm",
                                            "lm",
                                            "compare_totals"),
                        "use_covariance" = c(TRUE, # Whether or not to include each parameter in the covariance matrix
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE),
                        stringsAsFactors = FALSE)
  
replaceObsNames <- c("AirTC_Avg" = "AirTemp", # old column name (left) ob observations and new column names (right)
                       "WS_ms_Avg" = "WindSpeed",
                       "RH" = "RelHum",
                       "SR01Up_Avg" = "ShortWave",
                       "IR01UpCo_Avg" = "LongWave",
                       "Rain_mm_Tot" = "Rain")
  
source(paste0(SCC_folder_location, "/FLARE/Rscripts/met_downscale/process_downscale_GEFS.R"))


output = process_downscale_GEFS(folder = paste0(SCC_folder_location, "FLARE/"),
                       noaa_location = paste0(SCC_folder_location, "SCC_data/noaa-data/"),
                       input_met_file = met_obs_fname_wdir[2],
                       working_directory = paste0(SCC_folder_location, "FLARE/working_directory/"),
                       n_ds_members = 5,
                       n_met_members = 21,
                       file_name =  "20190101gep_all_00z",
                       output_tz = "US/Eastern",
                       FIT_PARAMETERS = TRUE,
                       DOWNSCALE_MET = TRUE,
                       met_downscale_uncertainty = TRUE,
                       compare_output_to_obs = TRUE,
                       VarInfo,
                       replaceObsNames,
                       downscaling_coeff = NA,
                       full_time_local = NA,
                       first_obs_date = as.Date("2018-04-06"),
                       last_obs_date = as.Date("2019-01-01"))
  ## access output data example (there is one output file for each ensemble)
  read.csv(output[1])
  
  