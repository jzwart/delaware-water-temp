# Workflow for applying EFI forecasting standards to ensemble forecast temperature output

library(EML)
library(tidyverse)
library(uuid)
library(ncdf4)

forecast_issue_time <- as.Date("2001-03-04")
Forecast_id <- uuid::UUIDgenerate() #ID that applies to the specific forecast
ForecastProject_id <- 30405043 #Some ID that applies to a set of forecasts



#' Function to organize model output arrays into netcdf
#' @param ind_file
#' @param output_array_file
#' @param n_en
#' @param n_steps
#' @param n_states_est
#' @param n_params_est
#'
array_to_ncdf = function(ind_file,
                         output_array_file,
                         n_en,
                         n_steps,
                         n_states_est,
                         n_params_est){

  output_array = readRDS(output_array_file)


  ncfname <- "logistic-forecast-ensemble-multi-variable-space-long.nc"

  time <- as.Date(as.character(2000 + 1:NT), format = "%Y")

  data_assimilation <- rep(0, length(time))

  #Set dimensions
  ens <- as.integer(seq(1,n_ensembles,1))
  depths <- as.integer(c(1,2,3))
  timestep <- as.integer(seq(1, NT, 1))



}









