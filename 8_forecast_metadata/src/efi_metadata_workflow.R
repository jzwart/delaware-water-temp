# Workflow for applying EFI forecasting standards to ensemble forecast temperature output


#' Function to organize model output arrays into netcdf
#' @param ind_file
#' @param output_array_file
#' @param n_en
#' @param forecast_issue_time
#' @param forecast_id
#' @param forecast_project_id
#'
array_to_ncdf = function(ind_file,
                         output_array_file,
                         n_en,
                         forecast_issue_time,
                         forecast_id,
                         forecast_project_id,
                         gd_config = 'lib/cfg/gd_config.yml'){

  output_array <- readRDS(output_array_file)

  nc_name_out <- scipiper::as_data_file(ind_file)

  n_steps <- length(output_array$dates)

  # check if any observations were assimilated for each date, 0 for no data assimilated, 1 for data assimilated
  data_assimilation <- lapply(seq(1,n_steps), function(date){
    da <- any(!is.na(output_array$obs[,,date]))
  }) %>% unlist() %>% ifelse(., 1, 0)


  #Set dimensions
  ens <- as.integer(seq(1, n_en$n_en, 1))
  model_locations <- as.integer(output_array$model_locations$model_idx)
  timestep <- as.integer(seq(1, n_steps, 1))
  dates <- as.Date(output_array$dates)

  ens_dim <- ncdim_def("ens",
                       units = "",
                       vals = ens,
                       longname = 'ensemble member')
  loc_dim <- ncdim_def("loc",
                       units = "",
                       vals = model_locations,
                       longname = 'stream segment model index')
  time_dim <- ncdim_def("timestep",
                        units = '1 day',
                        longname = 'timestep',
                        vals = timestep)

  dim_nchar <- ncdim_def("nchar",
                        units = "",
                        vals = 1:nchar(as.character(dates[1])),
                        create_dimvar = FALSE)

  #Define variables
  fillvalue <- 1e32

  def_list <- list()
  def_list[[1]] <- ncvar_def(name = 'time',
                             units = 'datetime',
                             dim = list(dim_nchar, time_dim),
                             longname = 'time',
                             prec = 'char')

  def_list[[2]] <- ncvar_def(name =  'stream_temp',
                             units = 'degrees C',
                             dim = list(loc_dim, time_dim, ens_dim),
                             missval = fillvalue,
                             longname = 'daily mean stream segment water temperature',
                             prec = 'float')

  def_list[[3]] <- ncvar_def(name =  'data_assimilation',
                             units = 'logical',
                             dim = list(time_dim),
                             missval = fillvalue,
                             longname = '1 = data assimilation used in timestep',
                             prec = 'single')

  ncout <- nc_create(nc_name_out, def_list, force_v4 = T)

  ncvar_put(nc = ncout,
            varid = def_list[[1]],
            vals = dates)
  ncvar_put(nc = ncout,
            varid = def_list[[2]],
            vals = output_array$Y[model_locations,,])
  ncvar_put(nc = ncout,
            varid = def_list[[3]],
            vals = data_assimilation)

  #Global file metadata
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "ForecastProject_id",
            attval = as.character(forecast_project_id$forecast_project_id),
            prec =  "text")
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "Forecast_id",
            attval = as.character(forecast_id$forecast_id),
            prec =  "text")
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "forecast_issue_time",
            attval = as.character(forecast_issue_time$forecast_issue_time),
            prec =  "text")
  nc_close(ncout)

  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}


# nc = nc_open('8_forecast_metadata/out/model_out.nc')
#
# temp = ncvar_get(nc = nc, varid = 'stream_temp' )

create_forecast_eml = function(ind_file,
                               model_out_nc_file,
                               gd_config = 'lib/cfg/gd_config.yml'){

  model_out = nc_open(model_out_nc_file)

  attributes <- tibble::tribble(
    ~attributeName, ~attributeDefinition, ~unit, ~formatString, ~numberType, ~definition,
    "time",          "time",                       "year",     "YYYY-MM-DD", "numberType", NA,
    "location",         "stream segment model index",         "meter",   NA,          "real", NA,
    "ensemble",      "index of ensemble member",   "dimensionless",    NA,         "integer", NA,
    "species_1",     "Population size of species 1", "numberPerMeterSquared", NA,  "real", NA,
    "species_2",     "Population size of species 2", "numberPerMeterSquared", NA,  "real", NA,
    "forecast_issue_time",     "time that forecast was created", NA, "YYYY-MM-DD",  NA, NA,
    "data_assimilation",     "Flag whether time step included data assimilation", "dimensionless", NA, "integer", NA,
    "Forecast_id",     "ID for specific forecast cycle", NA, NA,  NA, "forecast id",
    "ForecastProject_id",     "ID for forecasting project", NA, NA,  NA, "project id"
  )
  attrList <- set_attributes(attributes,
                             col_classes = c("Date", "numeric", "numeric",
                                             "numeric","numeric", "Date",
                                             "numeric", "character", "character"))
  physical <- set_physical("logistic-forecast-ensemble-multi-variable-multi-depth.csv")

}


