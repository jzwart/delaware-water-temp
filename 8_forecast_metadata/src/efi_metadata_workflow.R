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
            attval = as.character(forecast_id),
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

  model_out <- nc_open(model_out_nc_file)

  dates <- ncvar_get(nc = model_out, varid = 'time')

  forecast_issue_time <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_issue_time')$value

  ForecastProject_id <- ncatt_get(nc = model_out, varid = 0, attname = 'ForecastProject_id')$value

  Forecast_id <- ncatt_get(nc = model_out, varid = 0, attname = 'Forecast_id')$value

  nc_close(model_out)

  attributes <- tibble::tribble(
    ~attributeName, ~attributeDefinition, ~unit, ~formatString, ~numberType, ~definition,
    "valid_date",          "forecast valid date",    "date",     "YYYY-MM-DD", "numberType", NA,
    "location",         "stream segment model index",   "dimensionless",   NA,  "integer", NA,
    "ensemble",      "index of ensemble member",   "dimensionless",    NA,    "integer", NA,
    "stream_temp",     "arithemtic mean stream temperature", "celsius", NA,  "real", NA,
    "forecast_issue_time",     "time that forecast was created", NA, "YYYY-MM-DD",  NA, NA,
    "data_assimilation",     "flag whether time step included data assimilation", "dimensionless", NA, "integer", NA,
    "forecast_id",     "ID for specific forecast cycle", NA, NA,  NA, "forecast id",
    "forecast_project_id",     "ID for forecasting project", NA, NA,  NA, "project id"
  )
  attrList <- set_attributes(attributes,
                             col_classes = c("Date", "numeric", "numeric",
                                             "numeric", "Date",
                                             "numeric", "character", "character"))
  physical <- set_physical(model_out_nc_file)

  dataTable <- eml$dataTable(
    entityName = model_out_nc_file,
    entityDescription = 'Delaware River Basin stream temperature forecasts',
    physical = physical,
    attributeList = attrList)

  me <- list(individualName = list(givenName = "Jacob",
                                   surName = "Zwart"),
             electronicMailAddress = "jzwart@usgs.gov",
             id = 'http://orcid.org/0000-0002-3870-405X')

  coverage <- EML::set_coverage(begin = as_datetime(dates[1]),
                                end = as_datetime(tail((dates)[1])),
                                geographicDescription = "Delaware River Basin",
                                west = -76.396, east = -74.358,  # could read in geo boundary and set this with function
                                north = 42.462, south = 38.684,
                                altitudeMaximum = 787, altitudeMinimum = -1.2, altitudeUnits = 'meter')

  keywordSet <- list(
    list(
      keywordThesaurus = "EFI controlled vocabulary",
      keyword = list("forecast",
                     "ecosystem",
                     "timeseries",
                     "water")
    ))


  ### methods details here ###
  methods = list(methods = 'methods go here')
  ###########

  dataset = eml$dataset(title = 'DRB stream temperature forecast',
                        creator = me,
                        contact = list(references = 'http://orcid.org/0000-0002-3870-405X'),
                        pubDate = forecast_issue_time,
                        intellectualRights = 'USGS policies',
                        abstract = 'EML metadata for stream temperature forecasts',
                        dataTable = dataTable,
                        keywordSet = keywordSet,
                        coverage = coverage,
                        methods = methods)

  eml_out <- eml$eml(dataset = dataset,
                    packageId = ForecastProject_id,
                    system = "uuid")

  ### validate the EML
  eml_validate(eml_out)

  write_eml(eml_out, scipiper::as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}


