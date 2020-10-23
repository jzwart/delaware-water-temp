

nc_create_forecast_out = function(seg_model_idxs,
                                  forecast_horizon,
                                  n_en,
                                  issue_dates,
                                  forecast_project_id,
                                  vars,
                                  nc_name_out){

  #Set dimensions
  ens <- as.integer(seq(1, n_en, 1))
  model_locations <- as.integer(seg_model_idxs)
  n_issue_date <- length(issue_dates)
  timestep <- as.integer(seq(0, n_issue_date - 1, 1)) # days since issue date #1
  forecast_days <- as.integer(seq(0, forecast_horizon - 1, 1))

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
                        longname = sprintf('Days since %s', issue_dates[1]),
                        vals = timestep)
  fdays_dim <- ncdim_def('forecast_days',
                         units = '1 day',
                         longname = 'Valid dates from issue time',
                         vals = forecast_days)

  dim_nchar <- ncdim_def("nchar",
                         units = "",
                         vals = 1:nchar(as.character(issue_dates[1])),
                         create_dimvar = FALSE)
  ## quick check that units are valid
  udunits2::ud.is.parseable(ens_dim$units)
  udunits2::ud.is.parseable(loc_dim$units)
  udunits2::ud.is.parseable(time_dim$units)
  udunits2::ud.is.parseable(fdays_dim$units)
  udunits2::ud.is.parseable(dim_nchar$units)

  #Define variables
  fillvalue <- 1e32

  # use same dimensions as NOAA forecasts [lon, lat, forecast hours, ensemble, issue date]
  def_list <- list()
  # loop through variables that we're forecasting
  n_vars = length(vars$state)
  for(i in 1:n_vars){
    def_list[[i]] <- ncvar_def(name =  vars$state[i],
                               units = vars$units[i],
                               dim = list(loc_dim, fdays_dim, ens_dim, time_dim),
                               missval = fillvalue,
                               longname = vars$longname[i],
                               prec = vars$prec[i])
  }

  def_list[[n_vars + 1]] <- ncvar_def(name = 'issue_time',
                                      units = 'datetime',
                                      dim = list(dim_nchar, time_dim),
                                      longname = 'Forecast issue time',
                                      prec = 'char')

  def_list[[n_vars + 2]] <- ncvar_def(name =  'data_assimilation',
                                      units = 'logical',
                                      dim = list(time_dim),
                                      missval = fillvalue,
                                      longname = 'EFI standard data assimilation code. 0 = no data',
                                      prec = 'single')

  ncout <- nc_create(nc_name_out, def_list, force_v4 = T)

  ncvar_put(nc = ncout,
            varid = def_list[[n_vars + 1]],
            vals = issue_dates)

  #Global file metadata
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "forecast_project_id",
            attval = as.character(forecast_project_id),
            prec =  "text")
  # ncatt_put(nc = ncout,
  #           varid = 0,
  #           attname = "forecast_id",
  #           attval = as.character(forecast_id),
  #           prec =  "text")
  # ncatt_put(nc = ncout,
  #           varid = 0,
  #           attname = "forecast_issue_time",
  #           attval = as.character(forecast_issue_time$forecast_issue_time),
  #           prec =  "text")
  nc_close(ncout)
}


#' insert forecast variables
#'
nc_forecast_put = function(var_df,
                           var_name,
                           en,
                           issue_date,
                           nc_name_out){

  ncout <- nc_open(nc_name_out, write = T)

  cur_var = ncout$var[[var_name]]
  varsize = cur_var$varsize
  issue_dates = ncvar_get(ncout, varid = 'issue_time')
  # temp output dims [loc_dim, fdays_dim, ens_dim, time_dim]; position of dimensions following:
  loc_pos = 1
  fdays_pos = 2
  ens_pos = 3
  time_pos = 4

  n_dims = cur_var$ndims

  cur_issue_time = which(issue_date == issue_dates)
  cur_en = en

  n_fdays = varsize[fdays_pos]
  n_en = varsize[ens_pos]

  start = rep(1, n_dims)
  start[time_pos] = cur_issue_time
  start[ens_pos] = cur_en

  count = varsize
  count[time_pos] = 1 # adding only one issue time step
  count[ens_pos] = 1 # adding output from only one ensemble

  ncvar_put(nc = ncout,
            varid = var_name,
            vals = var_df$water_temp,
            start = start,
            count = count)

  nc_close(ncout)
}


#' function for returning forecasted variables; returns tibble
#'
nc_forecast_get = function(nc_file,
                           var_name,
                           issue_dates = NULL,
                           model_idxs = NULL,
                           ens = NULL,
                           fdays = NULL){

  nc = nc_open(nc_file)

  # temp output dims [loc_dim, fdays_dim, ens_dim, time_dim]; position of dimensions following:
  loc_pos = 1
  fdays_pos = 2
  ens_pos = 3
  time_pos = 4

  cur_var = nc$var[[var_name]]
  varsize = cur_var$varsize
  all_issue_dates = as.Date(ncvar_get(nc, varid = 'issue_time')) # all possible issue dates
  all_model_idxs = as.character(cur_var$dim[[loc_pos]]$vals)
  all_ens = as.integer(cur_var$dim[[ens_pos]]$vals)
  all_valid_times = cur_var$dim[[fdays_pos]]$vals

  n_dims = cur_var$ndims

  # return all values, and then filter
  all_out = ncvar_get(nc = nc, varid = var_name) %>%
    reshape2::melt(all_out,
                     varnames = c('model_idx', 'valid_time', 'ensemble', 'issue_time')) %>%
    mutate(issue_time = all_issue_dates[issue_time],
           model_idx = all_model_idxs[model_idx],
           valid_time = issue_time + as.difftime(all_valid_times[valid_time], units = 'days'),
           ensemble = all_ens[ensemble]) %>%
    rename(!!var_name := value) %>%
    as_tibble()

  if(!is.null(issue_dates)){
    cur_issue_times = as.Date(issue_dates)
  }else{cur_issue_times = as.Date(all_issue_dates)} # return all dates if NULL
  if(!is.null(model_idxs)){
    cur_model_idxs = as.character(model_idxs)
  }else{cur_model_idxs = as.character(all_model_idxs)}
  if(!is.null(ens)){
    cur_ens = as.integer(ens)
  }else{cur_ens = as.integer(all_ens)}
  if(!is.null(fdays)){
    cur_valid_times = as.integer(fdays)
  }else{cur_valid_times = as.integer(all_valid_times)}

  out = dplyr::filter(all_out,
                      model_idx %in% cur_model_idxs,
                      ensemble %in% cur_ens,
                      issue_time %in% cur_issue_times) %>%
    mutate(filter_valid_time = as.integer(valid_time - issue_time)) %>%
    dplyr::filter(filter_valid_time %in% cur_valid_times) %>%
    select(-filter_valid_time)

  nc_close(nc)

  return(out)
}

