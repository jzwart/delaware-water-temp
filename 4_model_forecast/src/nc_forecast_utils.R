

nc_create_forecast_out = function(seg_model_idxs,
                                  forecast_horizon,
                                  n_en,
                                  issue_dates,
                                  forecast_project_id,
                                  vars,
                                  nc_name_out,
                                  overwrite = T){

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

  if(file.exists(nc_name_out)){
    if(overwrite){
      file.remove(nc_name_out)
      ncout <- nc_create(nc_name_out, def_list, force_v4 = T)
    }else{stop('cannot overwrite nc output file')}
  }else{ncout <- nc_create(nc_name_out, def_list, force_v4 = T)}

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
    reshape2::melt(varnames = c('model_idx', 'valid_time', 'ensemble', 'issue_time')) %>%
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
    dplyr::select(-filter_valid_time)

  nc_close(nc)

  return(out)
}



nc_create_cal_params = function(n_en,
                                forecast_project_id,
                                vars, # parameter names
                                nc_name_out,
                                model_run_loc,
                                param_default_file,
                                n_segments = 456,
                                n_hrus = 765,
                                n_gwr = 765,
                                n_ssr = 765,
                                overwrite = T){

  #Set dimensions
  ens <- as.integer(seq(1, n_en, 1))
  seg_model_idxs <- as.integer(seq(1, n_segments, 1))
  hru_model_idxs <- as.integer(seq(1, n_hrus, 1))
  gwr_model_idxs <- as.integer(seq(1, n_gwr, 1))
  ssr_model_idxs <- as.integer(seq(1, n_ssr, 1))

  ens_dim <- ncdim_def("ens",
                       units = "",
                       vals = ens,
                       longname = 'ensemble member')
  seg_dim <- ncdim_def("seg_loc",
                       units = "",
                       vals = seg_model_idxs,
                       longname = 'stream segment model index')
  hru_dim <- ncdim_def("hru_loc",
                       units = "",
                       vals = hru_model_idxs,
                       longname = 'HRU model index')
  gwr_dim <- ncdim_def("gwr_loc",
                       units = "",
                       vals = gwr_model_idxs,
                       longname = 'groundwater reservoir model index')
  ssr_dim <- ncdim_def("ssr_loc",
                       units = "",
                       vals = ssr_model_idxs,
                       longname = 'shallow subsurface reservoir model index')
  one_dim <- ncdim_def('global_loc',
                       units = '',
                       vals = 1,
                       longname = 'global parameter; applied to every segment or hru')
  month_dim <- ncdim_def('month_param',
                         units = '',
                         vals = seq(1,12,1),
                         longname = 'parameter applied either globally by segment / hru for a given month')

  #Define variables
  fillvalue_float <- 1e32
  fillvalue_int <- -99

  # use same dimensions as NOAA forecasts [lon, lat, forecast hours, ensemble, issue date]
  def_list <- list()
  # loop through variables that we're forecasting
  n_vars = length(vars$param)
  for(i in 1:n_vars){
    cur_defaults = get_default_param_vals(param_name = vars$param[i],
                                          model_run_loc = model_run_loc,
                                          param_default_file = param_default_file)
    cur_time_dim = NULL

    if(cur_defaults$dim == 'one'){
      cur_loc_dim = one_dim
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nsegment'){ # a stream segment-based parameter
      cur_loc_dim = seg_dim
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nhru'){
      cur_loc_dim = hru_dim
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'ngw'){
      cur_loc_dim = gwr_dim
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nssr'){
      cur_loc_dim = ssr_dim
    }else if(cur_defaults$dim == 'nmonths'){
      cur_loc_dim = one_dim
      cur_time_dim = month_dim
    }else if(cur_defaults$ndim == '2'){
      if(grepl('nsegment', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
        cur_loc_dim = seg_dim
        cur_time_dim = month_dim
      }else if(grepl('nhru', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
        cur_loc_dim = hru_dim
        cur_time_dim = month_dim
      }
    }
    # print(cur_loc_dim$name)

    if(!is.null(cur_time_dim)){
      def_list[[i]] <- ncvar_def(name =  vars$param[i],
                                 units = vars$units[i],
                                 dim = list(cur_loc_dim, ens_dim, cur_time_dim),
                                 missval = ifelse(vars$prec[i] == 'float', fillvalue_float, fillvalue_int),
                                 longname = vars$longname[i],
                                 prec = vars$prec[i])
    }else{
      def_list[[i]] <- ncvar_def(name =  vars$param[i],
                                 units = vars$units[i],
                                 dim = list(cur_loc_dim, ens_dim),
                                 missval = ifelse(vars$prec[i] == 'float', fillvalue_float, fillvalue_int),
                                 longname = vars$longname[i],
                                 prec = vars$prec[i])
    }

  }

  if(file.exists(nc_name_out)){
    if(overwrite){
      file.remove(nc_name_out)
      ncout <- nc_create(nc_name_out, def_list, force_v4 = T)
    }else{stop('cannot overwrite nc output file')}
  }else{ncout <- nc_create(nc_name_out, def_list, force_v4 = T)}

  #Global file metadata
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "forecast_project_id",
            attval = as.character(forecast_project_id),
            prec =  "text")

  nc_close(ncout)
}



#' insert calibrated parameters
#'
nc_cal_params_put = function(var_list,
                             n_en,
                             nc_name_out){

  ncout <- nc_open(nc_name_out, write = T)

  param_names = names(var_list[[1]])

  for(i in seq_along(param_names)){

    cur_var = ncout$var[[param_names[i]]]
    varsize = cur_var$varsize
    # temp output dims [loc_dim, fdays_dim, ens_dim, time_dim]; position of dimensions following:
    loc_pos = 1
    ens_pos = 2
    time_pos = 3

    n_dims = cur_var$ndims

    for(n in seq_len(n_en)){
      cur_en = n

      start = rep(1, n_dims)
      # start[time_pos] = # need to update if there are month based parameters
      start[ens_pos] = cur_en

      count = varsize
      # count[time_pos] = 1 # adding only one issue time step
      count[ens_pos] = 1 # adding output from only one ensemble

      ncvar_put(nc = ncout,
                varid = param_names[i],
                vals = var_list[[n]][[param_names[i]]],
                start = start,
                count = count)
    }
  }
  nc_close(ncout)
}

#' function for returning calibrated parameters for forecasting; returns list
#'
nc_cal_params_get = function(nc_file,
                             param_names = NULL,
                             model_idxs = NULL,
                             ens = NULL,
                             months = NULL){

  nc = nc_open(nc_file)

  if(!is.null(param_names)){
    param_names = param_names
  }else{
    param_names = names(nc$var)
  }

  # temp output dims [loc_dim, fdays_dim, ens_dim, time_dim]; position of dimensions following:
  loc_pos = 1
  ens_pos = 2
  time_pos = 3

  out_list = vector(mode = 'list', length = length(param_names))
  for(i in seq_along(param_names)){
    cur_var = nc$var[[param_names[i]]]
    varsize = cur_var$varsize
    n_dims = cur_var$ndims

    all_model_idxs = as.character(cur_var$dim[[loc_pos]]$vals)
    all_ens = as.integer(cur_var$dim[[ens_pos]]$vals)
    if(n_dims > 2){
      all_months = cur_var$dim[[time_pos]]$vals
      # return all values, and then filter
      all_out = ncvar_get(nc = nc, varid = param_names[i]) %>% array(dim = varsize) %>%
        reshape2::melt(varnames = c('model_idx', 'ensemble', 'month')) %>%
        mutate(model_idx = all_model_idxs[model_idx],
               ensemble = all_ens[ensemble],
               month = all_months[month]) %>%
        rename(!!param_names[i] := value) %>%
        as_tibble()
    }else{
      # return all values, and then filter
      all_out = ncvar_get(nc = nc, varid = param_names[i]) %>% array(dim = varsize) %>%
        reshape2::melt(varnames = c('model_idx', 'ensemble')) %>%
        mutate(model_idx = all_model_idxs[model_idx],
               ensemble = all_ens[ensemble]) %>%
        rename(!!param_names[i] := value) %>%
        as_tibble()
    }

    if(!is.null(model_idxs)){
      cur_model_idxs = as.character(model_idxs)
    }else{cur_model_idxs = as.character(all_model_idxs)}
    if(!is.null(ens)){
      cur_ens = as.integer(ens)
    }else{cur_ens = as.integer(all_ens)}
    if(n_dims > 2){
      if(!is.null(months)){
        cur_months = as.integer(months)
      }else{cur_months = as.integer(all_months)}
    }

    if(n_dims > 2){
      out = dplyr::filter(all_out,
                          model_idx %in% cur_model_idxs,
                          ensemble %in% cur_ens,
                          month %in% cur_months)
    }else{
      out = dplyr::filter(all_out,
                          model_idx %in% cur_model_idxs,
                          ensemble %in% cur_ens)
    }

    out_list[[i]] = out[,ncol(out)] %>% pull()
    names(out_list)[i] = param_names[i]
  }

  nc_close(nc)

  return(out_list)
}


# example function call:
# nc_create_drivers(hru_model_idxs = seq(1,765),
#                   forecast_horizon = 8,
#                   n_en = 20,
#                   issue_dates = dates,
#                   forecast_project_id = forecast_project_id,
#                   vars = drivers,
#                   nc_name_out = '2_2_model_drivers/out/forecasted_drivers.nc',
#                   overwrite = T)


nc_create_drivers = function(hru_model_idxs,
                             forecast_horizon,
                             n_en,
                             issue_dates,
                             forecast_project_id,
                             vars,
                             nc_name_out,
                             overwrite = T){

  #Set dimensions
  ens <- as.integer(seq(1, n_en, 1))
  model_locations <- as.integer(hru_model_idxs)
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
                       longname = 'hydrologic reach unit model index')
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
  n_vars = length(vars$driver)
  for(i in 1:n_vars){
    def_list[[i]] <- ncvar_def(name =  vars$driver[i],
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

  if(file.exists(nc_name_out)){
    if(overwrite){
      file.remove(nc_name_out)
      ncout <- nc_create(nc_name_out, def_list, force_v4 = T)
    }else{stop('cannot overwrite nc output file')}
  }else{ncout <- nc_create(nc_name_out, def_list, force_v4 = T)}

  ncvar_put(nc = ncout,
            varid = def_list[[n_vars + 1]],
            vals = issue_dates)

  #Global file metadata
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "forecast_project_id",
            attval = as.character(forecast_project_id),
            prec =  "text")

  nc_close(ncout)
}


nc_drivers_put = function(var_df,
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
            vals = pull(var_df, var_name),
            start = start,
            count = count)

  nc_close(ncout)
}


#' function for returning forecasted driver variables; returns tibble
#'
nc_drivers_get = function(nc_file,
                          var_name = c('tmin','tmax','prcp'), # default return all drivers
                          issue_dates = NULL,
                          model_idxs = NULL, # hru model idx
                          ens = NULL,
                          fdays = NULL){

  nc = nc_open(nc_file)

  # temp output dims [loc_dim, fdays_dim, ens_dim, time_dim]; position of dimensions following:
  loc_pos = 1
  fdays_pos = 2
  ens_pos = 3
  time_pos = 4

  cur_var = nc$var[[1]]

  all_issue_dates = as.Date(ncvar_get(nc, varid = 'issue_time')) # all possible issue dates
  all_model_idxs = as.character(cur_var$dim[[loc_pos]]$vals)
  all_ens = as.integer(cur_var$dim[[ens_pos]]$vals)
  all_valid_times = cur_var$dim[[fdays_pos]]$vals

  nc_close(nc)

  if(!is.null(issue_dates)){
    cur_issue_dates = which(all_issue_dates %in% as.Date(issue_dates)) - 1 # indexed starting with 0
  }else{
    cur_issue_dates = seq(0, length(all_issue_dates), 1)
  }
  if(!is.null(model_idxs)){
    cur_model_idxs = which(all_model_idxs %in% as.character(model_idxs))
  }else{
    cur_model_idxs = seq_along(all_model_idxs)
  }
  if(!is.null(ens)){
    cur_ens = which(all_ens %in% as.integer(ens))
  }else{
    cur_ens = seq_along(all_ens)
  }
  if(!is.null(fdays)){
    cur_fdays = which(all_valid_times %in% as.integer(fdays)) - 1 # indexed starting with 0
  }else{
    cur_fdays = seq(0, length(all_valid_times), 1)
  }

  # return all values, and then filter
  out = tidync::tidync(x = nc_file) %>%
    tidync::activate(var_name) %>%
    tidync::hyper_filter(loc = loc %in% cur_model_idxs,
                 forecast_days = forecast_days %in% cur_fdays,
                 ens = ens %in% cur_ens,
                 timestep = timestep %in% cur_issue_dates) %>%
    tidync::hyper_tibble() %>%
    mutate(issue_time = all_issue_dates[timestep + 1],
           model_idx = all_model_idxs[loc],
           valid_time = issue_time + as.difftime(all_valid_times[forecast_days + 1], units = 'days'),
           ensemble = all_ens[ens]) %>%
    dplyr::select(model_idx, valid_time, ensemble, issue_time, var_name)

  return(out)
}

