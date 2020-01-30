

#' wrapper function for calibrating SNTemp
#'
#' @param start start date of model run
#' @param stop date of model run
#' @param obs_file observation file
#' @param driver_file driver data file
#' @param n_states_est number of states we're estimating
#' @param n_params_est number of parameters we're estimating
#' @param n_params_obs number of parameters for which we have observations
#' @param obs_cv coefficient of variation of observations
#' @param param_cv coefficient of variation of parameters
#' @param driver_cv coefficient of variation of driver data
#' @param init_cond_cv initial condition CV (what we're )
#' @param gd_config google drive configuration
calibrate_sntemp = function(ind_file,
                            start,
                            stop,
                            model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp',
                            obs_file = '3_observations/in/obs_temp_full.rds',
                            init_param_file = '2_3_model_parameters/out/init_params.rds',
                            model_run_loc = '4_model/tmp',
                            orig_model_loc = '20191002_Delaware_streamtemp',
                            subbasin_file = '4_model_calibrate/out/drb_subbasins.rds',
                            gd_config = 'lib/cfg/gd_config.yml'){

  # copy over original run files to temporary file location
  dir.create(model_run_loc, showWarnings = F)
  print('Copying original model files to model working directory...')
  files_to_transfer = list.files(orig_model_loc)
  file.copy(from = file.path(orig_model_loc, files_to_transfer), to = model_run_loc, overwrite = T, recursive = T)

  # use this to organize the parameters being calibrated
  model_fabric = sf::read_sf(model_fabric_file)

  model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                           model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  # get model start, stop, full dates, and n_steps
  start = as.Date(as.character(start))
  stop = as.Date(as.character(stop))
  dates = get_model_dates(model_start = start, model_stop = stop, time_step = 'days')

  # get observation matrix
  obs_df = readRDS(obs_file)

  # get initial parameters
  init_params_df = readRDS(init_param_file) %>% arrange(as.numeric(model_idx))
  n_params_est = (ncol(init_params_df) - 2) * nrow(init_params_df) # columns 1 & 2 are model locations

  param_names = colnames(init_params_df)[3:ncol(init_params_df)]


  subbasins = readRDS(subbasin_file)

  cal_order = get_calibration_order(subbasin_outlet_file = subbasin_outlet_file)

  # need to calibrate for upstream segments of DRB before moving downstream
  #  need to pull obs in calibrated segment and only update parameters in calibrated segment

  for(cur_subbasin_outlet in cal_order$subbasin_outlet){

    cur_subbasin = subbasins[cur_subbasin_outlet][[cur_subbasin_outlet]]

    # get subbasin parameter locations

    # supply subsetted_segs parameters as initial params to calibrate
    optim()

    update_sntemp_params(param_names = param_names,
                         updated_params = updated_params)

    run_sntemp(start = start,
               stop = stop,
               spinup = F,
               restart = F)




  }




}
