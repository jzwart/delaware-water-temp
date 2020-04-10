

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
                            subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml',
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

  # get observations
  obs_df = readRDS(obs_file) %>%
    left_join(model_locations, by = 'seg_id_nat')

  # get initial parameters
  init_params_df = readRDS(init_param_file) %>% arrange(as.numeric(model_idx))
  # n_params_est = (ncol(init_params_df) - 2) * nrow(init_params_df) # columns 1 & 2 are model locations

  param_names = colnames(init_params_df)[3:ncol(init_params_df)]

  # read in the subbasins and calibration order
  subbasins = readRDS(subbasin_file)

  cal_order = get_calibration_order(subbasin_outlet_file = subbasin_outlet_file)

  # need to calibrate for upstream segments of DRB before moving downstream
  #  need to pull obs in calibrated segment and only update parameters in calibrated segment

  for(cur_subbasin_outlet in cal_order$subbasin_outlet){

    cur_subbasin = subbasins[cur_subbasin_outlet][[cur_subbasin_outlet]]

    # get subbasin parameter locations
    cur_model_idxs = as.character(cur_subbasin$model_idx)




    # supply subsetted_segs parameters as initial params to calibrate
    optim()



  }

}




cal_sntemp_run = function(start,
                          stop,
                          model_run_loc,
                          spinup = F,
                          restart = F,
                          obs,
                          model_idxs_to_cal,
                          obs,
                          params,
                          param_names){

  cur_obs = dplyr::filter(obs, model_idx %in% model_idxs_to_cal,
                          date >= as.Date(start),
                          date <= as.Date(stop))



  update_sntemp_params(param_names = param_names,
                       updated_params = params,
                       model_run_loc = model_run_loc)

  run_sntemp(start = start,
             stop = stop,
             spinup = spinup,
             restart = restart,
             model_run_loc = model_run_loc)

  preds = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                         model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

  compare = left_join(preds, select(cur_obs, model_idx, date, temp_C),
                      by = c('model_idx', 'date'))

  # plot(compare$water_temp ~ compare$temp_C,
  #      ylim = c(0, max(compare$temp_C, na.rm = T)), ylab = 'pred', xlab = 'obs')

  return(rmse(compare$temp_C, compare$water_temp, na.rm = T)) #optimize on water temp RMSE
}


rmse = function (actual, predicted, na.rm = T)
{
  return(sqrt(mse(actual, predicted, na.rm)))
}

mse = function (actual, predicted, na.rm = T)
{
  return(mean(se(actual, predicted), na.rm = na.rm))
}

se = function (actual, predicted)
{
  return((actual - predicted)^2)
}

