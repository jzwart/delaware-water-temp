

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
                            model_fabric_file,
                            obs_file,
                            init_param_file,
                            model_run_loc,
                            orig_model_loc,
                            subbasin_file,
                            subbasin_outlet_file,
                            gd_config = 'lib/cfg/gd_config.yml'){


  ############## for debugging purposes ###########
  source('4_model/src/EnKF_functions.R')
  source('4_model/src/get_sntemp_values.R')
  source('4_model/src/run_sntemp.R')
  source('4_model/src/update_sntemp.R')
  source('4_model/src/set_sntemp_output.R')
  source('4_model_calibrate/src/calibrate_sntemp.R')
  source('4_model_calibrate/src/get_subbasins.R')
  source('4_model/src/get_upstream_downstream_segs.R')
  source('4_model_calibrate/src/get_calibration_order.R')
  start = '2014-05-01'
  stop = '2014-07-10'
  model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
  obs_file = '3_observations/in/obs_temp_full.rds'
  init_param_file = '2_3_model_parameters/out/calibration_params_init.rds'
  model_run_loc = I('4_model_calibrate/tmp')
  orig_model_loc = I('20191002_Delaware_streamtemp')
  subbasin_file = '4_model_calibrate/out/drb_subbasins.rds'
  subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml'
  library(tidyverse)
  library(igraph)
  library(hydroPSO)
  library(hydroGOF)
 #######################################################################


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

  # setting params to initial conditions before calibrating
  init_params = init_params_df %>%
    pivot_longer(cols = eval(param_names), names_to = 'param_name', values_to = 'param_value') %>%
    arrange(factor(param_name, levels = param_names), as.numeric(model_idx)) %>%
    pull(param_value)

  update_sntemp_params(param_names = param_names,
                       updated_params = init_params,
                       model_run_loc = model_run_loc)

  # run sntemp once with spinup to create a starting point for the model
  run_sntemp(start = (start-1),
             stop = (start-1),
             spinup = T,
             restart = F,
             var_save_file = 'prms_ic.txt',
             model_run_loc = model_run_loc)

  # need to calibrate for upstream segments of DRB before moving downstream
  #  need to pull obs in calibrated segment and only update parameters in calibrated segment

  for(cur_subbasin_outlet in cal_order$subbasin_outlet){

    # for debugging:
    cur_subbasin_outlet = '4182'
    #############

    cur_subbasin = subbasins[cur_subbasin_outlet][[cur_subbasin_outlet]]

    # get subbasin parameter locations
    cur_model_idxs = as.character(cur_subbasin$model_idx)

    # observations for current subbasin
    cur_obs = dplyr::filter(obs_df, model_idx %in% cur_model_idxs,
                            date >= as.Date(start),
                            date <= as.Date(stop))

    # current parameters (after calibrating subbasin if further along than first subbasin)
    cur_params = get_sntemp_params(param_names = param_names,
                                   model_run_loc = model_run_loc)

    # pull out parameters for current subbasin
    cur_params = cur_params %>% mutate(calibrate = ifelse(model_idx %in% cur_model_idxs, T, F))

    cur_params_to_cal = dplyr::filter(cur_params, calibrate == T) %>%
      pivot_longer(cols = eval(param_names), names_to = 'param_name', values_to = 'param_value') %>%
      # arranging in order of param_names and then model_idx (which is the way the update_sntemp_params function works since it was made for DA work)
      arrange(factor(param_name, levels = param_names), as.numeric(model_idx)) %>%
      pull(param_value)

    # supply subsetted_segs parameters as initial params to calibrate
    sprintf('Starting calibration of %s', cur_subbasin_outlet)
    # cur_cal_out = optim(fn = cal_sntemp_run,
    #                     par = as.numeric(cur_params_to_cal),# method = 'L-BFGS-B', lower = 0, upper = 100,
    #                     start = start,
    #                     stop = stop,
    #                     spinup = F,
    #                     restart = T,
    #                     var_init_file = 'prms_ic.txt',
    #                     var_save_file = 'ic_out_dont_use.txt',
    #                     model_run_loc = model_run_loc,
    #                     obs = cur_obs,
    #                     model_idxs_to_cal = cur_model_idxs,
    #                     all_params = cur_params,
    #                     param_names = param_names)

    all_params = cur_params # currently debugging PSO

    cur_cal_out = hydroPSO(par = as.numeric(cur_params_to_cal),
                           fn = 'hydromodInR',
                           lower = rep(0, length(cur_params_to_cal)),
                           upper = rep(200, length(cur_params_to_cal)),
                           control = list(),
                           model.FUN = cal_sntemp_run,
                           model.FUN.args = list(param.values = as.numeric(cur_params_to_cal),
                                                 start = start,
                                                 stop = stop,
                                                 spinup = F,
                                                 restart = T,
                                                 var_init_file = 'prms_ic.txt',
                                                 var_save_file = 'ic_out_dont_use.txt',
                                                 model_run_loc = model_run_loc,
                                                 obs = cur_obs,
                                                 model_idxs_to_cal = cur_model_idxs,
                                                 all_params = all_params,
                                                 param_names = param_names),
    )

    #########files needed for running hydroPSO
    param_file_name = 'delaware.control.param'

    param_files = tibble(ParameterNmbr = c(1,1),
                        ParameterName = c('gw_tau','gw_tau'),
                        Filename = c(param_file_name, param_file_name),
                        Row.Number = c(60274, 60275),
                        Col.Start = c(1,1),
                        Col.End = c(1,1),
                        DecimalPlaces = c(2,2))

    write.table(param_files, file = '4_model_calibrate/in/ParamFiles.txt')

    param_ranges = tibble(ParameterNmbr = c(1),
                          ParameterName = c('gw_tau'),
                          MinValue = c(0),
                          MaxValue = c(200))

    write.table(param_ranges, file = '4_model_calibrate/in/ParamRanges.txt')


    cur_cal_out = hydroPSO(par = as.numeric(cur_params_to_cal),
                           fn = 'hydromodInR',
                           lower = rep(0, length(cur_params_to_cal)),
                           upper = rep(200, length(cur_params_to_cal)),
                           control = list(),
                           model.FUN = cal_sntemp_run,
                           model.FUN.args = list(param.values = as.numeric(cur_params_to_cal),
                                                 out.FUN.args = list(param.values = as.numeric(cur_params_to_cal),
                                                                     model_idxs_to_cal = model_idxs_to_cal)),
    )

    # update param file with calibrated params
    updated_params = combine_cal_uncal_params(cal_params = cur_cal_out$par,
                                              all_params = cur_init_params,
                                              param_names = param_names)

    update_sntemp_params(param_names = param_names,
                         updated_params = updated_params,
                         model_run_loc = model_run_loc)


    # optionally run SNTemp with calibrated params to see how well we're doing
    run_sntemp(start = start,
               stop = stop,
               spinup = F,
               restart = T,
               var_init_file = 'prms_ic.txt',
               var_save_file = 'ic_out_dont_use.txt',
               model_run_loc = model_run_loc)

    preds = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                   model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

    compare = left_join(preds, select(cur_obs, model_idx, date, temp_C),
                        by = c('model_idx', 'date'))

    rmse(compare$temp_C, compare$water_temp, na.rm = T)

  }

}


combine_cal_uncal_params = function(cal_params, all_params, param_names){
  updated_cal_params = all_params %>%
    pivot_longer(cols = eval(param_names), names_to = 'param_name', values_to = 'param_value') %>%
    arrange(factor(param_name, levels = param_names), as.numeric(model_idx)) %>%
    dplyr::filter(calibrate) %>%
    mutate(param_value = as.numeric(cal_params))

  out = all_params %>%
    pivot_longer(cols = eval(param_names), names_to = 'param_name', values_to = 'param_value') %>%
    arrange(factor(param_name, levels = param_names), as.numeric(model_idx)) %>%
    left_join(updated_cal_params, by = c('seg_id_nat','model_idx','calibrate', 'param_name'), suffix = c('','_cal')) %>%
    mutate(param_value = ifelse(calibrate, as.character(param_value_cal), param_value)) %>%
    pull(param_value)

  return(out)
}



cal_sntemp_run = function(param.values,
                          start,
                          stop,
                          model_run_loc,
                          spinup = F,
                          restart = F,
                          var_init_file,
                          var_save_file,
                          obs,
                          model_idxs_to_cal,
                          all_params,
                          param_names){
  # debugging
  print(param.values)
  print(model_idxs_to_cal)
   #

  updated_params = combine_cal_uncal_params(cal_params = pararm.values,
                           all_params = all_params,
                           param_names = param_names)

  update_sntemp_params(param_names = param_names,
                       updated_params = updated_params,
                       model_run_loc = model_run_loc)

  run_sntemp(start = start,
             stop = stop,
             spinup = spinup,
             restart = restart,
             model_run_loc = model_run_loc,
             var_init_file = var_init_file,
             var_save_file = var_save_file)

  preds = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                         model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

  compare = left_join(preds, select(obs, model_idx, date, temp_C),
                      by = c('model_idx', 'date'))

  # plot(compare$water_temp ~ compare$temp_C,
  #      ylim = c(0, max(compare$temp_C, na.rm = T)), ylab = 'pred', xlab = 'obs')

  return(nll(compare$temp_C, compare$water_temp)) #optimize on NLL for water temp
  # return(rmse(compare$temp_C, compare$water_temp, na.rm = T)) #optimize on water temp RMSE
}

# returns negative log likelihood
nll = function(obs, pred){
  if (any(is.na(obs))){
    NA_obs <- which(is.na(obs))
    res <- obs[-NA_obs] - pred[-NA_obs]
  } else{
    res <- obs - pred
  }

  nRes <- length(res)
  SSE <- sum(res^2)
  sigma2 <- SSE/nRes
  NLL <- 0.5*((SSE/sigma2) + nRes*log(2*pi*sigma2))

  return(NLL)
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


# function for retrieving parameters from param file
get_sntemp_params = function(param_names,
                             model_run_loc,
                             model_fabric_file = 'GIS/Segments_subset.shp',
                             param_file = 'delaware.control.param',
                             n_segments = 456){

  params = readLines(file.path(model_run_loc, 'control', param_file))

  model_fabric = sf::read_sf(file.path(model_run_loc, model_fabric_file))

  # order by model_idx
  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  out = seg_ids

  if(length(param_names) == 0){
    out = out
  }else{
    for(i in 1:length(param_names)){
      param_loc_start = grep(param_names[i], params) + 5
      param_loc_end = param_loc_start + n_segments - 1

      cur_param_vals = params[param_loc_start:param_loc_end]

      out = out %>%
        mutate(temp_name = cur_param_vals) %>%
        rename(!!noquote(param_names[i]) := temp_name)
    }
  }

  return(out)
}


