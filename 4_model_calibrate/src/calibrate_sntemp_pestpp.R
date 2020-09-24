# using original PRMS executable for running since it is faster and we don't need to update states

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
                            calibration_settings,
                            calibrate_flow, #T/F
                            calibrate_temp, #T/F
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
  source('4_model_calibrate/src/write_pestpp_tpl_files.R')
  source('4_model_calibrate/src/write_pestpp_ins_files.R')
  source('4_model_calibrate/src/write_pestpp_pst_files.R')
  source('2_3_model_parameters/src/add_default_sntemp_params.R')
  source('2_1_model_fabric/src/get_segment_hrus.R')
  library(tidyverse)
  library(igraph)
  start = '1980-10-01'
  stop = '2005-09-30'
  model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
  obs_file = '3_observations/out/obs_temp_flow.rds'
  init_param_file = '2_3_model_parameters/out/calibration_params_init.rds'
  model_run_loc = I('4_model_calibrate/tmp')
  orig_model_loc = I('20191002_Delaware_streamtemp')
  pestpp_exe_loc = I('pestpp/bin/win')
  subbasin_file = '4_model_calibrate/out/drb_subbasins.rds'
  subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml'
  param_groups = as_tibble(yaml::read_yaml('4_model_calibrate/cfg/calibration_settings.yml')$param_groups)
  calibrate_flow = T
  calibrate_temp = F
 #######################################################################


  # copy over original run files to temporary file location
  dir.create(model_run_loc, showWarnings = F)
  print('Copying original model files to model working directory...')
  files_to_transfer = list.files(orig_model_loc)
  file.copy(from = file.path(orig_model_loc, files_to_transfer), to = model_run_loc, overwrite = T, recursive = T)
  # copy over pest++ executables
  # files_to_transfer = list.files(pestpp_exe_loc)
  # file.copy(from = file.path(pestpp_exe_loc, files_to_transfer), to = model_run_loc, overwrite = T, recursive = T)

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
  if(calibrate_temp){
    temp_obs_df = readRDS(obs_file)$temp %>%
      left_join(model_locations, by = 'seg_id_nat')
  }
  if(calibrate_flow){
    flow_obs_df = readRDS(obs_file)$flow %>%
      left_join(model_locations, by = 'seg_id_nat') %>%
      mutate(discharge_cfs = discharge_cms * (3.28084^3))
  }

  # get initial parameters
  init_params_list = readRDS(init_param_file) #%>% arrange(as.numeric(model_idx))
  # n_params_est = (ncol(init_params_df) - 2) * nrow(init_params_df) # columns 1 & 2 are model locations

  param_names = names(init_params_list)

  # read in the subbasins and calibration order
  subbasins = readRDS(subbasin_file)

  cal_order = get_calibration_order(subbasin_outlet_file = subbasin_outlet_file)

  update_sntemp_params(param_names = param_names,
                       updated_params = init_params_list,
                       model_run_loc = model_run_loc,
                       param_file = 'input/myparam.param')

  # run sntemp once with spinup to create a starting point for the model
  run_sntemp(start = (start-1),
             stop = (start-1),
             spinup = T,
             restart = F, #spinup_days = 2,
             var_save_file = 'prms_ic.out',
             model_run_loc = model_run_loc)

  # need to calibrate for upstream segments of DRB before moving downstream
  #  need to pull obs in calibrated segment and only update parameters in calibrated segment


  # for debugging:
  cur_subbasin_outlet = '4182'
  #############

  sprintf('Starting calibration of %s', cur_subbasin_outlet)

  cur_subbasin = subbasins[cur_subbasin_outlet][[cur_subbasin_outlet]]

  # get subbasin parameter locations
  cur_model_idxs = as.character(cur_subbasin$model_idx)

  # Order by model_idx and then date
  # observations for current subbasin
  if(calibrate_temp){
    cur_temp_obs = dplyr::filter(temp_obs_df, model_idx %in% cur_model_idxs,
                                 date >= as.Date(start),
                                 date <= as.Date(stop)) %>%
      arrange(as.numeric(model_idx), date)
  }
  if(calibrate_flow){
    cur_flow_obs = dplyr::filter(flow_obs_df, model_idx %in% cur_model_idxs,
                                 date >= as.Date(start),
                                 date <= as.Date(stop)) %>%
      arrange(as.numeric(model_idx), date)
  }

  # pull out parameters for current subbasin based on segment model_idx
  cur_params_to_cal = get_params_by_segment(param_names = param_names,
                                            model_run_loc = model_run_loc,
                                            seg_model_idxs = cur_model_idxs)

  # write template files needed for running PEST++ sen
  write_pestpp_tpl_files(params = cur_params_to_cal,
                         model_run_loc = model_run_loc,
                         param_file_name = 'input/myparam.param',
                         param_file_out = sprintf('pestpp/subbasin_%s.tpl', cur_subbasin_outlet),
                         delim = '%')

  # run sntemp once to produce output for .ins template
  run_sntemp(start = start,
             stop = stop,
             spinup = F,
             restart = T,
             var_init_file = 'prms_ic.out',
             var_save_file = 'ic_out_dont_use.out',
             model_run_loc = model_run_loc)

  # write instruction files needed for running PEST++
  # for temperature
  if(calibrate_temp){
    write_pestpp_ins_files(params = cur_params_to_cal,
                           seg_model_idxs = cur_model_idxs,
                           model_run_loc = model_run_loc,
                           model_output_file = 'output/seg_tave_water.csv',
                           file_out = sprintf('pestpp/subbasin_%s_temp.ins', cur_subbasin_outlet),
                           delim = '@',
                           secondary_delim = '!',
                           obs_type = 'wtemp')
  }

  # for flow
  if(calibrate_flow){
    write_pestpp_ins_files(params = cur_params_to_cal,
                           seg_model_idxs = cur_model_idxs,
                           model_run_loc = model_run_loc,
                           model_output_file = 'output/seg_outflow.csv',
                           file_out = sprintf('pestpp/subbasin_%s_flow.ins', cur_subbasin_outlet),
                           delim = '@',
                           secondary_delim = '!',
                           obs_type = 'flow')
  }

  ## to check ins file run pyEMU with
  #  i = pyemu.pst_utils.InstructionFIle("my.ins")
  #  df = i.read_output_file("my.output")

  if(calibrate_flow & calibrate_temp){
    cur_obs_list = list(temp = cur_temp_obs, flow = cur_flow_obs)
  }else if(calibrate_flow & !calibrate_temp){
    cur_obs_list = list(flow = cur_flow_obs)
  }else if(calibrate_temp & !calibrate_flow){
    cur_obs_list = list(temp = cur_temp_obs)
  }

  # write PEST++ sen control file
  write_pestpp_pst_files(params = cur_params_to_cal,
                         seg_model_idxs = cur_model_idxs,
                         model_run_loc = model_run_loc,
                         temp_model_output_file = 'output/seg_tave_water.csv',
                         flow_model_output_file = 'output/seg_outflow.csv',
                         obs = cur_obs_list,
                         file_out = sprintf('pestpp/subbasin_%s.pst', cur_subbasin_outlet),
                         param_groups = param_groups,
                         param_file_name = 'input/myparam.param',
                         tpl_file_name = sprintf('pestpp/subbasin_%s.tpl', cur_subbasin_outlet),
                         temp_ins_file_name = sprintf('pestpp/subbasin_%s_temp.ins', cur_subbasin_outlet),
                         flow_ins_file_name = sprintf('pestpp/subbasin_%s_flow.ins', cur_subbasin_outlet),
                         calibrate_flow = calibrate_flow,
                         calibrate_temp = calibrate_temp,
                         weight_by_magnitude = T,# assigning weight based on magnitude of obs value
                         tie_by_group = F) # tying parameters together by group (e.g. gw_tau)

  set_sntemp_start_stop(start = start,
                        stop = stop,
                        model_run_loc = model_run_loc,
                        control_file = 'delaware.control')

  current.wd = getwd() # getting current project root wd to reset after running pest++

  setwd(file.path(current.wd, model_run_loc)) # set wd to where model run location is
  # to run pest++ in serial, pest++ pest_ctl_file.pst
  shell(sprintf('pestpp-sen %s', sprintf('pestpp/subbasin_%s.pst', cur_subbasin_outlet))) # run pest++; this is running pestpp 4.3.17

  setwd(current.wd) # set wd back to root of project

}


