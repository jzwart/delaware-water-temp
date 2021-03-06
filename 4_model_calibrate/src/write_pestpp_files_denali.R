


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
library(tidyverse)
library(igraph)
start = '1980-10-01'
stop = '2004-09-30'
model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
obs_file = '3_observations/in/obs_temp_full.rds'
init_param_file = '2_3_model_parameters/out/calibration_params_init.rds'
model_run_loc = I('4_model_calibrate/tmp')
orig_model_loc = I('20191002_Delaware_streamtemp')
pestpp_exe_loc = I('pestpp/bin/win')
subbasin_file = '4_model_calibrate/out/drb_subbasins.rds'
subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml'
param_ranges = as_tibble(yaml::read_yaml('4_model_calibrate/cfg/calibration_settings.yml')$param_ranges)
#######################################################################


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

# for HydroPSO, vector of observations must match vector of simulated output. Order by model_idx and then date
# observations for current subbasin
cur_obs = dplyr::filter(obs_df, model_idx %in% cur_model_idxs,
                        date >= as.Date(start),
                        date <= as.Date(stop)) %>%
  arrange(as.numeric(model_idx), date)

# obs_vec = cur_obs$temp_C # vector of temp observations for hydroPSO

# current parameters (after calibrating subbasin if further along than first subbasin)
cur_params = get_sntemp_params(param_names = param_names,
                               model_run_loc = model_run_loc,
                               param_file = 'input/myparam.param')

# pull out parameters for current subbasin
cur_params = cur_params %>% mutate(calibrate = ifelse(model_idx %in% cur_model_idxs, T, F))

cur_params_to_cal = dplyr::filter(cur_params, calibrate == T) %>%
  pivot_longer(cols = eval(param_names), names_to = 'param_name', values_to = 'param_value') %>%
  arrange(factor(param_name, levels = param_names), as.numeric(model_idx))

# write template files needed for running PEST++
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
write_pestpp_ins_files(params = cur_params_to_cal,
                       model_run_loc = model_run_loc,
                       model_output_file = 'output/seg_tave_water.csv',
                       file_out = sprintf('pestpp/subbasin_%s.ins', cur_subbasin_outlet),
                       delim = '@',
                       secondary_delim = '!')

# write PEST++ control file
write_pestpp_pst_files(params = cur_params_to_cal,
                       model_run_loc = model_run_loc,
                       model_output_file = 'output/seg_tave_water.csv',
                       obs = cur_obs,
                       file_out = sprintf('pestpp/subbasin_%s.pst', cur_subbasin_outlet),
                       param_transform = 'log',
                       param_ranges = param_ranges,
                       param_file_name = 'input/myparam.param',
                       tpl_file_name = sprintf('pestpp/subbasin_%s.tpl', cur_subbasin_outlet),
                       ins_file_name = sprintf('pestpp/subbasin_%s.ins', cur_subbasin_outlet))

set_sntemp_start_stop(start = start,
                      stop = stop,
                      model_run_loc = model_run_loc,
                      control_file = 'delaware.control')

# current.wd = getwd() # getting current project root wd to reset after running pest++
#
# setwd(file.path(current.wd, model_run_loc)) # set wd to where model run location is
# # to run pest++ in serial, pest++ pest_ctl_file.pst
# shell(sprintf('pestpp-ies %s', sprintf('pestpp/subbasin_%s.pst', cur_subbasin_outlet))) # run pest++; this is running pestpp 4.3.17
#
# setwd(current.wd) # set wd back to root of project

