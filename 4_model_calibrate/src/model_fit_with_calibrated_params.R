


library(tidyverse)
library(ggplot2)
source('2_3_model_parameters/src/add_default_sntemp_params.R')
source('2_1_model_fabric/src/get_segment_hrus.R')
source('4_model/src/get_sntemp_values.R')
source('4_model/src/update_sntemp.R')
source('4_model/src/run_sntemp.R')

model_run_loc = I('4_model_calibrate/tmp')
init_param_file = '2_3_model_parameters/out/calibration_params_init.rds'
param_default_file = 'control/delaware.control.par_name'
start = '2004-10-01'
stop = '2016-09-30'
obs_file = '3_observations/in/obs_temp_full.rds'
model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
cur_subbasin_outlet = '4182'
subbasin_file = '4_model_calibrate/out/drb_subbasins.rds'
n_segments = 456
n_hrus = 765
#############

subbasins = readRDS(subbasin_file)

cur_subbasin = subbasins[cur_subbasin_outlet][[cur_subbasin_outlet]]

# get subbasin parameter locations
cur_model_idxs = as.character(cur_subbasin$model_idx)

# get initial parameters
init_params_list = readRDS(init_param_file)
param_names = names(init_params_list)
cal_params_list = init_params_list

# pull out parameters for current subbasin based on segment model_idx
cur_params_to_cal = get_params_by_segment(param_names = param_names,
                                          model_run_loc = model_run_loc,
                                          seg_model_idxs = cur_model_idxs)

model_fabric = sf::read_sf(model_fabric_file)

model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                         model_idx = as.character(model_fabric$model_idx)) %>%
  arrange(as.numeric(model_idx))

obs_df = readRDS(obs_file) %>%
  left_join(model_locations, by = 'seg_id_nat')

best_params_file = '4_model_calibrate/tmp/pestpp/subbasin_4182.20.par.csv'

par_cal = data.table::fread(best_params_file) %>% as_tibble()

for(i in seq_along(param_names)){
  cur_defaults = get_default_param_vals(param_name = param_names[i],
                                        model_run_loc = model_run_loc,
                                        param_default_file = param_default_file)
  cur_params = cur_params_to_cal[[param_names[i]]]

  if(cur_defaults$dim == 'one'){
    param_name_out = param_names[i]
    cal_val = par_cal %>% slice(1:49) %>% pull(param_name_out) %>% mean() %>% round(digits = 6)
    cal_params_list[[param_names[i]]][1] = cal_val
  }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nsegment'){ # a stream segment-based parameter
    for(j in seq_len(nrow(cur_params))){
      param_name_out = paste(param_names[i], cur_params$seg_model_idx[j], sep = '_')
      cal_val = par_cal %>% slice(1:49) %>% pull(param_name_out) %>% mean() %>% round(digits = 6)
      cal_params_list[[param_names[i]]][as.numeric(cur_params$seg_model_idx[j])] = cal_val
    }
  }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nhru'){
    for(j in seq_len(nrow(cur_params))){
      param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], sep = '_')
      cal_val = par_cal %>% slice(1:49) %>% pull(param_name_out) %>% mean() %>% round(digits = 6)
      cal_params_list[[param_names[i]]][as.numeric(cur_params$hru_model_idx[j])] = cal_val
    }
  }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'ngw'){
    for(j in seq_len(nrow(cur_params))){
      param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], sep = '_')
      cal_val = par_cal %>% slice(1:49) %>% pull(param_name_out) %>% mean() %>% round(digits = 6)
      cal_params_list[[param_names[i]]][as.numeric(cur_params$hru_model_idx[j])] = cal_val
    }else if(cur_defaults$ndim == '2'){
    if(grepl('nsegment', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
      # per segment x month basis is organized in order of segment model_idx and then month
      #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 455_Dec, 456_Dec
      for(j in seq_len(nrow(cur_params))){
        param_name_out = paste(param_names[i], cur_params$seg_model_idx[j], cur_params$month[j], sep = '_')
        cal_val = par_cal %>% slice(1:49) %>% pull(param_name_out) %>% mean() %>% round(digits = 6)
        cal_params_list[[param_names[i]]][as.numeric(cur_params$seg_model_idx[j]) + (as.numeric(cur_params$month[j]) - 1)*n_segments] = cal_val
      }
    }else if(grepl('nhru', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
      # per hru x month basis is organized in order of hru model_idx and then month
      #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 755_Dec, 756_Dec
      for(j in seq_len(nrow(cur_params))){
        param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], cur_params$month[j], sep = '_')
        cal_val = par_cal %>% slice(1:49) %>% pull(param_name_out) %>% mean() %>% round(digits = 6)
        cal_params_list[[param_names[i]]][as.numeric(cur_params$hru_model_idx[j]) + (as.numeric(cur_params$month[j]) - 1)*n_hrus] = cal_val
      }
    }
  }
}

update_sntemp_params(param_names = param_names,
                     updated_params = cal_params_list,
                     model_run_loc = model_run_loc,
                     param_file = 'input/myparam.param')

# run prms-sntemp with cal vals
run_sntemp(start = start,
           stop = stop,
           spinup = T,
           restart = T,
           model_run_loc = model_run_loc)

cal_temp = get_sntemp_temperature(model_output_file = '4_model_calibrate/tmp/output/seg_tave_water.csv',
                       model_fabric_file = '4_model_calibrate/tmp/GIS/Segments_subset.shp')


update_sntemp_params(param_names = param_names,
                     updated_params = init_params_list,
                     model_run_loc = model_run_loc,
                     param_file = 'input/myparam.param')

# run prms-sntemp with uncal vals
run_sntemp(start = start,
           stop = stop,
           spinup = T,
           restart = T,
           model_run_loc = model_run_loc)

uncal_temp = get_sntemp_temperature(model_output_file = '4_model_calibrate/tmp/output/seg_tave_water.csv',
                                  model_fabric_file = '4_model_calibrate/tmp/GIS/Segments_subset.shp')


cur_subbasin_outlet = '4182'

cur_subbasin = subbasins[cur_subbasin_outlet][[cur_subbasin_outlet]]

# get subbasin parameter locations
cur_model_idxs = as.character(cur_subbasin$model_idx)
cur_obs = dplyr::filter(obs_df, model_idx %in% cur_model_idxs,
                        date >= as.Date(start),
                        date <= as.Date(stop)) %>%
  arrange(as.numeric(model_idx), date)


all_preds = left_join(cal_temp, uncal_temp, by = c('seg_id_nat', 'model_idx', 'date'), suffix = c('_cal','_uncal'))

all_preds_obs = left_join(all_preds, select(cur_obs, seg_id_nat, date, temp_C), by = c('seg_id_nat', 'date'))


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

rmse(actual = all_preds_obs$temp_C, predicted = all_preds_obs$water_temp_cal)
rmse(actual = all_preds_obs$temp_C, predicted = all_preds_obs$water_temp_uncal)


all_preds_obs$cal_error = all_preds_obs$water_temp_cal - all_preds_obs$temp_C
all_preds_obs$uncal_error = all_preds_obs$water_temp_uncal - all_preds_obs$temp_C

plot(all_preds_obs$water_temp_cal, all_preds_obs$temp_C, ylim = c(0,40), xlim =c(0,40))
abline(0,1, col ='red')
points(all_preds_obs$water_temp_uncal, all_preds_obs$temp_C, col = 'grey')

plot(all_preds_obs$cal_error, all_preds_obs$uncal_error)
abline(0,1, col ='red')

plot(all_preds_obs$cal_error)
