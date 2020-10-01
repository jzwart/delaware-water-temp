


library(tidyverse)
library(ggplot2)
source('2_3_model_parameters/src/add_default_sntemp_params.R')
source('2_1_model_fabric/src/get_segment_hrus.R')
source('4_model/src/get_sntemp_values.R')
source('4_model/src/update_sntemp.R')
source('4_model/src/run_sntemp.R')

model_run_loc = I('4_model_calibrate/tmp')
orig_model_loc = I('20191002_Delaware_streamtemp')
init_param_file = '2_3_model_parameters/out/calibration_params_init.rds'
param_default_file = 'control/delaware.control.par_name'
start = '2004-10-02'
stop = '2016-09-30'
obs_file = '3_observations/out/obs_temp_flow.rds'
model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
cur_subbasin_outlet = '4182'
subbasin_file = '4_model_calibrate/out/drb_subbasins.rds'
n_segments = 456
n_hrus = 765
cfs_to_cms = 1/(3.28084^3)
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

temp_obs_df = readRDS(obs_file)$temp %>%
  left_join(model_locations, by = 'seg_id_nat')

flow_obs_df = readRDS(obs_file)$flow %>%
  left_join(model_locations, by = 'seg_id_nat') %>%
  mutate(discharge_cfs = discharge_cms * (3.28084^3))

best_params_file = '4_model_calibrate/tmp/pestpp/subbasin_4182.20.par.csv' # previous best with few params
best_params_file = '4_model_calibrate/tmp/pestpp/subbasin_4182.10.par.csv'
best_params_file = '4_model_calibrate/tmp/pestpp/subbasin_4182.15.par.csv' # cal with temp and flow
best_params_file = '4_model_calibrate/tmp/pestpp/denali_cal/flow/subbasin_4182.13.par.csv'
best_params_file = '4_model_calibrate/tmp/pestpp/denali_cal/temp/subbasin_4182.8.par.csv'

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
    }
  }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nssr'){
    for(j in seq_len(nrow(cur_params))){
      param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], sep = '_')
      cal_val = par_cal %>% slice(1:49) %>% pull(param_name_out) %>% mean() %>% round(digits = 6)
      cal_params_list[[param_names[i]]][as.numeric(cur_params$hru_model_idx[j])] = cal_val
    }
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

dir.create(model_run_loc, showWarnings = F)
print('Copying original model files to model working directory...')
files_to_transfer = list.files(orig_model_loc)
file.copy(from = file.path(orig_model_loc, files_to_transfer), to = model_run_loc, overwrite = T, recursive = T)

# update_sntemp_params(param_names = param_names,
#                      updated_params = init_params_list,
#                      model_run_loc = model_run_loc,
#                      param_file = 'input/myparam.param')

# run prms-sntemp with uncal vals
run_sntemp(start = start,
           stop = stop,
           spinup = T,
           restart = T,
           model_run_loc = model_run_loc)

uncal_temp = get_sntemp_temperature(model_output_file = '4_model_calibrate/tmp/output/seg_tave_water.csv',
                                    model_fabric_file = '4_model_calibrate/tmp/GIS/Segments_subset.shp')
uncal_flow = get_sntemp_discharge(model_output_file = '4_model_calibrate/tmp/output/seg_outflow.csv',
                                  model_fabric_file = '4_model_calibrate/tmp/GIS/Segments_subset.shp')


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
cal_flow = get_sntemp_discharge(model_output_file = '4_model_calibrate/tmp/output/seg_outflow.csv',
                                  model_fabric_file = '4_model_calibrate/tmp/GIS/Segments_subset.shp')


cur_subbasin_outlet = '4182'

cur_subbasin = subbasins[cur_subbasin_outlet][[cur_subbasin_outlet]]

# get subbasin parameter locations
cur_model_idxs = as.character(cur_subbasin$model_idx)
cur_temp_obs = dplyr::filter(temp_obs_df, model_idx %in% cur_model_idxs,
                             date >= as.Date(start),
                             date <= as.Date(stop)) %>%
  arrange(as.numeric(model_idx), date)

cur_flow_obs = dplyr::filter(flow_obs_df, model_idx %in% cur_model_idxs,
                             date >= as.Date(start),
                             date <= as.Date(stop)) %>%
  arrange(as.numeric(model_idx), date)


all_preds_temp = left_join(cal_temp, uncal_temp, by = c('seg_id_nat', 'model_idx', 'date'), suffix = c('_cal','_uncal'))

all_preds_obs_temp = left_join(all_preds_temp, select(cur_temp_obs, seg_id_nat, date, temp_C), by = c('seg_id_nat', 'date'))

all_preds_flow = left_join(cal_flow, uncal_flow, by = c('seg_id_nat', 'model_idx', 'date'), suffix = c('_cal','_uncal'))

all_preds_obs_flow = left_join(all_preds_flow, select(cur_flow_obs, seg_id_nat, date, discharge_cfs), by = c('seg_id_nat', 'date'))


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

# all_preds_obs_temp = all_preds_obs_temp[all_preds_obs_temp$water_temp_cal>-20 & all_preds_obs_temp$water_temp_uncal>-20, ]

rmse(actual = all_preds_obs_temp$temp_C, predicted = all_preds_obs_temp$water_temp_cal)
rmse(actual = all_preds_obs_temp$temp_C, predicted = all_preds_obs_temp$water_temp_uncal)

rmse(actual = all_preds_obs_flow$discharge_cfs, predicted = all_preds_flow$discharge_cal) * cfs_to_cms
rmse(actual = all_preds_obs_flow$discharge_cfs, predicted = all_preds_flow$discharge_uncal) * cfs_to_cms


d = dplyr::filter(all_preds_obs_temp, model_idx == '445')

plot(d$water_temp_cal, type = 'l')
points(d$temp_C, col = 'red')
lines(d$water_temp_uncal, col ='grey')

all_preds_obs_temp$cal_error = all_preds_obs_temp$water_temp_cal - all_preds_obs_temp$temp_C
all_preds_obs_temp$uncal_error = all_preds_obs_temp$water_temp_uncal - all_preds_obs_temp$temp_C

plot(all_preds_obs_temp$water_temp_cal, all_preds_obs_temp$temp_C, ylim = c(0,40), xlim =c(0,40))
abline(0,1, col ='red')
points(all_preds_obs_temp$water_temp_uncal, all_preds_obs_temp$temp_C, col = 'grey')

plot(all_preds_obs_temp$cal_error, all_preds_obs_temp$uncal_error)
abline(0,1, col ='red')

plot(all_preds_obs_temp$cal_error)

hist(all_preds_obs_temp$cal_error)
hist(all_preds_obs_temp$uncal_error)


summary(all_preds_obs_flow$discharge_cal)
summary(all_preds_obs_flow$discharge_uncal)


d = dplyr::filter(all_preds_obs_flow, model_idx == '416')

plot(d$discharge_cal, type = 'l')
lines(d$discharge_cfs, col = 'red')
lines(d$discharge_uncal, col ='grey')

flow_summary = all_preds_obs_flow %>%
  group_by(model_idx, seg_id_nat) %>%
  summarise(cal_rmse = rmse(discharge_cal, discharge_cfs),
            uncal_rmse = rmse(discharge_uncal, discharge_cfs),
            mean_discharge = mean(discharge_cfs)) %>%
  ungroup() %>%
  mutate(cal_rmse = cal_rmse * cfs_to_cms,
         uncal_rmse = uncal_rmse * cfs_to_cms,
         mean_discharge = mean_discharge * cfs_to_cms,
         rmse_diff = uncal_rmse - cal_rmse) %>%
  filter(model_idx %in% cur_model_idxs)

flow_summary

windows()
ggplot(flow_summary, aes(x = mean_discharge, y = rmse_diff)) +
  geom_point(size =2)+ theme_minimal() +
  ylab('Reduction in RMSE (cms)') + xlab('Mean Discharge (cms)')

# all_flow = left_join(all_preds_obs_flow, flow_summary, by = 'model_idx')
#
# ggplot(all_flow, aes(x = mean_discharge, y = discharge_cal, group = mean_discharge)) +
#   geom_histogram()
arrange(flow_summary, mean_discharge)

# d = dplyr::filter(all_preds_obs_flow, model_idx == '416')
#
# windows()
# ggplot(d, aes(x = date, y = discharge_cal, color = 'Calibrated')) +
#   geom_line()+
#   geom_line(data = d, aes(x = date, y = discharge_cfs, color = 'Obs')) +
#   geom_line(data = d, aes(x = date, y = discharge_uncal, color = 'Uncal')) +
#   theme_minimal() +
#   ylab('Discharge (cms)') + xlab('Date')



d = dplyr::filter(all_preds_obs_flow, model_idx == '416') %>%
  mutate(cal_error = discharge_cal - discharge_cfs,
         uncal_error = discharge_uncal - discharge_cfs,
         delta_error = cal_error - uncal_error)

windows()
ggplot(d, aes(x = date, y = discharge_cal * cfs_to_cms, color = 'Calibrated')) +
  geom_line()+
  geom_line(data = d, aes(x = date, y = discharge_cfs* cfs_to_cms, color = 'Obs')) +
  geom_line(data = d, aes(x = date, y = discharge_uncal* cfs_to_cms, color = 'Uncal')) +
  theme_minimal() +
  ylab('Discharge (cms)') + xlab('Date')

ggplot(d, aes(x = discharge_cfs* cfs_to_cms, y = discharge_cal* cfs_to_cms, color = 'Calibrated')) +
  geom_point()+
  geom_point(data = d, aes(x = discharge_cfs* cfs_to_cms, y = discharge_uncal* cfs_to_cms, color = 'Uncalibrated')) +
  theme_minimal() +
  ylab('Predicted Discharge (cms)') + xlab('Observed Discharge (cms)') + scale_y_log10() + scale_x_log10() +
  geom_abline()

ggplot(d, aes(x = discharge_cfs* cfs_to_cms, y = cal_error* cfs_to_cms, color = 'Calibrated Error')) +
  geom_point()+
  geom_point(data = d, aes(x = discharge_cfs* cfs_to_cms, y = uncal_error* cfs_to_cms, color = 'Uncalibrated Error')) +
  theme_minimal() +
  ylab('Error (pred - obs, cms)') + xlab('Observed Discharge (cms)') + scale_x_log10()
#
# ggplot(d, aes(x = discharge_cfs, y = delta_error)) +
#   geom_point()+
#   theme_minimal() +
#   ylab('Error (cms)') + xlab('Observed Discharge (cms)') + scale_x_log10()
