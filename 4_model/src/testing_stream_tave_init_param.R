

library(tidyverse)

source('4_model/src/run_sntemp.R')
source('4_model/src/update_sntemp.R')
source('4_model/src/get_sntemp_values.R')
source('2_3_model_parameters/src/add_default_sntemp_params.R')

dates = seq.Date(as.Date('2014-06-01'), as.Date('2014-06-10'), by='day')
model_run_loc = '4_model_calibrate/tmp'

run_sntemp(start = dates[1],
           stop = dates[10],
           spinup = F,
           restart = F,
           save_ic = F,
           model_run_loc = model_run_loc)


temp_out = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                  model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))
flow_out = get_sntemp_discharge(model_output_file = file.path(model_run_loc, 'output/seg_outflow.csv'),
                                model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

temp_out
flow_out

# add_default_sntemp_params(param_names = 'stream_tave_init',
#                           model_run_loc = model_run_loc)

temp_out_2 = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                  model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))
flow_out_2 = get_sntemp_discharge(model_output_file = file.path(model_run_loc, 'output/seg_outflow.csv'),
                                model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

temp_out_2
flow_out_2

temp_out_all = left_join(temp_out, temp_out_2, by = c('model_idx', 'seg_id_nat','date'), suffix = c('_no_init','_init'))
flow_out_all = left_join(flow_out, flow_out_2, by = c('model_idx', 'seg_id_nat','date'), suffix = c('_no_init','_init'))

plot(temp_out_all$water_temp_no_init, temp_out_all$water_temp_init)
temp_out_all[which(temp_out_all$water_temp_no_init != temp_out_all$water_temp_init),]

plot(flow_out_all$discharge_no_init, flow_out_all$discharge_init)

dplyr::filter(flow_out_all, model_idx == '1')
dplyr::filter(temp_out_all, model_idx == '1')
