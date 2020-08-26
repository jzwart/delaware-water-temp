
# testing jh_coef modification



source('4_model/src/EnKF_functions.R')
source('4_model/src/get_sntemp_values.R')
source('4_model/src/run_sntemp.R')
source('4_model/src/update_sntemp.R')
source('4_model/src/set_sntemp_output.R')
source('4_model_calibrate/src/calibrate_sntemp.R')
source('4_model_calibrate/src/get_subbasins.R')
source('4_model/src/get_upstream_downstream_segs.R')
source('4_model_calibrate/src/get_calibration_order.R')
library(tidyverse)
library(igraph)
start = '2000-10-01'
stop = '2004-09-30'
model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
obs_file = '3_observations/in/obs_temp_full.rds'
init_param_file = '2_3_model_parameters/out/calibration_params_init.rds'
model_run_loc = I('4_model/tmp')
orig_model_loc = I('20191002_Delaware_streamtemp')
pestpp_exe_loc = I('pestpp/bin/win')
subbasin_file = '4_model_calibrate/out/drb_subbasins.rds'
subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml'
param_ranges = as_tibble(yaml::read_yaml('4_model_calibrate/cfg/calibration_settings.yml')$param_ranges)

# copy over original run files to temporary file location
dir.create(model_run_loc, showWarnings = F)
print('Copying original model files to model working directory...')
files_to_transfer = list.files(orig_model_loc)
file.copy(from = file.path(orig_model_loc, files_to_transfer), to = model_run_loc, overwrite = T, recursive = T)


jh_coef_init = get_jh_coef(model_run_loc = orig_model_loc)

update_jh_coef(updated_params = jh_coef_init$jh_coef,
               model_run_loc = model_run_loc)

# run sntemp with original jh_coef as baseline
run_sntemp(start = start,
           stop = stop,
           spinup = T,
           restart = T,
           var_init_file = 'prms_ic.out',
           var_save_file = 'prms_ic.out',
           model_run_loc = model_run_loc)

preds_orig = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                    model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))


# modify jh_coeff to test impact on stream temp predictions

jh_coef_new = as.character(as.numeric(jh_coef_init$jh_coef) + .001)

update_jh_coef(updated_params = jh_coef_new,
               model_run_loc = model_run_loc)

run_sntemp(start = start,
           stop = stop,
           spinup = T,
           restart = T,
           var_init_file = 'prms_ic.out',
           var_save_file = 'prms_ic.out',
           model_run_loc = model_run_loc)

preds_new = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                   model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

compare = left_join(preds_orig, preds_new,
                    by = c('seg_id_nat', 'model_idx', 'date'),
                    suffix = c('_orig', '_new'))

summary(compare$water_temp_orig - compare$water_temp_new)





###### lat_temp_adj sensativity


lat_temp_adj_init = get_lat_temp_adj(model_run_loc = orig_model_loc)

update_lat_temp_adj(updated_params = lat_temp_adj_init$lat_temp_adj,
                    model_run_loc = model_run_loc)

# run sntemp with original jh_coef as baseline
run_sntemp(start = start,
           stop = stop,
           spinup = T,
           restart = T,
           var_init_file = 'prms_ic.out',
           var_save_file = 'prms_ic.out',
           model_run_loc = model_run_loc)

preds_orig = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                    model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))


# modify lat_temp_adj to test impact on stream temp predictions

lat_temp_adj_new = as.character(as.numeric(lat_temp_adj_init$lat_temp_adj) + 2)

update_lat_temp_adj(updated_params = lat_temp_adj_new,
                    model_run_loc = model_run_loc)

run_sntemp(start = start,
           stop = stop,
           spinup = T,
           restart = T,
           var_init_file = 'prms_ic.out',
           var_save_file = 'prms_ic.out',
           model_run_loc = model_run_loc)

preds_new = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                   model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

compare = left_join(preds_orig, preds_new,
                    by = c('seg_id_nat', 'model_idx', 'date'),
                    suffix = c('_orig', '_new'))

summary(compare$water_temp_orig - compare$water_temp_new)

ggplot() +
  geom_histogram(data = compare, aes(x = water_temp_orig - water_temp_new)) +
  theme_bw() +
  xlim(c(-3,1))


