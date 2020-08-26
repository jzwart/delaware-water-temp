


# sensativity for shade parameters

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
library(tidyverse)
library(igraph)
start = '1980-10-01'
stop = '2016-09-30'
model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
obs_file = '3_observations/in/obs_temp_full.rds'
init_param_file = '2_3_model_parameters/out/calibration_params_init.rds'
model_run_loc = I('4_model_for_PGDL/tmp')
orig_model_loc = I('20191002_Delaware_streamtemp')
pestpp_exe_loc = I('pestpp/bin/win')
subbasin_file = '4_model_calibrate/out/drb_subbasins.rds'
subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml'
param_ranges = as_tibble(yaml::read_yaml('4_model_calibrate/cfg/calibration_settings.yml')$param_ranges)
#######################################################################



# # use this to organize the parameters being calibrated
# model_fabric = sf::read_sf(model_fabric_file)
#
# model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
#                          model_idx = as.character(model_fabric$model_idx)) %>%
#   arrange(as.numeric(model_idx))
#
# # get model start, stop, full dates, and n_steps
# start = as.Date(as.character(start))
# stop = as.Date(as.character(stop))
# dates = get_model_dates(model_start = start, model_stop = stop, time_step = 'days')
#
# # get observations
# obs_df = readRDS(obs_file) %>%
#   left_join(model_locations, by = 'seg_id_nat')
#
# # get initial parameters
# init_params_df = readRDS(init_param_file) %>% arrange(as.numeric(model_idx))
#
# # changing parameters related to vegetation
#
#
#
# param_names = colnames(init_params_df)[3:ncol(init_params_df)]
#
# # read in the subbasins and calibration order
# subbasins = readRDS(subbasin_file)
#
# cal_order = get_calibration_order(subbasin_outlet_file = subbasin_outlet_file)
#
# # setting params to initial conditions before calibrating
# init_params = init_params_df %>%
#   pivot_longer(cols = eval(param_names), names_to = 'param_name', values_to = 'param_value') %>%
#   arrange(factor(param_name, levels = param_names), as.numeric(model_idx)) %>%
#   pull(param_value)
#
# update_sntemp_params(param_names = param_names,
#                      updated_params = init_params,
#                      model_run_loc = model_run_loc,
#                      param_file = 'input/myparam.param')

# 2020-06-26; JAZ; stream temperature model sensativity to

update_sntemp_params(param_names = 'vce',
                     updated_params = rep(15,456),
                     model_run_loc = model_run_loc,
                     param_file = 'input/myparam.param')


# run sntemp once with spinup to create a starting point for the model
run_sntemp(start = start,
           stop = stop,
           spinup = F,
           restart = F,
           var_save_file = 'prms_ic.out',
           model_run_loc = model_run_loc)


model_run_loc = I('4_model_for_PGDL/tmp')
model_output_file = I('output/stream_temp.out.nsegment')
model_fabric_file = I('GIS/Segments_subset.shp')
uncal_settings = yaml::read_yaml('4_model_for_PGDL/cfg/uncal_settings.yml')
sntemp_output_vars = uncal_settings[I('sntemp_output')]

stream_temp_intermediates = get_sntemp_intermediates(model_output_file = file.path(model_run_loc,
                                                                                   model_output_file),
                                                     model_fabric_file = file.path(model_run_loc,
                                                                                   model_fabric_file),
                                                     sntemp_vars = sntemp_output_vars[[1]])

inches_to_m = 0.0254
cfs_to_m3sec = 1/3.28084^3
langleys_day_to_w_m2 = 11.63/24 # see https://www.nrcs.usda.gov/wps/portal/nrcs/detailfull/null/?cid=stelprdb1043619 for conversion
stream_temp_intermediates = stream_temp_intermediates %>%
  mutate(parameter_value = case_when(parameter == 'seg_rain' ~ parameter_value * inches_to_m,
                                     parameter == 'seg_outflow' ~ parameter_value * cfs_to_m3sec,
                                     parameter == 'seginc_gwflow' ~ parameter_value * cfs_to_m3sec,
                                     parameter == 'seginc_sroff' ~ parameter_value * cfs_to_m3sec,
                                     parameter == 'seginc_ssflow' ~ parameter_value * cfs_to_m3sec,
                                     parameter == 'seg_upstream_inflow' ~ parameter_value * cfs_to_m3sec,
                                     parameter == 'seg_potet' ~ parameter_value * inches_to_m,
                                     parameter == 'seginc_swrad' ~ parameter_value * langleys_day_to_w_m2,
                                     TRUE ~ parameter_value))


stream_temp_intermediates_wide = stream_temp_intermediates %>%
  spread(key = 'parameter', value = 'parameter_value')

# get static variables
hru_mapping = read.table(file.path(model_run_loc, 'input/myparam.param'), skip = 4, stringsAsFactors = F)

seg_length = hru_mapping[(grep('seg_length',hru_mapping[,1])+5):(grep('seg_length',hru_mapping[,1])+460),] # seg_length, units are in m
seg_length = tibble(model_idx = as.character(seq(1,456)), seg_length = as.numeric(seg_length)) # in meters

seg_slope = hru_mapping[(grep('seg_slope',hru_mapping[,1])+5):(grep('seg_slope',hru_mapping[,1])+460),] # seg_slope, units dimensionless
seg_slope = tibble(model_idx = as.character(seq(1,456)), seg_slope = as.numeric(seg_slope)) # dimensionless

seg_elev = hru_mapping[(grep('seg_elev',hru_mapping[,1])+5):(grep('seg_elev',hru_mapping[,1])+460),] # seg_elev, units are in m
seg_elev = tibble(model_idx = as.character(seq(1,456)), seg_elev = as.numeric(seg_elev)) # in meters


stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_length, by = 'model_idx')
stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_slope, by = 'model_idx')
stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_elev, by = 'model_idx')


more_shade = stream_temp_intermediates_wide
rm(stream_temp_intermediates)
rm(stream_temp_intermediates_wide)

table(more_shade$seg_shade)
range(more_shade$seg_shade)

no_shade = feather::read_feather('4_model_for_PGDL/out/sntemp_output.feather')


combined_temp = left_join(select(more_shade, seg_id_nat, date, seg_outflow, seg_tave_water, seg_shade),
                          select(no_shade, seg_id_nat, date, seg_outflow, seg_tave_water, seg_shade),
                          by = c('seg_id_nat', 'date'),
                          suffix = c('_more_shade', '_no_shade')) %>%
  mutate(temp_diff = seg_tave_water_no_shade - seg_tave_water_more_shade,
         flow_diff = seg_outflow_no_shade - seg_outflow_more_shade,
         month = lubridate::month(date))

summary = combined_temp %>%
  group_by(month) %>%
  summarise(temp_diff_25 = quantile(temp_diff)[2],
            temp_diff_median = median(temp_diff, na.rm = T),
            temp_diff_75 = quantile(temp_diff)[4],
            temp_diff_mean = mean(temp_diff, na.rm = T),
            flow_diff_median = median(flow_diff, na.rm =T),
            shade_median = median(seg_shade_more_shade, na.rm = T),
            shade_75 = quantile(seg_shade_more_shade, na.rm = T)[4],
            shade_mean = mean(seg_shade_more_shade, na.rm = T)) %>%
  ungroup()

summary


summary_by_seg = combined_temp %>% #dplyr::filter(month == 8) %>%
  group_by(seg_id_nat) %>%
  summarise(temp_diff_median = median(temp_diff),
            temp_diff_75 = quantile(temp_diff)[4]) %>%
  ungroup()

summary_by_seg

max(summary_by_seg$temp_diff_75)


model_fabric = sf::read_sf('4_model_calibrate/tmp/GIS/Segments_subset.shp') %>%
  mutate(seg_id_nat = as.character(seg_id_nat))

summary_by_seg = left_join(model_fabric, summary_by_seg, by = 'seg_id_nat')

library(ggplot2)
windows()
ggplot() +
  geom_sf(data = summary_by_seg, aes(color = temp_diff_75), size = 2)+
  scale_color_viridis_c() +
  theme_minimal()

ggplot()+
  geom_histogram(data = combined_temp, aes(x = temp_diff)) +
  scale_x_log10() +
  theme_minimal()


