

# comparing PRMS-SNTemp output from original DayMet drivers and new GridMet drivers pulled in Sept 2020
library(tidyverse)
library(ggplot2)

source('4_model/src/run_sntemp.R')
source('4_model/src/get_sntemp_values.R')

start = '2004-10-01'
stop = '2016-10-01'
restart = 'F'
spinup = 'F'

# run prms-sntemp with daymet drivers
run_sntemp(start = start,
           stop = stop,
           spinup = spinup,
           restart = restart,
           model_run_loc = '20191002_Delaware_streamtemp',
           precip_file = './input/Daymet/prcp.cbh',
           tmax_file = './input/Daymet/tmax.cbh',
           tmin_file = './input/Daymet/tmin.cbh',
           sf_data_file = './input/Daymet/sf_data') # original model loc has daymet drivers

daymet_temp = get_sntemp_temperature(model_output_file = '20191002_Delaware_streamtemp/output/seg_tave_water.csv',
                                    model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')
daymet_flow = get_sntemp_discharge(model_output_file = '20191002_Delaware_streamtemp/output/seg_outflow.csv',
                                  model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')

# run prms-sntemp with gridmet drivers
run_sntemp(start = start,
           stop = stop,
           spinup = spinup,
           restart = restart,
           model_run_loc = '20191002_Delaware_streamtemp') # original model loc has daymet drivers

gridmet_temp = get_sntemp_temperature(model_output_file = '20191002_Delaware_streamtemp/output/seg_tave_water.csv',
                                     model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')
gridmet_flow = get_sntemp_discharge(model_output_file = '20191002_Delaware_streamtemp/output/seg_outflow.csv',
                                   model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')


obs = readRDS('3_observations/out/obs_temp_flow.rds')

cfs_to_cms = 1/(3.28084^3)
compare_temp = left_join(daymet_temp, gridmet_temp,
                         by = c('seg_id_nat', 'model_idx', 'date'),
                         suffix = c('_daymet', '_gridmet')) %>%
  left_join(select(obs$temp, seg_id_nat, date, temp_C), by = c('seg_id_nat', 'date'))
compare_flow = left_join(daymet_flow, gridmet_flow,
                         by = c('seg_id_nat', 'model_idx', 'date'),
                         suffix = c('_daymet', '_gridmet')) %>%
  left_join(select(obs$flow, seg_id_nat, date, discharge_cms), by = c('seg_id_nat', 'date'))


caret::RMSE(pred = compare_temp$water_temp_gridmet, obs = compare_temp$water_temp_daymet)

caret::RMSE(pred = compare_flow$discharge_gridmet, obs = compare_flow$discharge_daymet) * cfs_to_cms

caret::RMSE(pred = compare_temp$water_temp_gridmet, obs = compare_temp$temp_C, na.rm = T)
caret::RMSE(pred = compare_temp$water_temp_daymet, obs = compare_temp$temp_C, na.rm = T)

caret::RMSE(pred = compare_flow$discharge_gridmet * cfs_to_cms, obs = compare_flow$discharge_cms, na.rm = T)
caret::RMSE(pred = compare_flow$discharge_daymet * cfs_to_cms, obs = compare_flow$discharge_cms, na.rm = T)


d = filter(compare_temp, model_idx == '1')
windows()
ggplot(d, aes(x = date, y = water_temp_gridmet, color = 'Gridmet')) +
  geom_line() +
  geom_line(data = d, aes(x = date, y = water_temp_daymet, color = 'Daymet')) +
  theme_minimal() + ylab('Water Temp (C)')
ggplot(d, aes(x = water_temp_daymet, y = water_temp_gridmet)) +
  geom_point() +
  theme_minimal() +
  ylab('Gridmet Water Temp (C)') + xlab('Daymet Water Temp (C)')+
  geom_abline(color = 'red')
caret::RMSE(pred = d$water_temp_gridmet, obs = d$water_temp_daymet)

temp_summary = compare_temp %>%
  group_by(model_idx) %>%
  summarise(rmse = caret::RMSE(pred = water_temp_gridmet, obs = water_temp_daymet)) %>%
  ungroup()
ggplot(temp_summary) + geom_boxplot(aes(y = rmse), size =2) + ylim(c(0,2)) + theme_minimal()+ ylab('Temp RMSE')



d = filter(compare_flow, model_idx == '25')
windows()
ggplot(d, aes(x = date, y = discharge_gridmet * cfs_to_cms, color = 'Gridmet')) +
  geom_line() +
  geom_line(data = d, aes(x = date, y = discharge_daymet * cfs_to_cms, color = 'Daymet')) +
  theme_minimal() + ylab('Discharge (CMS)')
ggplot(d, aes(x = discharge_daymet * cfs_to_cms, y = discharge_gridmet * cfs_to_cms)) +
  geom_point() +
  theme_minimal() +
  ylab('Gridmet Discharge (CMS)') + xlab('Daymet Discharge (CMS)')+
  geom_abline(color = 'red') + scale_y_log10() + scale_x_log10()
caret::RMSE(pred = d$discharge_daymet, obs = d$discharge_gridmet) * cfs_to_cms

flow_summary = compare_flow %>%
  group_by(model_idx) %>%
  summarise(rmse = caret::RMSE(pred = discharge_daymet, obs = discharge_gridmet) * cfs_to_cms,
            mean_discharge = mean(discharge_daymet) * cfs_to_cms,
            standardized_rmse = rmse / mean_discharge) %>%
  ungroup()
ggplot(flow_summary) + geom_boxplot(aes(y = rmse), size =2) +
  #ylim(c(0,2)) +
  theme_minimal()+ ylab('Flow RMSE (CMS)') + scale_y_log10()
ggplot(flow_summary) + geom_boxplot(aes(y = standardized_rmse), size =2) +
  #ylim(c(0,2)) +
  theme_minimal()+ ylab('Flow RMSE (CMS) / Mean Flow (CMS)')
ggplot(flow_summary, aes(x = mean_discharge, y = standardized_rmse)) +
  geom_point(size =2) +
  theme_minimal()+
  ylab('Flow RMSE (CMS) / Mean Flow (CMS)') +
  xlab('Mean Discharge (CMS)') + scale_x_log10()





# comparing PRMS-SNTemp output from GridMet drivers pulled in Sept 2020, using humidity as an input or parameter
library(tidyverse)
library(ggplot2)

source('4_model/src/run_sntemp.R')
source('4_model/src/get_sntemp_values.R')

start = '2004-10-01'
stop = '2016-10-01'
restart = 'F'
spinup = 'F'

# run prms-sntemp without humidity cbh input
run_sntemp(start = start,
           stop = stop,
           spinup = spinup,
           restart = restart,
           model_run_loc = '20191002_Delaware_streamtemp',
           strmtemp_humidity_flag = 1)  # 1 = use seg_humidity parameter

humidity_param_temp = get_sntemp_temperature(model_output_file = '20191002_Delaware_streamtemp/output/seg_tave_water.csv',
                                             model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')
humidity_param_flow = get_sntemp_discharge(model_output_file = '20191002_Delaware_streamtemp/output/seg_outflow.csv',
                                           model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')

# run prms-sntemp with humidity cbh input
run_sntemp(start = start,
           stop = stop,
           spinup = spinup,
           restart = restart,
           model_run_loc = '20191002_Delaware_streamtemp')

humidity_input_temp = get_sntemp_temperature(model_output_file = '20191002_Delaware_streamtemp/output/seg_tave_water.csv',
                                             model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')
humidity_input_flow = get_sntemp_discharge(model_output_file = '20191002_Delaware_streamtemp/output/seg_outflow.csv',
                                           model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')


obs = readRDS('3_observations/out/obs_temp_flow.rds')

cfs_to_cms = 1/(3.28084^3)
compare_temp = left_join(humidity_input_temp, humidity_param_temp,
                         by = c('seg_id_nat', 'model_idx', 'date'),
                         suffix = c('_input', '_param')) %>%
  left_join(select(obs$temp, seg_id_nat, date, temp_C), by = c('seg_id_nat', 'date'))
compare_flow = left_join(humidity_input_flow, humidity_param_flow,
                         by = c('seg_id_nat', 'model_idx', 'date'),
                         suffix = c('_input', '_param')) %>%
  left_join(select(obs$flow, seg_id_nat, date, discharge_cms), by = c('seg_id_nat', 'date'))


caret::RMSE(pred = compare_temp$water_temp_input, obs = compare_temp$water_temp_param)

caret::RMSE(pred = compare_flow$discharge_input, obs = compare_flow$discharge_param) * cfs_to_cms

caret::RMSE(pred = compare_temp$water_temp_input, obs = compare_temp$temp_C, na.rm = T)
caret::RMSE(pred = compare_temp$water_temp_param, obs = compare_temp$temp_C, na.rm = T)

caret::RMSE(pred = compare_flow$discharge_input * cfs_to_cms, obs = compare_flow$discharge_cms, na.rm = T)
caret::RMSE(pred = compare_flow$discharge_param * cfs_to_cms, obs = compare_flow$discharge_cms, na.rm = T)



# comparing PRMS-SNTemp output from GridMet drivers pulled in Sept 2020, using ws as an input
library(tidyverse)
library(ggplot2)

source('4_model/src/run_sntemp.R')
source('4_model/src/get_sntemp_values.R')

start = '2004-10-01'
stop = '2016-10-01'
restart = 'F'
spinup = 'F'

# run prms-sntemp without humidity cbh input
run_sntemp(start = start,
           stop = stop,
           spinup = spinup,
           restart = restart,
           model_run_loc = '20191002_Delaware_streamtemp',
           strmtemp_humidity_flag = 1)  # 1 = use seg_humidity parameter

humidity_param_temp = get_sntemp_temperature(model_output_file = '20191002_Delaware_streamtemp/output/seg_tave_water.csv',
                                             model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')
humidity_param_flow = get_sntemp_discharge(model_output_file = '20191002_Delaware_streamtemp/output/seg_outflow.csv',
                                           model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')

# run prms-sntemp with humidity cbh input
run_sntemp(start = start,
           stop = stop,
           spinup = spinup,
           restart = restart,
           model_run_loc = '20191002_Delaware_streamtemp')

humidity_input_temp = get_sntemp_temperature(model_output_file = '20191002_Delaware_streamtemp/output/seg_tave_water.csv',
                                             model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')
humidity_input_flow = get_sntemp_discharge(model_output_file = '20191002_Delaware_streamtemp/output/seg_outflow.csv',
                                           model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp')


obs = readRDS('3_observations/out/obs_temp_flow.rds')

cfs_to_cms = 1/(3.28084^3)
compare_temp = left_join(humidity_input_temp, humidity_param_temp,
                         by = c('seg_id_nat', 'model_idx', 'date'),
                         suffix = c('_input', '_param')) %>%
  left_join(select(obs$temp, seg_id_nat, date, temp_C), by = c('seg_id_nat', 'date'))
compare_flow = left_join(humidity_input_flow, humidity_param_flow,
                         by = c('seg_id_nat', 'model_idx', 'date'),
                         suffix = c('_input', '_param')) %>%
  left_join(select(obs$flow, seg_id_nat, date, discharge_cms), by = c('seg_id_nat', 'date'))


caret::RMSE(pred = compare_temp$water_temp_input, obs = compare_temp$water_temp_param)

caret::RMSE(pred = compare_flow$discharge_input, obs = compare_flow$discharge_param) * cfs_to_cms

caret::RMSE(pred = compare_temp$water_temp_input, obs = compare_temp$temp_C, na.rm = T)
caret::RMSE(pred = compare_temp$water_temp_param, obs = compare_temp$temp_C, na.rm = T)

caret::RMSE(pred = compare_flow$discharge_input * cfs_to_cms, obs = compare_flow$discharge_cms, na.rm = T)
caret::RMSE(pred = compare_flow$discharge_param * cfs_to_cms, obs = compare_flow$discharge_cms, na.rm = T)


