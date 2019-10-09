
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)

get_modeled_discharge = function(model_output_file, model_fabric_file){

  model_output = read.csv(model_output_file, header = T, stringsAsFactors = F)

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx))

  model_output = model_output %>% as_tibble() %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key = 'model_idx', value = 'discharge', starts_with('X')) %>%
    mutate(model_idx = gsub('X', '', model_idx)) %>%
    rename(date = Date) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    select(seg_id_nat, model_idx, date, discharge)

  return(model_output)
}

get_modeled_temperature = function(model_output_file, model_fabric_file){

  model_output = read.csv(model_output_file, header = T, stringsAsFactors = F)

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx))

  model_output = model_output %>% as_tibble() %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key = 'model_idx', value = 'water_temp', starts_with('X')) %>%
    mutate(model_idx = gsub('X', '', model_idx)) %>%
    rename(date = Date) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    select(seg_id_nat, model_idx, date, water_temp)

  return(model_output)
}

get_modeled_intermediates = function(model_output_file, model_fabric_file){

  model_output = read.table(model_output_file, header = T, stringsAsFactors = F) %>%
    dplyr::slice(-1) # first row indicates column type

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx))

  model_output = model_output %>% as_tibble() %>%
    mutate(timestamp = as.Date(timestamp),
           nsegment = as.character(nsegment)) %>%
    rename(date = timestamp,
           model_idx = nsegment) %>%
    gather(key = 'parameter', value = 'parameter_value', starts_with('seg')) %>%
    mutate(parameter_value = as.numeric(parameter_value)) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    select(seg_id_nat, model_idx, date, parameter, parameter_value)

  return(model_output)
}


# getting model output to Xiaowei; 2019-09-27
# model_output_file = '20190913_Delaware_streamtemp/output/seg_outflow.csv'
# model_fabric_file = '20190913_Delaware_streamtemp/GIS/Segments_subset.shp'
#
# discharge = get_modeled_discharge(model_output_file = model_output_file,
#                                   model_fabric_file = model_fabric_file)
#
# # feather::write_feather(x = discharge, path = 'data_for_Xiaowei/model_estimated_discharge.feather')
#
# model_output_file = '20190913_Delaware_streamtemp/output/seg_tave_water.csv'
# model_fabric_file = '20190913_Delaware_streamtemp/GIS/Segments_subset.shp'
#
# stream_temp = get_modeled_temperature(model_output_file = model_output_file,
#                                       model_fabric_file = model_fabric_file)

# feather::write_feather(x = stream_temp, path = 'data_for_Xiaowei/model_estimated_temperature.feather')

model_output_file = '20190913_Delaware_streamtemp/output/stream_temp.out.nsegment'
model_fabric_file = '20190913_Delaware_streamtemp/GIS/Segments_subset.shp'

stream_temp_intermediates = get_modeled_intermediates(model_output_file = model_output_file,
                                      model_fabric_file = model_fabric_file)


# seg_4_int = dplyr::filter(stream_temp_intermediates, parameter == 'seg_rain', model_idx == '4')
# seg_4_temp = dplyr::filter(stream_temp,  model_idx == '4')
# seg_4_disch = dplyr::filter(discharge, model_idx == '4')
#
# plot(seg_4_int$parameter_value~seg_4_temp$water_temp)
# abline(0,1)
#
# plot(seg_4_int$parameter_value, type = 'l')

##### sntemp units
#' tave_air: C
#' tave_water: C
#' rain: inches
#' outflow: cfs

# converting rainfall from inches to m and converting discharge from cfs to m^3 s-1. And SWrad from langleys/day to W/m2
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
hru_mapping = read.table('20190913_Delaware_streamtemp/control/delaware.control.param', skip = 4, stringsAsFactors = F)

seg_length = hru_mapping[(grep('seg_length',hru_mapping[,1])+6):(grep('seg_length',hru_mapping[,1])+461),] # seg_length, units are in m
seg_length = tibble(model_idx = as.character(seq(1,456)), seg_length = as.numeric(seg_length)) # in meters

seg_slope = hru_mapping[(grep('seg_slope',hru_mapping[,1])+6):(grep('seg_slope',hru_mapping[,1])+461),] # seg_slope, units dimensionless
seg_slope = tibble(model_idx = as.character(seq(1,456)), seg_slope = as.numeric(seg_slope)) # dimensionless

seg_elev = hru_mapping[(grep('seg_elev',hru_mapping[,1])+6):(grep('seg_elev',hru_mapping[,1])+461),] # seg_elev, units are in m
seg_elev = tibble(model_idx = as.character(seq(1,456)), seg_elev = as.numeric(seg_elev)) # in meters


stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_length, by = 'model_idx')
stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_slope, by = 'model_idx')
stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_elev, by = 'model_idx')


feather::write_feather(x = stream_temp_intermediates_wide, path = 'data_for_Xiaowei/sntemp_input_output.feather')

# subset network for Xiaowei
sub_net = readRDS('data_for_Xiaowei/network_subset.rds')
sub_net_sites = unique(sub_net$edges$seg_id_nat)
stream_temp_intermediates_wide_sub = stream_temp_intermediates_wide %>%
  dplyr::filter(seg_id_nat %in% sub_net_sites)

length(unique(stream_temp_intermediates_wide_sub$seg_id_nat))

feather::write_feather(x = stream_temp_intermediates_wide_sub, path = 'data_for_Xiaowei/sntemp_input_output_subset.feather')


hist(stream_temp_intermediates_wide_sub$seg_tave_air)




# output for Xiaowei
# seg_ccov, seg_daylight, seg_gwflow, seg_humid, seg_inflow, seg_lateral_inflow, seg_melt, seg_outflow,
# seg_potet, seg_rain, seg_shade, seg_sroff, seg_ssflow, seg_tave_air, seg_tave_gw, seg_tave_lat,
# seg_tave_sroff, seg_tave_ss, seg_tave_upstream, seg_tave_water, seg_upstream_inflow, seg_width

model_output_file = '20190913_Delaware_streamtemp/output/stream_temp.out.nsegment'
model_fabric_file = '20190913_Delaware_streamtemp/GIS/Segments_subset.shp'

intermediates = get_modeled_intermediates(model_output_file = model_output_file,
                                          model_fabric_file = model_fabric_file)


hru_mapping = read.table('20190913_Delaware_streamtemp/control/delaware.control.param', skip = 4, stringsAsFactors = F)

hru_area = hru_mapping[(grep('hru_area',hru_mapping[,1])+6):(grep('hru_area',hru_mapping[,1])+770),] # hru area, units are in acres
# hru_area

hru_seg_map = hru_mapping[(grep('hru_segment',hru_mapping[,1])+6):(grep('hru_segment',hru_mapping[,1])+770),] # mapping hru to segment model_idx
# hru_seg_map
hru_sf = sf::read_sf('20190913_Delaware_streamtemp/GIS/HRU_subset.shp')
hru_seg_map = tibble(model_idx = as.integer(seq(1,765)), seg_model_idx = as.integer(hru_seg_map), hru_area = as.numeric(hru_area))
hru_seg_map = left_join(hru_sf, hru_seg_map, by = 'model_idx')


seg_K_coef = hru_mapping[(grep('K_coef',hru_mapping[,1])+6):(grep('K_coef',hru_mapping[,1])+461),] # K_coeff, units are in hours
seg_K_coef = tibble(model_idx = as.integer(seq(1,456)), k_coef = as.numeric(seg_K_coef))
# seg_K_coef = left_join(model_fabric, seg_K_coef, by = 'model_idx')


seg_length = hru_mapping[(grep('seg_length',hru_mapping[,1])+6):(grep('seg_length',hru_mapping[,1])+461),] # seg_length, units are in m
seg_length = tibble(model_idx = as.integer(seq(1,456)), length = as.numeric(seg_length))
seg_length_k_coef = left_join(model_fabric, left_join(seg_K_coef, seg_length, by = 'model_idx'), by = 'model_idx')



# plotting seg_K_coef
ggplot(seg_length_k_coef) +
  geom_sf(aes(col = k_coef))+
  scale_color_viridis_c(begin = 0, end = 1, direction = -1) +
  theme_minimal()

plot(seg_length_k_coef$k_coef~seg_length_k_coef$length, ylab= 'K_coef (hrs)', xlab = 'Segment Length (m)')

# plotting segment matching to HRU's
ggplot(hru_seg_map) +
  geom_sf(aes(fill = seg_model_idx))+
  scale_fill_viridis_c(begin = 0, end = 1, direction = -1) +
  geom_sf(data = seg_length_k_coef, aes(col = model_idx)) +
  scale_color_viridis_c(begin = 0, end = 1, direction = -1) +
  theme_minimal()

# plotting segment not matching to HRU's
seg_length_k_coef = seg_length_k_coef %>%
  mutate(hru_match = model_idx %in% hru_seg_map$seg_model_idx)

d = seg_length_k_coef[seg_length_k_coef$hru_match==F,]

ggplot(seg_length_k_coef) +
  geom_sf(aes(col = hru_match)) +
  theme_minimal()


bbox = sf::st_bbox(d[5:10,])
ylim = bbox[c(2,4)]
xlim = bbox[c(1,3)]

ggplot(seg_length_k_coef) +
  geom_sf(data = hru_seg_map, size = 2) +
  geom_sf(aes(col = hru_match), size = 1.4) +
  theme_minimal()+
  ylim(ylim)+
  xlim(xlim)


ggplot(seg_length_k_coef) +
  geom_sf(data = hru_seg_map, size = 2) +
  geom_sf(aes(col = hru_match)) +
  theme_minimal()+
  ylim(ylim)+
  xlim(xlim)

seg_4 = dplyr::filter(intermediates, model_idx == '4') %>%
  spread(key = 'parameter', value = 'parameter_value')

plot(seg_4$seg_outflow[2:nrow(seg_4)]~ c(seg_4$seg_inflow[1:1095] + seg_4$seg_lateral_inflow[1:1095]))


table(model_output$parameter)

windows()
ggplot(model_output, aes(x = parameter_value)) +
  facet_wrap(~parameter, scales = 'free') +
  scale_y_log10() +
  geom_histogram()


range(model_output$parameter_value[model_output$parameter == 'seg_shade'])
range(model_output$parameter_value[model_output$parameter == 'seg_daylight'])
range(model_output$parameter_value[model_output$parameter == 'seg_humid'])
range(model_output$parameter_value[model_output$parameter == 'seg_tave_sroff'])
range(model_output$parameter_value[model_output$parameter == 'seg_potet'])
range(model_output$parameter_value[model_output$parameter == 'seg_tave_air'])
boxplot(model_output$parameter_value[model_output$parameter == 'seg_tave_air'])

sub = dplyr::filter(model_output, parameter %in% c('seg_tave_air', 'seg_tave_gw', 'seg_tave_ss'))

ggplot(sub) +
  geom_violin(aes(x = parameter, y = parameter_value))+
  # geom_jitter(aes(x = parameter, y = parameter_value)) +
  facet_wrap(~parameter, scales = 'free') +
  ylab('Temperature (C)')


hot_air_dates = dplyr::filter(model_output, parameter == 'seg_tave_air', parameter_value > 120)
table(hot_air_dates$seg_id_nat)
# appears to be a single segment that is causing issues => 3558 seg_id_nat
ggplot(model_fabric) +
  geom_sf()+
  geom_sf(data =dplyr::filter(model_fabric, seg_id_nat == 3558), aes(col = 'red', fill = 'red', size = 8)) +
  theme_minimal()

ggplot() +
  geom_sf(data =dplyr::filter(model_fabric, seg_id_nat %in% seq(2120, 3980))) +
  geom_sf(data =dplyr::filter(model_fabric, seg_id_nat == 3558), aes(col = 'red')) +
  theme_minimal()

cool_air_dates = dplyr::filter(model_output, parameter == 'seg_tave_air', parameter_value < 120, parameter_value > -40)
ggplot(cool_air_dates) +
  geom_histogram(aes(x = parameter_value))+
  xlab('Air Temperature (C)')

seg_179_air = dplyr::filter(model_output, model_idx == 179, parameter == 'seg_tave_air')
plot(seg_179_air$parameter_value~seg_179_air$date, type = 'l', ylab = 'Air Temperature (C)', xlab = '')

hot_ss_dates = dplyr::filter(model_output, parameter == 'seg_tave_ss', parameter_value > 60)
table(hot_ss_dates$seg_id_nat)

ggplot(model_fabric) +
  geom_sf()+
  geom_sf(data =dplyr::filter(model_fabric, seg_id_nat %in% unique(hot_ss_dates$seg_id_nat)),
          aes(col = 'red')) +
  theme_minimal()

seg_3573_ss = dplyr::filter(model_output, seg_id_nat == 3573, parameter == 'seg_tave_ss')
plot(seg_3573_ss$parameter_value~seg_3573_ss$date, type = 'l', ylab = 'Subsurface Temperature (C)', xlab = '')

hot_gw_dates = dplyr::filter(model_output, parameter == 'seg_tave_gw', parameter_value > 50)
table(hot_gw_dates$seg_id_nat)

ggplot(model_fabric) +
  geom_sf()+
  geom_sf(data =dplyr::filter(model_fabric, seg_id_nat %in% unique(hot_gw_dates$seg_id_nat)),
          aes(col = 'red')) +
  theme_minimal()

seg_1494_gw = dplyr::filter(model_output, seg_id_nat == 1494, parameter == 'seg_tave_gw')
plot(seg_1494_gw$parameter_value~seg_1494_gw$date, type = 'l', ylab = 'GW Temperature (C)', xlab = '')


hot_water_dates = dplyr::filter(model_output, parameter == 'seg_tave_water', parameter_value > 50)
table(hot_water_dates$seg_id_nat)

ggplot(model_fabric) +
  geom_sf()+
  geom_sf(data =dplyr::filter(model_fabric, seg_id_nat %in% unique(hot_water_dates$seg_id_nat)),
          aes(col = 'red')) +
  theme_minimal()

seg_1496_water = dplyr::filter(model_output, seg_id_nat == 1496, parameter == 'seg_tave_water')
plot(seg_1496_water$parameter_value~seg_1496_water$date, type = 'l', ylab = 'Stream Temperature (C)', xlab = '')



# system2('C:/Users/jzwart/delaware-water-temp/20190913_Delaware_streamtemp/delaware.bat')



#############################################################
model_output_file = '20190913_Delaware_streamtemp/output/seg_tave_water.csv'

model_output_file = '20190913_Delaware_streamtemp/output/stream_temp.out.nsegment'
model_output_file = '20190913_Delaware_streamtemp/output/stream_temp.out.nhru'
model_output_file = '20190913_Delaware_streamtemp/output/stream_temp.out.one'

model_output_file = '20190913_Delaware_streamtemp/output/seg_outflow.csv'
model_fabric_file = '20190913_Delaware_streamtemp/GIS/Segments_subset.shp'

discharge = get_modeled_discharge(model_output_file = model_output_file,
                                  model_fabric_file = model_fabric_file)

discharge_sub = discharge %>%
  dplyr::filter(date < as.Date('1981-01-01'))

plot_fabric = model_fabric %>% mutate(seg_id_nat = as.character(seg_id_nat)) %>%
  left_join(dplyr::select(discharge_sub, seg_id_nat, date, discharge), by = 'seg_id_nat')

plot_fabric

library(ggplot2)
library(gganimate)

dates = seq.Date(from = as.Date('1980-11-01'), to = as.Date('1980-11-01'), by = 'days')

ggplot(dplyr::filter(plot_fabric, date %in% dates)) +
  geom_sf(aes(col = discharge)) +
  scale_color_viridis_c(begin = 0, end = 1, direction = -1) +
  # transition_time(as.numeric(date)) +
  theme_minimal()

plot_fabric = model_fabric %>% mutate(seg_id_nat = as.character(seg_id_nat)) %>%
  left_join(dplyr::select(dplyr::filter(model_output, date < as.Date('1981-01-01')), seg_id_nat, date, water_temp),
            by = 'seg_id_nat')

ggplot(dplyr::filter(plot_fabric, date %in% dates, water_temp >-5)) +
  geom_sf(aes(col = water_temp)) +
  scale_color_viridis_c(begin = 0, end = 1, direction = 1) +
  # transition_time(as.numeric(date)) +
  theme_minimal()







