

# plotting forecasted data
library(tidyverse)
library(ncdf4)
library(ggplot2)
library(scales)
source('4_model_forecast/src/nc_forecast_utils.R')

forecast_out_file = '4_model_forecast/out/DRB_DA_SNTemp_20201023_2014-05-01_to_2014-08-10.nc'
obs_file = '3_observations/out/obs_temp_flow.rds'
model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'

model_fabric = sf::read_sf(model_fabric_file)
model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                         model_idx = as.character(model_fabric$model_idx)) %>%
  arrange(as.numeric(model_idx))

obs = readRDS(obs_file)
temp_obs = obs$temp %>%
  left_join(model_locations, by = 'seg_id_nat')

most_obs = c('416', '422', '423', '446', '455', '421', '430', '439', '442', '445') # segments with most obs
out = nc_forecast_get(nc_file = forecast_out_file,
                      var_name = 'seg_tave_water',
                      model_idxs = most_obs) %>%
  group_by(model_idx, valid_time, issue_time) %>%
  mutate(mean_seg_tave_water = mean(seg_tave_water)) %>%
  ungroup() %>%
  mutate(lead_time = as.integer(valid_time - issue_time)) %>%
  left_join(select(temp_obs, model_idx, date, temp_C), by = c('model_idx' = 'model_idx', 'valid_time' = 'date'))


windows()
ggplot(out, aes(x = valid_time, y = mean_seg_tave_water, group = issue_time, color = issue_time))+
  geom_line(size =3) +
  geom_line(data = out, aes(x = valid_time, y = seg_tave_water, group = interaction(ensemble, issue_time), color = issue_time))+
  geom_point(data = out, aes(x = valid_time, y = temp_C), color = 'red') +
  ylab('Stream Water Temperature (C)') + xlab('') +
  theme_minimal()

windows()
ggplot(out, aes(x = valid_time, y = mean_seg_tave_water, group = issue_time, color = lead_time))+
  geom_line(size =3) +
  geom_line(data = out, aes(x = valid_time, y = seg_tave_water, group = interaction(ensemble, issue_time), color = lead_time))+
  geom_point(data = out, aes(x = valid_time, y = temp_C), color = 'red') +
  ylab('Stream Water Temperature (C)') + xlab('') +
  theme_minimal() + facet_wrap(~model_idx, scales = 'free')

forecast_accuracy = out %>%
  group_by(lead_time, valid_time) %>%
  summarise(rmse = caret::RMSE(mean_seg_tave_water, temp_C, na.rm = T)) %>%
  ungroup()

ggplot(forecast_accuracy, aes(x = valid_time, y = rmse, group = lead_time, color = lead_time)) +
  geom_point(size = 4, alpha = .4) +
  theme_minimal() +
  xlab('Valid Date') +
  ylab('RMSE (C)') +
  geom_smooth(se = F, size = 2)

forecast_accuracy = out %>%
  group_by(lead_time) %>%
  summarise(rmse = caret::RMSE(mean_seg_tave_water, temp_C, na.rm = T)) %>%
  ungroup()

ggplot(forecast_accuracy, aes(x = lead_time, y = rmse)) +
  geom_point(size = 4) +
  scale_x_continuous(breaks = pretty_breaks())+
  theme_minimal() +
  xlab('Lead Time (Days)') +
  ylab('RMSE (C)')

