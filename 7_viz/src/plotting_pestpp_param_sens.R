

library(tidyverse)
library(ggplot2)

file = '4_model_calibrate/tmp/pestpp/subbasin_4182.msn'
sens = data.table::fread(file) %>% as_tibble()

sens$model_idx = sub('.*_', '', sens$parameter_name)

model_fabric = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') %>%
  mutate(seg_id_nat = as.character(seg_id_nat),
         model_idx = as.character(model_idx)) %>%
  select(seg_id_nat, model_idx, geometry)

sens = filter(sens, model_idx %in% model_fabric$model_idx) # gets rid of albedo param

sens = mutate(sens, param_group = case_when(grepl('ss_tau', parameter_name) ~ 'ss_tau',
                                            grepl('gw_tau', parameter_name) ~ 'gw_tau',
                                            grepl('vce', parameter_name) ~ 'vce',
                                            grepl('vdemx', parameter_name) ~ 'vdemx',
                                            grepl('vhe', parameter_name) ~ 'vhe'))

# get observations
obs_df = readRDS('3_observations/in/obs_temp_full.rds') %>%
  filter(date >= as.Date('2000-10-01'), date <= as.Date('2005-09-30')) %>%
  group_by(seg_id_nat) %>%
  summarise(n_obs = n()) %>%
  ungroup()

plot_data = left_join(model_fabric, sens, by = c('model_idx')) %>% filter(model_idx %in% unique(sens$model_idx)) %>%
  left_join(obs_df)

ggplot() +
  geom_sf(data = plot_data, aes(color = sen_mean_abs), size = 2) +
  geom_sf(data = plot_data, color = 'grey80') +

  # scale_color_viridis_c(direction = -1) +
  scale_color_gradient2(low = 'blue', high = 'red')+
  theme_minimal()+
  facet_wrap(~param_group)+
  ggtitle('Parameter Sensativity')


ggplot() +
  geom_sf(data = filter(plot_data, !is.na(n_obs)), aes(color = n_obs), size = 2) +
  geom_sf(data = plot_data, color = 'grey80') +
  scale_color_viridis_c(direction = -1) +
  theme_minimal()+
  ggtitle('# of Obs')

ggplot(data = plot_data, aes(x = n_obs, y = sen_mean_abs, group = param_group, color = param_group)) +
  geom_point()+
  facet_wrap(~param_group, scales = 'free_y') +
  geom_smooth(method = 'lm', se = F) +
  theme_bw()

