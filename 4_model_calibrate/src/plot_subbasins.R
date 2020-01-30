




data_list = readRDS('4_model_calibrate/out/drb_subbasins.rds')

add_subbasin_name = function(df, name){
  cur_sub = df %>% mutate(subbasin_outlet = name)
}

# adding subbasin outlet name before concatinating list
data_list = mapply(add_subbasin_name, df = data_list, names(data_list), SIMPLIFY = F, USE.NAMES = T)
# rbinding all lists into one dataframe for plotting
data = do.call(rbind, data_list)

# obs per basin
obs_per_basin = tibble(subbasin_outlet = names(data_list), obs_train= NA, obs_test =NA )
for(cur in names(data_list)){
  cur_basin = data_list[[cur]]

  cur_obs = get_subbasin_obs(cur_basin)

  obs_per_basin$obs_train[obs_per_basin$subbasin_outlet == cur] = sum(cur_obs$date < as.Date('2004-10-02') & cur_obs$date > as.Date('1980-01-01'))
  obs_per_basin$obs_test[obs_per_basin$subbasin_outlet == cur] = sum(cur_obs$date >= as.Date('2004-10-02'))
}

data = left_join(data, obs_per_basin, by = 'subbasin_outlet')

# pull out subbasin outlets
outlets = dplyr::filter(data, seg_id_nat == subbasin_outlet)

n_params_cal = 4

data = data %>%
  group_by(subbasin_outlet) %>%
  mutate(n_segs = n(),
         obs_train_per_param = round(obs_train / (n_segs * n_params_cal))) %>%
  ungroup()

data$obs_train_cat = cut(data$obs_train, breaks = c(0,50, 100,500, 1000, 5000, 10000000))

data_low_obs = dplyr::filter(data, obs_train < 100)


library(ggplot2)

cols = rainbow(n = length(unique(data$subbasin_outlet)))
windows()
ggplot() +
  geom_sf(data = data, aes(group = subbasin_outlet, color = subbasin_outlet)) +
  scale_color_discrete() +
  theme_minimal() +
  geom_sf(data = outlets$geometry, color = 'black', lwd = 1.4)

windows()
ggplot() +
  geom_sf(data = data, aes(group = subbasin_outlet, color = obs_train)) +
  scale_color_viridis_c() +
  theme_minimal() +
  geom_sf(data = outlets$geometry, color = 'black', lwd = 1.4)

windows()
ggplot() +
  geom_sf(data = data, aes(group = subbasin_outlet, color = log10(obs_train))) +
  scale_color_viridis_c() +
  theme_minimal() +
  geom_sf(data = outlets$geometry, color = 'black', lwd = 1.4)



windows()
ggplot() +
  geom_sf(data = data, color = 'grey80') +
  geom_sf(data = data_low_obs, aes(group = subbasin_outlet, color = obs_train)) +
  scale_color_viridis_c() +
  theme_minimal() +
  geom_sf(data = outlets$geometry[outlets$subbasin_outlet %in% data_low_obs$subbasin_outlet], color = 'black', lwd = 1.4)



windows()
ggplot() +
  geom_sf(data = data, aes(group = subbasin_outlet, color = obs_train_cat)) +
  scale_color_discrete() +
  theme_minimal() +
  geom_sf(data = outlets$geometry, color = 'black', lwd = 1.4)


data_low_obs = dplyr::filter(data, obs_train_per_param < 10)

windows()
ggplot() +
  geom_sf(data = data, color = 'grey80') +
  geom_sf(data = data_low_obs, aes(group = subbasin_outlet, color = obs_train_per_param)) +
  scale_color_viridis_c(direction = -1) +
  theme_minimal() +
  geom_sf(data = outlets$geometry[outlets$subbasin_outlet %in% data_low_obs$subbasin_outlet], color = 'black', lwd = 1.4)








