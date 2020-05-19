




compare_rgcn_temp = function(ind_file,
                             model_output_file,
                             obs_file,
                             model_fabric_file,
                             gd_config = 'lib/cfg/gd_config.yml'){

  model_output = feather::read_feather(model_output_file)

  seg_ids = unique(model_output$seg_id_nat)

  obs = read.csv(obs_file) %>% as_tibble() %>%
    mutate(seg_id_nat = as.character(seg_id_nat),
           date = as.Date(date))

  train_obs = obs %>%
    dplyr::filter(seg_id_nat %in% seg_ids,
                  date < as.Date('2004-10-01'),
                  date > as.Date('1980-01-01')) %>%
    group_by(seg_id_nat) %>%
    summarise(n_obs_train = n()) %>%
    ungroup()


  model_fabric = sf::read_sf(model_fabric_file) %>%
    mutate(seg_id_nat = as.character(seg_id_nat)) %>%
    select(seg_id_nat, geometry)

  compare = left_join(model_output, select(obs, -subseg_id),
              by = c('seg_id_nat','date')) %>%
    dplyr::filter(date < as.Date('2016-09-29')) # 2016-09-30 had 0 temperature predictions

  model_rmse = compare %>%
    group_by(seg_id_nat, model) %>%
    summarise(rmse = rmse(stream_temp_C, temp_C),
              n_obs_test = sum(!is.na(temp_C))) %>%
    ungroup() %>%
    left_join(train_obs, by = 'seg_id_nat') %>%
    mutate(n_obs_train = ifelse(is.na(n_obs_train), 0, n_obs_train))

  model_rmse = left_join(model_fabric, model_rmse, by = 'seg_id_nat') %>%
    dplyr::filter(seg_id_nat %in% seg_ids)

  lowest_rmse = model_rmse %>%
    group_by(seg_id_nat) %>%
    dplyr::filter(rmse == min(rmse)) %>%
    ungroup()

  ggplot() +
    geom_sf(data = model_rmse, aes(color = rmse), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Temperature RMSE (C)')

  ggplot() +
    geom_sf(data = model_rmse, aes(color = log10(n_obs_test)), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Observations Test / Segment')

  ggplot() +
    geom_sf(data = model_rmse, aes(color = log10(n_obs_train)), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Observations Train / Segment')


  ggplot(model_rmse, aes(x = n_obs_train + .1, y = rmse, color = model, group = model)) +
    geom_jitter(size = 4) +
    scale_x_log10() +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    geom_smooth(method = 'loess', se = F) +
    ylab('RMSE (C)') + xlab('# of Training Obs')

  ggplot() +
    geom_sf(data = model_rmse, color = 'grey80') +
    geom_sf(data = lowest_rmse, aes(color = model), size = 2) +
    theme_minimal()+
    ggtitle('Best performing model')

  ggplot(lowest_rmse, aes(x = n_obs_train + .1, y = rmse, color = model, group = model)) +
    geom_jitter(size = 4) +
    scale_x_log10() +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ylab('RMSE (C)') + xlab('# of Training Obs')

}




compare_rgcn_flow = function(ind_file,
                             model_output_file,
                             obs_file,
                             model_fabric_file,
                             gd_config = 'lib/cfg/gd_config.yml'){

  model_output = feather::read_feather(model_output_file)

  seg_ids = unique(model_output$seg_id_nat)

  obs = read.csv(obs_file) %>% as_tibble() %>%
    mutate(seg_id_nat = as.character(seg_id_nat),
           date = as.Date(date))

  train_obs = obs %>%
    dplyr::filter(seg_id_nat %in% seg_ids,
                  date < as.Date('2004-10-01'),
                  date > as.Date('1980-01-01')) %>%
    group_by(seg_id_nat) %>%
    summarise(n_obs_train = n()) %>%
    ungroup()


  model_fabric = sf::read_sf(model_fabric_file) %>%
    mutate(seg_id_nat = as.character(seg_id_nat)) %>%
    select(seg_id_nat, geometry)

  compare = left_join(model_output, select(obs, -subseg_id),
                      by = c('seg_id_nat','date')) %>%
    dplyr::filter(date < as.Date('2016-09-29')) # 2016-09-30 had 0 flow predictions

  model_rmse = compare %>%
    group_by(seg_id_nat, model) %>%
    summarise(rmse = rmse(stream_flow_m3_s, discharge_cms),
              n_obs_test = sum(!is.na(discharge_cms)),
              mean_flow = mean(discharge_cms, na.rm = T)) %>%
    ungroup() %>%
    left_join(train_obs, by = 'seg_id_nat') %>%
    mutate(n_obs_train = ifelse(is.na(n_obs_train), 0, n_obs_train))

  model_rmse = left_join(model_fabric, model_rmse, by = 'seg_id_nat') %>%
    dplyr::filter(seg_id_nat %in% seg_ids)

  lowest_rmse = model_rmse %>%
    group_by(seg_id_nat) %>%
    dplyr::filter(rmse == min(rmse)) %>%
    ungroup()

  ggplot() +
    geom_sf(data = model_rmse, aes(color = rmse), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Flow RMSE (m3 sec-1)')

  ggplot() +
    geom_sf(data = model_rmse, aes(color = log10(n_obs_test)), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Observations Test / Segment')

  ggplot() +
    geom_sf(data = model_rmse, aes(color = log10(n_obs_train)), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Observations Train / Segment')


  ggplot(model_rmse, aes(x = n_obs_train, y = rmse, color = model, group = model)) +
    geom_point(size = 4) +
    # scale_x_log10() +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    geom_smooth(method = 'loess', se = F) +
    ylab('RMSE (m3 sec-1)') + xlab('# of Training Obs')

  ggplot(model_rmse, aes(x = mean_flow, y = rmse, color = model, group = model)) +
    geom_point(size = 4) +
    # scale_x_log10() +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    geom_smooth(method = 'lm', se = F) +
    ylab('RMSE (m3 sec-1)') + xlab('Mean flow')

  ggplot() +
    geom_sf(data = model_rmse, color = 'grey80') +
    geom_sf(data = lowest_rmse, aes(color = model), size = 2) +
    theme_minimal()+
    ggtitle('Best performing model')

  ggplot(lowest_rmse, aes(x = n_obs_train, y = rmse, color = model, group = model)) +
    geom_jitter(size = 4) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ylab('RMSE (C)') + xlab('# of Training Obs')

  ggplot(lowest_rmse, aes(x = mean_flow, y = rmse, color = model, group = model)) +
    geom_jitter(size = 4) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ylab('RMSE (C)') + xlab('Mean Flow')
}

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
