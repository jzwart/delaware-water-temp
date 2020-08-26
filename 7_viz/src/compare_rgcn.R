




compare_rgcn_temp = function(ind_file,
                             model_output_file,
                             obs_file,
                             uncal_sntemp_file,
                             model_fabric_file,
                             gd_config = 'lib/cfg/gd_config.yml'){

  model_output = feather::read_feather(model_output_file)

  seg_ids = unique(model_output$seg_id_nat)

  obs = read.csv(obs_file) %>% as_tibble() %>%
    mutate(seg_id_nat = as.character(seg_id_nat),
           date = as.Date(date))

  uncal_sntemp = feather::read_feather(uncal_sntemp_file) %>%
    dplyr::filter(seg_id_nat %in% seg_ids,
                  date >= as.Date('2004-10-01'),
                  date <= as.Date('2016-09-30'))

  uncal_sntemp_temp = uncal_sntemp %>%
    rename(stream_temp_C = seg_tave_water) %>%
    mutate(model = 'uncal_sntemp') %>%
    select(seg_id_nat, date, stream_temp_C, model)

  model_output = bind_rows(model_output, uncal_sntemp_temp)

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

  compare_to_sntemp = left_join(dplyr::filter(model_rmse, model != 'uncal_sntemp'),
                                dplyr::filter(model_rmse, model =='uncal_sntemp') %>%
                                  select(seg_id_nat, rmse) %>% tibble(),
                                by = c('seg_id_nat'), suffix = c('', '_sntemp')) %>%
    select(-geometry_sntemp) %>%
    mutate(delta_rmse_sntemp = rmse - rmse_sntemp)

  ggplot() +
    geom_sf(data = model_rmse, color = 'grey80') +
    geom_sf(data = dplyr::filter(model_rmse, !is.na(rmse)), aes(color = rmse), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Temperature RMSE (C)')

  ggplot() +
    geom_sf(data = compare_to_sntemp, color = 'grey80') +
    geom_sf(data = dplyr::filter(compare_to_sntemp, !is.na(delta_rmse_sntemp)),
            aes(color = delta_rmse_sntemp), size = 2) +
    scale_color_gradient2(low = 'blue', high = 'red')+
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Model RMSE - SNTemp RMSE (C)')

  ggplot() +
    geom_sf(data = model_rmse, color = 'grey80') +
    geom_sf(data = dplyr::filter(model_rmse, n_obs_test >0), aes(color = log10(n_obs_test)), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    ggtitle('Observations Test / Segment')

  ggplot() +
    geom_sf(data = model_rmse, color = 'grey80') +
    geom_sf(data = dplyr::filter(model_rmse, n_obs_train >0),  aes(color = log10(n_obs_train)), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    ggtitle('Observations Train / Segment')


  ggplot(model_rmse, aes(x = n_obs_train + .1, y = rmse, color = model, group = model)) +
    geom_jitter(size = 4) +
    scale_x_log10() +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    geom_smooth(method = 'loess', se = F) +
    ylab('RMSE (C)') + xlab('# of Training Obs')

  ggplot(compare_to_sntemp, aes(x = n_obs_train + .1, y = delta_rmse_sntemp, color = model, group = model)) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =2, color = 'grey') +
    geom_jitter(size = 4) +
    scale_x_log10() +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ylab('Model RMSE - SNTemp RMSE (C)') + xlab('# of Training Obs')

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
                             uncal_sntemp_file,
                             model_fabric_file,
                             gd_config = 'lib/cfg/gd_config.yml'){

  model_output = feather::read_feather(model_output_file)

  seg_ids = unique(model_output$seg_id_nat)

  obs = read.csv(obs_file) %>% as_tibble() %>%
    mutate(seg_id_nat = as.character(seg_id_nat),
           date = as.Date(date))

  uncal_sntemp = feather::read_feather(uncal_sntemp_file) %>%
    dplyr::filter(seg_id_nat %in% seg_ids,
                  date >= as.Date('2004-10-01'),
                  date <= as.Date('2016-09-30'))

  uncal_sntemp_flow = uncal_sntemp %>%
    rename(stream_flow_m3_s = seg_outflow) %>%
    mutate(model = 'uncal_sntemp') %>%
    select(seg_id_nat, date, stream_flow_m3_s, model)

  model_output = bind_rows(model_output, uncal_sntemp_flow)

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
              mean_flow = mean(discharge_cms, na.rm = T),
              cv_rmse = rmse/mean_flow) %>%
    ungroup() %>%
    left_join(train_obs, by = 'seg_id_nat') %>%
    mutate(n_obs_train = ifelse(is.na(n_obs_train), 0, n_obs_train))

  model_rmse = left_join(model_fabric, model_rmse, by = 'seg_id_nat') %>%
    dplyr::filter(seg_id_nat %in% seg_ids)

  lowest_rmse = model_rmse %>%
    group_by(seg_id_nat) %>%
    dplyr::filter(rmse == min(rmse)) %>%
    ungroup()

  compare_to_sntemp = left_join(dplyr::filter(model_rmse, model != 'uncal_sntemp'),
                                dplyr::filter(model_rmse, model =='uncal_sntemp') %>%
                                  select(seg_id_nat, rmse) %>% tibble(),
                                by = c('seg_id_nat'), suffix = c('', '_sntemp')) %>%
    select(-geometry_sntemp) %>%
    mutate(delta_rmse_sntemp = rmse - rmse_sntemp)


  ggplot() +
    geom_sf(data = model_rmse, color= 'grey80' ) +
    geom_sf(data = dplyr::filter(model_rmse, !is.na(rmse)), aes(color = rmse), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Flow RMSE (m3 sec-1)')


  ggplot() +
    geom_sf(data = compare_to_sntemp, color = 'grey80') +
    geom_sf(data = dplyr::filter(compare_to_sntemp, !is.na(delta_rmse_sntemp)),
            aes(color = delta_rmse_sntemp), size = 2) +
    scale_color_gradient2(low = 'blue', high = 'red')+
    theme_minimal()+
    facet_wrap(~model)+
    ggtitle('Model RMSE - SNTemp RMSE (m3 sec-1)')

  ggplot() +
    geom_sf(data = model_rmse, color= 'grey80' ) +
    geom_sf(data = dplyr::filter(model_rmse, n_obs_test >0), aes(color = n_obs_test), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    ggtitle('Observations Test / Segment')

  ggplot() +
    geom_sf(data = model_rmse, color= 'grey80' ) +
    geom_sf(data = dplyr::filter(model_rmse, n_obs_train >0), aes(color = n_obs_train), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()+
    ggtitle('Observations Train / Segment')

  ggplot() +
    geom_sf(data = model_rmse, color= 'grey80', size =2 , ) +
    scale_color_viridis_c(direction = -1) +
    geom_sf_label(data = model_rmse, aes(label = seg_id_nat)) +
    theme_minimal()

  ggplot() +
    geom_sf(data = model_rmse, color= 'grey80' ) +
    geom_sf(data = dplyr::filter(model_rmse, mean_flow >0), aes(color = mean_flow), size = 2) +
    scale_color_viridis_c(direction = -1) +
    theme_minimal()

  ggplot(model_rmse, aes(x = n_obs_train, y = rmse, color = model, group = model)) +
    geom_point(size = 4) +
    # scale_x_log10() +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    geom_smooth(method = 'loess', se = F) +
    ylab('RMSE (m3 sec-1)') + xlab('# of Training Obs')

  ggplot(compare_to_sntemp, aes(x = n_obs_train, y = delta_rmse_sntemp, color = model, group = model)) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =2, color = 'grey') +
    geom_jitter(size = 4) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ylab('Model RMSE - SNTemp RMSE (m3 sec-1)') + xlab('# of Training Obs')

  ggplot(model_rmse, aes(x = mean_flow, y = rmse, color = model, group = model)) +
    geom_point(size = 4) +
    # scale_x_log10() +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    geom_smooth(method = 'lm', se = F) +
    ylab('RMSE (m3 sec-1)') + xlab('Mean flow (m3 sec-1)')

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
    ylab('RMSE (m3 sec-1)') + xlab('# of Training Obs')

  ggplot(lowest_rmse, aes(x = mean_flow, y = rmse, color = model, group = model)) +
    geom_jitter(size = 4) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ylab('RMSE (m3 sec-1)') + xlab('Mean Flow (m3 sec-1)')


  ggplot(compare_to_sntemp, aes(x = mean_flow, y = delta_rmse_sntemp, color = model, group = model)) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =2, color = 'grey') +
    geom_jitter(size = 4) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ylab('Model RMSE - SNTemp RMSE (m3 sec-1)') + xlab('Mean Flow (m3 sec-1)')

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
