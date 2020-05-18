

subbasin_options = function(ind_file,
                            subbasin_file,
                            uncal_sntemp_pred_file,
                            obs_file,
                            gd_config = 'lib/cfg/gd_config.yml'){

  data_list = readRDS(subbasin_file)

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

    obs_per_basin$subbasin_obs_train[obs_per_basin$subbasin_outlet == cur] = sum(cur_obs$date < as.Date('2004-10-02') & cur_obs$date > as.Date('1980-01-01'))
    obs_per_basin$subbasin_obs_test[obs_per_basin$subbasin_outlet == cur] = sum(cur_obs$date >= as.Date('2004-10-02'))
  }

  all_obs = readRDS(obs_file) %>%
    group_by(seg_id_nat) %>%
    summarise(n_obs = n()) %>%
    ungroup()

  data = left_join(data, obs_per_basin, by = 'subbasin_outlet') %>%
    mutate(seg_id_nat = as.character(seg_id_nat)) %>%
    left_join(all_obs, by = 'seg_id_nat')

  # pull out subbasin outlets
  outlets = dplyr::filter(data, seg_id_nat == subbasin_outlet)

  data = data %>%
    group_by(subbasin_outlet) %>%
    mutate(subbasin_n_segs = n(),
           subbasin_obs_train_per_seg = round(obs_train / (subbasin_n_segs))) %>%
    ungroup()

  uncal_preds = feather::read_feather(uncal_sntemp_pred_file) %>%
    select(seg_id_nat, date, seg_tave_water)
  obs = readRDS(obs_file)

  compare = left_join(uncal_preds, select(obs, seg_id_nat, date,temp_C), by = c('seg_id_nat', 'date')) %>%
    left_join(select(as_tibble(data), seg_id_nat, subbasin_outlet)) %>%
    group_by(subbasin_outlet) %>%
    summarise(subbasin_uncal_sntemp_rmse = rmse(temp_C, seg_tave_water)) %>%
    ungroup()

  data_out = left_join(select(as_tibble(data), -region, -geometry), compare, by = 'subbasin_outlet')

  out_file = as_data_file(ind_file)
  feather::write_feather(x = data_out, path = out_file)
  gd_put(remote_ind = ind_file, local_source = out_file, config_file = gd_config)
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

