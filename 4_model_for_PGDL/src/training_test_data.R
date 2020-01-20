
#' @param ind_file indicator file
#' @param data_file SNTemp output data file
#' @param percent_sites how many stream segments to use for training
#' @param temporal_res temporal resolution of the sampling at each site (days)
#' @param obs_error observation error added onto the SNTemp output
#' @param obs_type is the observation from in-situ or remote sensing? RS obs occur on the same day
#' @param exp_n number of experiments using this scheme
#' @param test_yrs how many years at the end of time series to leave as testing only
build_synthetic_training <- function(ind_file,
                                     data_file,
                                     percent_sites,
                                     temporal_res,
                                     obs_error,
                                     obs_type = 'in_situ',
                                     exp_n,
                                     test_yrs,
                                     gd_config = 'lib/cfg/gd_config.yml'){

  all_data = feather::read_feather(data_file)

  dates = all_data %>% pull(date) %>% unique

  segs = all_data %>% pull(seg_id_nat) %>% unique

  n_dates = length(dates)
  n_segs_obs = round(length(segs) * percent_sites / 100)

  exps = 1:exp_n

  if(obs_type == 'in_situ'){
    for(i in exps){
      set.seed(42 + as.numeric(i))

      segs_observed = sample(x = segs, size = n_segs_obs, replace = F)

      cur_train_test = all_data %>%
        mutate(train_test = 'ignore') %>%
        group_by(seg_id_nat) %>%
        # randomize starting date of sampling
        mutate(train_test = case_when(date %in% dates[seq(sample(1:temporal_res,1), n_dates, by = temporal_res)] &
                                        seg_id_nat %in% segs_observed ~ 'train',
                                      TRUE ~ 'ignore')) %>%
        ungroup() %>%
        mutate(train_test = case_when(date > (max(all_data$date) - as.numeric(test_yrs)*365) ~ 'test',
                                      TRUE ~ train_test)) %>%
        pull(train_test)

      col_name = paste0('exp',i) %>% noquote()
      all_data = mutate(all_data, !!col_name := cur_train_test) %>%
        select(seg_id_nat, date, starts_with('exp')) # only keeping id, date, and experiments to cut down on redundant data
    }
  }else if(obs_type == 'rs'){
    for(i in exps){
      set.seed(42 + as.numeric(i))

      segs_observed = sample(x = segs, size = n_segs_obs, replace = F)
      dates_observed = dates[seq(sample(1:temporal_res,1), n_dates, by = temporal_res)]

      cur_train_test = all_data %>%
        mutate(train_test = 'ignore') %>%
        group_by(seg_id_nat) %>%
        # randomize starting date of sampling
        mutate(train_test = case_when(date %in% dates_observed &
                                        seg_id_nat %in% segs_observed ~ 'train',
                                      TRUE ~ 'ignore')) %>%
        ungroup() %>%
        mutate(train_test = case_when(date > (max(all_data$date) - as.numeric(test_yrs)*365) ~ 'test',
                                      TRUE ~ train_test)) %>%
        pull(train_test)

      col_name = paste0('exp',i) %>% noquote()
      all_data = mutate(all_data, !!col_name := cur_train_test) %>%
        select(seg_id_nat, date, starts_with('exp')) # only keeping id, date, and experiments to cut down on redundant data
    }
  }


  out_file = as_data_file(ind_file)
  feather::write_feather(x = all_data, path = out_file)
  gd_put(remote_ind = ind_file, local_source = out_file, config_file = gd_config)
}


#' @param ind_file indicator file
#' @param data_file SNTemp output file
#' @param obs_file Observation file
#' @param percent_obs percent of observations used for training
#' @param exp_n number of experiments using this scheme
#' @param test_yrs how many years at the end of time series to leave as testing only
build_real_training <- function(ind_file,
                                data_file,
                                obs_file,
                                percent_obs,
                                exp_n,
                                test_yrs,
                                gd_config = 'lib/cfg/gd_config.yml'){

  data = feather::read_feather(data_file)
  obs = readRDS(obs_file)

  # filter observations to SNTemp model run time period
  obs = obs %>% dplyr::filter(date >= min(data$date) & date <= max(data$date))

  obs_train = obs %>% dplyr::filter(date < (max(data$date) - as.numeric(test_yrs)*365))
  obs_test = dplyr::filter(obs, !date %in% obs_train$date)

  n_obs = nrow(obs_train)

  exps = 1:exp_n

  n_obs_train = round(n_obs * as.numeric(percent_obs) / 100)

  for(i in exps){
    set.seed(42 + as.numeric(i))

    train_loc = sample(1:n_obs, size = n_obs_train, replace = F)

    cur_train_test = rep('ignore', nrow(obs_train))
    cur_train_test[train_loc] = 'train'

    col_name = paste0('exp',i) %>% noquote()
    obs_train = mutate(obs_train, !!col_name := cur_train_test)
    obs_test = mutate(obs_test, !!col_name := rep('test', nrow(obs_test)))
  }

  obs_out = bind_rows(obs_train, obs_test)

  out_file = as_data_file(ind_file)
  feather::write_feather(x = obs_out, path = out_file)
  gd_put(remote_ind = ind_file, local_source = out_file, config_file = gd_config)
}


