
#' retreive the model time steps based on start and stop dates and time step
#'
#' @param model_start model start date in date class
#' @param model_stop model stop date in date class
#' @param time_step model time step, defaults to daily timestep
get_model_dates = function(model_start, model_stop, time_step = 'days'){

  model_dates = seq.Date(from = as.Date(model_start), to = as.Date(model_stop), by = time_step)

  return(model_dates)
}

#' vector for holding states and parameters for updating
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_params_est number of parameters we're calibrating
#' @param n_step number of model timesteps
#' @param n_en number of ensembles
get_Y_vector = function(n_states, n_params_est, n_step, n_en){

  Y = array(dim = c(n_states + n_params_est, n_step, n_en))

  return(Y)
}

#' observation error matrix, should be a square matrix where
#'   col & row = the number of states and params for which you have observations
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_param_obs number of parameters for which we have observations
#' @param n_step number of model timesteps
#' @param state_sd vector of state observation standard deviation; assuming sd is constant through time
#' @param param_sd vector of parmaeter observation standard deviation; assuming sd is constant through time
get_obs_error_matrix = function(n_states, n_params_obs, n_step, state_sd, param_sd){

  R = array(0, dim = c(n_states + n_params_obs, n_states + n_params_obs, n_step))

  state_var = state_sd^2 #variance of temperature observations

  param_var = param_sd^2

  if(n_params_obs > 0){
    all_var = c(state_var, param_var)
  }else{
    all_var = state_var
  }

  for(i in 1:n_step){
    # variance is the same for each depth and time step; could make dynamic or varying by time step if we have good reason to do so
    R[,,i] = diag(all_var, n_states + n_params_obs, n_states + n_params_obs)
  }

  return(R)
}

#' Measurement operator matrix saying 1 if there is observation data available, 0 otherwise
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_param_obs number of parameters for which we have observations
#' @param n_params_est number of parameters we're calibrating
#' @param n_step number of model timesteps
#' @param obs observation matrix created with get_obs_matrix function
get_obs_id_matrix = function(n_states_obs, n_states_est, n_params_obs, n_params_est, n_step, obs){

  H = array(0, dim=c(n_states_obs + n_params_obs, n_states_est + n_params_est, n_step))

  # order goes 1) states, 2)params for which we have obs, 3) params for which we're estimating but don't have obs

  for(t in 1:n_step){
    H[1:(n_states_obs + n_params_obs), 1:(n_states_obs + n_params_obs), t] = diag(ifelse(is.na(obs[,,t]),0, 1), n_states_obs + n_params_obs, n_states_obs + n_params_obs)
  }

  return(H)
}


#' turn observation dataframe into matrix
#'
#' @param obs_df observation data frame
#' @param model_dates dates over which you're modeling
#' @param model_locations locations where you're estimating temperature
#' @param n_step number of model time steps
#' @param n_states number of states we're updating in data assimilation routine
get_obs_matrix = function(obs_df, model_dates, model_locations, n_step, n_states){

  # need to know location and time of observation
   # model_locations is arranged by model_idx

  obs_df_filtered = obs_df %>%
    dplyr::filter(seg_id_nat %in% model_locations,
                  date %in% model_dates) %>%
    select(seg_id_nat, date, temp_C) %>%
    group_by(seg_id_nat) %>%
    mutate(site_row = which(model_locations %in% seg_id_nat),  # getting which row in Y vector corresponds to site location
           date_step = which(model_dates %in% date)) %>%
    ungroup()

  obs_matrix = array(NA, dim = c(n_states, 1, n_step))

  for(i in 1:length(model_locations)){
    cur_site = dplyr::filter(obs_df_filtered, site_row == i)
    if(nrow(cur_site) > 0){
      for(j in cur_site$date_step){
        obs_matrix[i, 1, j] = dplyr::filter(obs_df_filtered,
                                            site_row == i,
                                            date_step == j) %>%
          pull(temp_C)
      }
    }else{
      next
    }
  }

  return(obs_matrix)
}



##' @param Y vector for holding states and parameters you're estimating
##' @param R observation error matrix
##' @param obs observations at current timestep
##' @param H observation identity matrix
##' @param n_en number of ensembles
##' @param cur_step current model timestep
kalman_filter = function(Y, R, obs, H, n_en, cur_step){

  cur_obs = obs[ , , cur_step]

  cur_obs = ifelse(is.na(cur_obs), 0, cur_obs) # setting NA's to zero so there is no 'error' when compared to estimated states

  ###### estimate the spread of your ensembles #####
  Y_mean = matrix(apply(Y[ , cur_step, ], MARGIN = 1, FUN = mean), nrow = length(Y[ , 1, 1])) # calculating the mean of each temp and parameter estimate
  delta_Y = Y[ , cur_step, ] - matrix(rep(Y_mean, n_en), nrow = length(Y[ , 1, 1])) # difference in ensemble state/parameter and mean of all ensemble states/parameters

  ###### estimate Kalman gain #########
  K = ((1 / (n_en - 1)) * delta_Y %*% t(delta_Y) %*% t(H[, , cur_step])) %*%
    qr.solve(((1 / (n_en - 1)) * H[, , cur_step] %*% delta_Y %*% t(delta_Y) %*% t(H[, , cur_step]) + R[, , cur_step]))

  ###### update Y vector ######
  for(q in 1:n_en){
    Y[, cur_step, q] = Y[, cur_step, q] + K %*% (cur_obs - H[, , cur_step] %*% Y[, cur_step, q]) # adjusting each ensemble using kalman gain and observations
  }
  return(Y)
}



#' initialize Y vector with draws from distribution of obs
#'
#' @param Y Y vector
#' @param obs observation matrix
initialize_Y = function(Y, init_states, init_params,
                        n_states_est, n_states_obs, n_params_est,
                        n_params_obs, n_step, n_en,
                        state_sd, param_sd){

  # initializing states with end of spinup from SNTemp ic files (or obs if available)
  if(n_states_est > 0){
    state_names = colnames(init_states)[3:ncol(init_states)]
    first_states = c()
    for(i in 1:length(state_names)){
      cur_state = init_states %>%
        arrange(as.numeric(model_idx)) %>% pull(2+i)
      first_states = c(first_states, cur_state)
    }
    first_states = as.numeric(first_states)
  }else{
    first_states = NULL
  }

  if(n_params_est > 0){
    param_names = colnames(init_params)[3:ncol(init_params)]
    first_params = c()
    for(i in 1:length(param_names)){
      cur_param = init_params %>%
        arrange(as.numeric(model_idx)) %>% pull(2+i)
      first_params = c(first_params, cur_param)
    }
    first_params = as.numeric(first_params)

  }else{
    first_params = NULL
  }

  Y[ , 1, ] = array(rnorm(n = n_en * (n_states_est + n_params_est),
                          mean = c(first_states, first_params),
                          sd = c(state_sd, param_sd)),
                    dim = c(c(n_states_est + n_params_est), n_en))

  return(Y)
}

get_updated_params = function(Y, param_names, n_states_est, n_params_est, cur_step, en){

  updated_params = Y[(n_states_est+1):(n_states_est+n_params_est), cur_step, en]

  return(updated_params)
}

get_updated_states = function(Y, state_names, n_states_est, n_params_est, cur_step, en){

  updated_states = Y[1:n_states_est, cur_step, en]

  return(updated_states)
}

gather_states = function(ic_out){

  state_names = colnames(ic_out)[3:ncol(ic_out)]
  states = c()
  for(i in 1:length(state_names)){
    cur_state = ic_out %>%
      arrange(as.numeric(model_idx)) %>% pull(2+i)
    states = c(states, cur_state)
  }
  states = as.numeric(states)

  return(states)
}


model_spinup = function(n_en,
                        start,
                        stop,
                        time_step = 'days',
                        model_run_loc = '4_model/tmp',
                        spinup_days = 730){

  n_en = as.numeric(n_en)
  spinup_days = as.numeric(spinup_days)
  start = as.Date(as.character(start))
  stop = as.Date(as.character(stop))
  dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)

  for(n in 1:n_en){
    run_sntemp(start = dates[1], stop = dates[1], spinup = T,
               model_run_loc = model_run_loc,
               spinup_days = spinup_days,
               restart = T,
               precip_file = sprintf('./input/prcp_%s.cbh', n),
               tmax_file = sprintf('./input/tmax_%s.cbh', n),
               tmin_file = sprintf('./input/tmin_%s.cbh', n),
               var_init_file = sprintf('prms_ic_spinup_%s.txt', n),
               var_save_file = sprintf('prms_ic_spinup_%s.txt', n))
  }
}


#' wrapper function for running EnKF for given model
#'
#' @param n_en number of model ensembles
#' @param start start date of model run
#' @param stop date of model run
#' @param time_step model time step, defaults to days
#' @param obs_file observation file
#' @param driver_file driver data file
#' @param n_states_est number of states we're estimating
#' @param n_params_est number of parameters we're estimating
#' @param n_params_obs number of parameters for which we have observations
#' @param obs_cv coefficient of variation of observations
#' @param param_cv coefficient of variation of parameters
#' @param driver_cv coefficient of variation of driver data
#' @param init_cond_cv initial condition CV (what we're )
#' @param gd_config google drive configuration
EnKF = function(ind_file,
                n_en = 20,
                start,
                stop,
                time_step = 'days',
                #process_model = 'random_walk',
                model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp',
                obs_file = '3_observations/in/obs_temp_full.rds',
                init_param_file = '2_3_model_parameters/out/init_params.rds',
                model_run_loc = '4_model/tmp',
                orig_model_loc = '20191002_Delaware_streamtemp',
                state_names,
                driver_file = NULL,
                n_states_est = 456,
                n_states_obs = 456,
                n_params_est = 2,
                n_param_loc = 456,
                n_params_obs = 0,
                obs_cv = 0.1,
                param_cv = 0.2,
                driver_cv = 0.1,
                init_cond_cv = 0.1,
                assimilate_obs = TRUE,
                gd_config = 'lib/cfg/gd_config.yml'){

  # copy over original run files to temporary file location
  dir.create(model_run_loc, showWarnings = F)
  print('Copying original model files to model working directory...')
  files_to_transfer = list.files(orig_model_loc)
  file.copy(from = file.path(orig_model_loc, files_to_transfer), to = model_run_loc, overwrite = T, recursive = T)

  # use this to organize the matrices
  model_fabric = sf::read_sf(model_fabric_file)

  # arrange by model_idx
  model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                           model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  # get model start, stop, full dates, and n_steps
  n_en = as.numeric(n_en)
  start = as.Date(as.character(start))
  stop = as.Date(as.character(stop))
  dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
  n_step = length(dates)

  state_names = state_names$states_to_update
  n_states_est = length(state_names) * nrow(model_locations)
  n_states_obs = nrow(model_locations) # only assimilating temperature obs
  state_sd = rep(obs_cv * 5, n_states_est)  # UPDATE THIS #########

  # get observation matrix
  obs_df = readRDS(obs_file)

  # get initial parameters; arrange by model_idx
  init_params_df = readRDS(init_param_file) %>% arrange(as.numeric(model_idx))
  n_params_est = (ncol(init_params_df) - 2) * nrow(init_params_df) # columns 1 & 2 are model locations

  if(n_params_est > 0){
    param_names = colnames(init_params_df)[3:ncol(init_params_df)]

    param_sd = mutate_at(init_params_df, 3:ncol(init_params_df), as.numeric) %>%
      gather(key = 'param', value = 'param_sd', -seg_id_nat, -model_idx) %>%
      mutate(param_sd = ifelse(param_sd < 2, 2, param_sd),
             param_sd = param_sd * param_cv) %>%
      pull(param_sd)
  }else{
    param_names = NULL
    param_sd = NULL
  }


  # setting up matrices
  # observations as matrix
  # organized by seg_id_nat
  print('setting up EnKF matrices...')
  obs = get_obs_matrix(obs_df = obs_df,
                       model_dates = dates,
                       model_locations = model_locations$seg_id_nat,
                       n_step = n_step,
                       n_states = n_states_obs)

  # Y vector for storing state / param estimates and updates
  Y = get_Y_vector(n_states = n_states_est,
                   n_params_est = n_params_est,
                   n_step = n_step,
                   n_en = n_en)

  # observation error matrix
  R = get_obs_error_matrix(n_states = n_states_obs,
                           n_params_obs = n_params_obs,
                           n_step = n_step,
                           state_sd = state_sd,
                           param_sd = param_sd)

  # observation identity matrix
  H = get_obs_id_matrix(n_states_obs = n_states_obs,
                        n_states_est = n_states_est,
                        n_params_obs = n_params_obs,
                        n_params_est = n_params_est,
                        n_step = n_step,
                        obs = obs)

  # for initial states, we want to use the observation for the first time step, if avaialble, otherwise use
  #  the mean of the ensembles' initial conditions.

  init_states = model_locations
  for(n in 1:n_en){
    cur_ic = get_sntemp_initial_states(state_names = state_names,
                                       model_run_loc = model_run_loc,
                                       ic_file = sprintf('prms_ic_spinup_%s.txt', n))

    init_states = dplyr::left_join(init_states, cur_ic, by = c('seg_id_nat', 'model_idx'), suffix = c('',n))
  }
  init_states_median = model_locations
  for(i in 1:length(state_names)){
    cur_state = init_states %>% select(c(1, 2, starts_with(state_names[i]))) %>%
      gather(key = 'state', value = 'value', starts_with(state_names[i])) %>%
      group_by(seg_id_nat, model_idx) %>%
      summarise(!!noquote(state_names[i]) := median(value)) %>%
      ungroup() %>% arrange(as.numeric(model_idx))

    init_states_median = dplyr::left_join(init_states_median, cur_state, by = c('seg_id_nat', 'model_idx'))
  }
  rm(init_states)

  # changing initial temperature state to obs if obs is available (since everything is ordered by model_idx, just use ifelse() function)
  init_states_median$seg_tave_water = ifelse(!is.na(obs[,1,1]), obs[,1,1], init_states_median$seg_tave_water)

  state_sd = mutate_at(init_states_median, 3:ncol(init_states_median), as.numeric) %>%
    gather(key = 'state', value = 'state_sd', -seg_id_nat, -model_idx) %>%
    mutate(state_sd = ifelse(state_sd < 2, 2, state_sd),
           state_sd = state_sd * init_cond_cv) %>%
    pull(state_sd)

  # initialize Y vector
  print('intializing Y vector...')
  Y = initialize_Y(Y = Y,
                   init_states = init_states_median,
                   init_params = init_params_df,
                   n_states_est = n_states_est,
                   n_states_obs = n_states_obs,
                   n_params_est = n_params_est,
                   n_params_obs = n_params_obs,
                   n_step = n_step, n_en = n_en, state_sd = state_sd, param_sd = param_sd)


  # start modeling
  if(assimilate_obs){
    for(t in 2:n_step){
      for(n in 1:n_en){
        # set parameters / states for model config
        if(n_params_est>0){
          updated_params = get_updated_params(Y = Y,
                                              param_names = param_names,
                                              n_states_est = n_states_est,
                                              n_params_est = n_params_est,
                                              cur_step = t-1,
                                              en = n)
          update_sntemp_params(param_names = param_names,
                               updated_params = updated_params)
        }

        updated_states = get_updated_states(Y = Y,
                                            state_names = state_names,
                                            n_states_est = n_states_est,
                                            n_params_est = n_params_est,
                                            cur_step = t-1,
                                            en = n)

        if(t==2){
          update_sntemp_states(state_names = state_names,
                               updated_states = updated_states,
                               ic_file_in = sprintf('prms_ic_spinup_%s.txt', n),
                               ic_file_out = sprintf('prms_ic_%s.txt', n))
        }else{
          update_sntemp_states(state_names = state_names,
                               updated_states = updated_states,
                               ic_file_in = sprintf('prms_ic_%s.txt', n),
                               ic_file_out = sprintf('prms_ic_%s.txt', n))
        }


        # run model
        run_sntemp(start = dates[t],
                   stop = dates[t],
                   spinup = F,
                   restart = T,
                   precip_file = sprintf('./input/prcp_%s.cbh', n),
                   tmax_file = sprintf('./input/tmax_%s.cbh', n),
                   tmin_file = sprintf('./input/tmin_%s.cbh', n),
                   var_init_file = sprintf('prms_ic_%s.txt', n),
                   var_save_file = sprintf('prms_ic_%s.txt', n))


        ic_out = get_sntemp_initial_states(state_names = state_names,
                                           model_run_loc = model_run_loc,
                                           ic_file = sprintf('prms_ic_%s.txt', n))
        predicted_states = gather_states(ic_out) # predicted states from model

        if(n_params_est > 0){
          Y[ , t, n] = c(predicted_states, updated_params) # store in Y vector
        }else{
          Y[ , t, n] = predicted_states # only updating states, not params
        }
      }
      if(any(H[,,t]==1)){
        print('updating with Kalman Filter...')
        Y = kalman_filter(Y = Y,
                          R = R,
                          obs = obs,
                          H = H,
                          n_en = n_en,
                          cur_step = t) # updating params / states if obs available
      }
    }
  }else{
    for(n in 1:n_en){
      # set parameters / states for model config
      if(n_params_est>0){
        updated_params = get_updated_params(Y = Y,
                                            param_names = param_names,
                                            n_states_est = n_states_est,
                                            n_params_est = n_params_est,
                                            cur_step = t-1,
                                            en = n)
        update_sntemp_params(param_names = param_names,
                             updated_params = updated_params)
      }
      updated_states = get_updated_states(Y = Y,
                                          state_names = state_names,
                                          n_states_est = n_states_est,
                                          n_params_est = n_params_est,
                                          cur_step = 1,
                                          en = n)

      if(t==2){
        update_sntemp_states(state_names = state_names,
                             updated_states = updated_states,
                             ic_file_in = sprintf('prms_ic_spinup_%s.txt', n),
                             ic_file_out = sprintf('prms_ic_%s.txt', n))
      }else{
        update_sntemp_states(state_names = state_names,
                             updated_states = updated_states,
                             ic_file_in = sprintf('prms_ic_%s.txt', n),
                             ic_file_out = sprintf('prms_ic_%s.txt', n))
      }

      # run model
      run_sntemp(start = dates[2],
                 stop = dates[n_step],
                 spinup = F,
                 restart = T,
                 precip_file = sprintf('./input/prcp_%s.cbh', n),
                 tmax_file = sprintf('./input/tmax_%s.cbh', n),
                 tmin_file = sprintf('./input/tmin_%s.cbh', n),
                 var_init_file = sprintf('prms_ic_%s.txt', n),
                 var_save_file = sprintf('prms_ic_%s.txt', n))

      model_output = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                            model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp')) %>%
        dplyr::filter(date %in% dates[2:n_step]) %>%
        arrange(date, as.numeric(model_idx))

      out_temp = array(model_output$water_temp, dim = c(nrow(model_locations), n_step-1))

      Y[1:nrow(model_locations), 2:n_step, n] = out_temp # store temp in Y vector
      if(n_params_est > 0){
        out_params = array(rep(updated_params, (n_step-1)), dim = c(n_params_est, n_step-1))
        Y[(n_states_est+1):(n_states_est+n_params_est), 2:n_step, n] = out_params # store params in Y vector
      }
    }
  }

  out = list(Y = Y, dates = dates, obs = obs, R = R, model_locations = model_locations)

  saveRDS(object = out, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}



