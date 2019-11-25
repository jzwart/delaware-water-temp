
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
get_obs_id_matrix = function(n_states, n_params_obs, n_params_est, n_step, obs){

  # dimensions will be n_depths by Y vector length (or n_depths + n_params)
  H = array(0, dim=c(n_states + n_params_obs, n_states + n_params_est, n_step))

  # order goes 1) states, 2)params for which we have obs, 3) params for which we're estimating but don't have obs

  for(t in 1:n_step){
    H[1:(n_states + n_params_obs), 1:(n_states + n_params_obs), t] = diag(ifelse(is.na(obs[,,t]),0, 1), n_states + n_params_obs, n_states + n_params_obs)
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
                        n_states_est, n_params_est,
                        n_params_obs, n_step, n_en,
                        state_sd, param_sd){

  # initializing states with earliest observations and parameters
  # first_obs = coalesce(!!!lapply(seq_len(dim(obs)[3]), function(i){obs[,,i]})) %>% # turning array into list, then using coalesce to find first obs in each position.
  #   ifelse(is.na(.), mean(., na.rm = T), .) # setting initial temp state to mean of earliest temp obs from other sites if no obs

  # initializing states with end of spinup from SNTemp
  first_states = init_states %>%
    arrange(seg_id_nat) %>% pull(water_temp) # always arrange by seg_id_nat

  if(n_params_est > 0){
    param_names = colnames(init_params)[3:ncol(init_params)]
    first_params = c()
    for(i in 1:length(param_names)){
      cur_param = init_params %>%
        arrange(seg_id_nat) %>% pull(2+i)
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
                model_locations,
                obs_file = '3_observations/in/obs_temp_full.rds',
                init_param_file = '2_3_model_parameters/out/init_params.rds', # create initial parameter file
                model_run_loc = '4_model/tmp',
                orig_model_loc = '20191002_Delaware_streamtemp',
                driver_file = NULL,
                n_states_est = 456,
                n_states_obs = 303,
                n_params_est = 2,
                n_param_loc = 456,
                n_params_obs = 0,
                obs_cv = 0.1,
                param_cv = 0.2,
                driver_cv = 0.1,
                init_cond_cv = 0.1,
                gd_config = 'lib/cfg/gd_config.yml'){

  # temporary dates for testing
  start = '2014-06-01'
  stop = '2014-07-01'

  # copy over original run files to temporary file location
  dir.create(model_run_loc, showWarnings = F)
  print('Copying original model files to model working directory...')
  files_to_transfer = list.files(orig_model_loc)
  file.copy(from = file.path(orig_model_loc, files_to_transfer), to = model_run_loc, overwrite = T, recursive = T)

  # get model start, stop, full dates, and n_steps
  n_en = as.numeric(n_en)
  start = as.Date(as.character(start))
  stop = as.Date(as.character(stop))
  dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
  n_step = length(dates)

  state_sd = rep(obs_cv * 5, n_states_est)

  # get observation matrix
  obs_df = readRDS(obs_file)

  # get initial parameters
  init_params_df = readRDS(init_param_file)
  n_params_est = (ncol(init_params_df) - 2) * nrow(init_params_df) # columns 1 & 2 are model locations

  param_sd = mutate_at(init_params_df, 3:ncol(init_params_df), as.numeric) %>%
    gather(key = 'param', value = 'param_sd', contains('tau')) %>%
    mutate(param_sd = param_sd * param_cv) %>%
    pull(param_sd)

  # use this to organize the matrices
  model_fabric = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp')

  model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                           model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  # setting up matrices
  # observations as matrix
  # organized by seg_id_nat
  print('setting up EnKF matrices...')
  obs = get_obs_matrix(obs_df = obs_df,
                       model_dates = dates,
                       model_locations = model_locations$seg_id_nat,
                       n_step = n_step,
                       n_states = n_states_est)

  # Y vector for storing state / param estimates and updates
  Y = get_Y_vector(n_states = n_states_est,
                   n_params_est = n_params_est,
                   n_step = n_step,
                   n_en = n_en)

  # observation error matrix
  R = get_obs_error_matrix(n_states = n_states_est,
                           n_params_obs = n_params_obs,
                           n_step = n_step,
                           state_sd = state_sd,
                           param_sd = param_sd)

  # observation identity matrix
  H = get_obs_id_matrix(n_states = n_states_est,
                        n_params_obs = n_params_obs,
                        n_params_est = n_params_est,
                        n_step = n_step,
                        obs = obs)

  # do spinup period here and then initialize Y vector
  for(n in 1:n_en){
    run_sntemp(start = dates[1], stop = dates[1], spinup = T,
               model_run_loc = model_run_loc,
               spinup_days = 730,
               restart = T,
               precip_file = sprintf('./input/prcp_%s.cbh', n),
               tmax_file = sprintf('./input/tmax_%s.cbh', n),
               tmin_file = sprintf('./input/tmin_%s.cbh', n),
               var_init_file = sprintf('prms_ic_%s.out', n),
               var_save_file = sprintf('prms_ic_%s.out', n))
  }

  stream_temp_init = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                           model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp')) %>%
    dplyr::filter(date == dates[1])

  # initialize Y vector
  print('intializing Y vector...')
  Y = initialize_Y(Y = Y,
                   init_states = stream_temp_init,
                   init_params = init_params_df,
                   n_states_est = n_states_est,
                   n_params_est = n_params_est,
                   n_params_obs = n_params_obs,
                   n_step = n_step, n_en = n_en, state_sd = state_sd, param_sd = param_sd)

  param_names = colnames(init_params_df)[3:ncol(init_params_df)]

  # start modeling
  for(t in 2:n_step){
    for(n in 1:n_en){
      # set parameters / states for model config
      updated_params = get_updated_params(Y = Y,
                                          param_names = param_names,
                                          n_states_est = n_states_est,
                                          n_params_est = n_params_est,
                                          cur_step = t-1,
                                          en = n)

      update_sntemp_params(param_names = param_names,
                           updated_params = updated_params)
      # model_config = Y[, t-1, n]

      # run model
      run_sntemp(start = dates[t],
                 stop = dates[t],
                 spinup = F,
                 restart = T,
                 precip_file = sprintf('./input/prcp_%s.cbh', n),
                 tmax_file = sprintf('./input/tmax_%s.cbh', n),
                 tmin_file = sprintf('./input/tmin_%s.cbh', n),
                 var_init_file = sprintf('prms_ic_%s.out', n),
                 var_save_file = sprintf('prms_ic_%s.out', n))

      model_output = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                                model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp')) %>%
        dplyr::filter(date == dates[t])

      Y[ , t, n] = c(model_output$water_temp, updated_params) # store in Y vector
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
    # update states / params for model config
  }

  out = list(Y = Y, dates = dates, obs = obs, R = R, model_locations = model_locations)

  saveRDS(object = out, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}

obs[,1,1]
site = 29
plot(Y[site,,1], type = 'l',ylim =  range(c(Y[site,,], obs[site,1,]), na.rm = T))
for(i in 1:n_en){
  lines(Y[site,,i])
}
points(obs[site,1,], col = 'red')
arrows(1:n_step, obs[site,1,]+R[site,site,], 1:n_step, obs[site,1,]-R[site,site,],
       angle = 90, length = .1, col = 'red', code = 3)

params = 456 + site
windows()
plot(Y[params,,1], type = 'l', ylim = range(Y[params,,]))
for(i in 1:n_en){
  lines(Y[params,,i])
}


