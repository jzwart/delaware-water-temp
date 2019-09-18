
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

  state_sd = state_sd # estimating temperature observation accruacy based on https://www.onsetcomp.com/products/data-loggers/ua-002-64
  state_var = state_sd^2 #variance of temperature observations

  param_sd = param_sd
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
    dplyr::filter(site_no %in% model_locations,
                  as.Date(dateTime) %in% model_dates) %>%
    mutate(date = as.Date(dateTime)) %>%
    select(site_no, date, temp_value) %>%
    group_by(site_no) %>%
    mutate(site_row = which(model_locations %in% site_no),  # getting which row in Y vector corresponds to site location
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
          pull(temp_value)
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
initialize_Y = function(Y, obs, n_states_est, n_params_est, n_params_obs, n_step, n_en, state_sd, param_sd){

  # initializing states with earliest observations and parameters
  first_obs = coalesce(!!!lapply(seq_len(dim(obs)[3]), function(i){obs[,,i]})) %>% # turning array into list, then using coalesce to find first obs in each position.
    ifelse(is.na(.), mean(., na.rm = T), .) # setting initial temp state to mean of earliest temp obs from other sites if no obs

  if(n_params_est > 0){
    ## update this later ***********************
    first_params = rep(.5, n_params_est)
  }else{
    first_params = NULL
  }

  Y[ , 1, ] = array(rnorm(n = n_en * (n_states_est + n_params_est),
                          mean = c(first_obs, first_params),
                          sd = c(state_sd, param_sd)),
                    dim = c(c(n_states_est + n_params_est), n_en))

  return(Y)
}


#' wrapper function for running EnKF for given model
#'
#' @param ind_file indicator file for scipiper target
#' @param n_en number of ensemble members, defaults to 100
#' @param start start of modeling
EnKF = function(ind_file,
                n_en = 100,
                start,
                stop,
                time_step = 'days',
                process_model = 'random_walk',
                #model_locations,
                obs_file,
                gd_config = 'lib/cfg/gd_config.yml'){

  # get model start, stop, full dates, and n_steps
  n_en = as.numeric(n_en)
  start = as.Date(as.character(start))
  stop = as.Date(as.character(stop))
  dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
  n_step = length(dates)

  # get observation matrix
  obs_df = readRDS(obs_file)

  # temp for now, add as input later ********************************
  model_locations = c('01115190','01011001', '30810310', '01482800','19393100',
                     '1308130831','310831','01015010','13083108','784200482')

  n_states_est = length(model_locations)

  # temp for now, add as input later ******************************
  n_params_est = 0 # number of parameters we're calibrating

  n_params_obs = 0 # number of parameters for which we have observations

  # temp for now, add as input later *****************************
  state_sd = rep(.5, n_states_est)

  param_sd = rep(.2, n_params_obs)

  # setting up matrices
  # observations as matrix
  obs = get_obs_matrix(obs_df = obs_df,
                       model_dates = dates,
                       model_locations = model_locations,
                       n_step = n_step,
                       n_states = n_states_est)

  # testing DA model error propogation
  # obs[4, 1, c(30:40, 100:112, 200:400)] = NA

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

  # use this later for organizing obs error matrix *******************************
  n_states_obs = 10 # number of states we're updating; will be dependent on obs

  # initialize Y vector
  Y = initialize_Y(Y = Y, obs = obs, n_states_est = n_states_est,
                   n_params_est = n_params_est, n_params_obs = n_params_obs,
                   n_step = n_step, n_en = n_en, state_sd = state_sd, param_sd = param_sd)

  # start modeling
  for(t in 2:n_step){
    for(n in 1:n_en){
      # set parameters / states for model config
      # model_config = Y[, t-1, n]

      # run model; using simple random walk for testing purposes
      model_output = random_walk(states = Y[, t-1, n], sd = .3)

      Y[ , t, n] = model_output # store in Y vector
    }
    if(any(!is.na(obs[ , , t]))){
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

