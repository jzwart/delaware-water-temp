
n_en = 100 # number of ensembles

n_params_est = 10 # number of parameters we're calibrating

n_params_obs = 0 # number of parameters for which we have observations

n_states = 10 # number of states we're updating; will be dependent on obs

start = as.Date('2010-01-01')

stop = as.Date('2015-01-01')

dates = get_model_dates(model_start = start, model_stop = stop)

n_step = length(dates)

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
get_obs_id_matrix = function(n_states, n_params_obs, n_params_est, n_step, obs){

  # dimensions will be n_depths by Y vector length (or n_depths + n_params)
  H = array(0, dim=c(n_states + n_params_obs, n_states + n_params_est, n_step))

  # order goes 1) states, 2)params for which we have obs, 3) params for which we're estimating but don't have obs

  for(t in 1:n_step){
    H[1:(n_states + n_params_obs), 1:(n_states + n_params_obs), t] = diag(ifelse(is.na(obs[,,t]),0, 1), n_states + n_params_obs, n_states + n_params_obs)
  }

  return(H)
}

obs_df1 = readRDS('3_observations/in/nwis_dv_data.rds')
obs_df = obs_df1 %>%
  dplyr::slice(1:5000)

unique(obs_df$site_no)
range(obs_df$dateTime)

model_locations = c('01115190','01011001', '30810310', '13081031','19393100',
                    '1308130831','310831','03183018','13083108','784200482')




#' turn dataframe into matrix
#'
#' @param obs_df observation data frame
#' @param model_dates dates over which you're modeling
#' @param model_locations locations where you're estimating temperature
get_obs_matrix = function(obs_df, model_dates, model_locations, n_step, n_states){

  # need to know location and time of observation

  obs_df_filtered = obs_df %>%
    dplyr::filter(site_no %in% model_locations,
                  as.Date(dateTime) %in% model_dates) %>%
    mutate(date = as.Date(dateTime)) %>%
    select(site_no, date, temp_value) %>%
    mutate(site_row = which(model_locations %in% site_no),  # getting which row in Y vector corresponds to site location
           date_step = which(model_dates %in% date))

  obs_matrix = array(NA, dim = c(n_states, 1, n_step))

  for(i in 1:length(model_locations)){
    for(j in 1:length(model_dates)){
      if(i %in% obs_df_filtered$site_row & j %in% obs_df_filtered$date_step){
        obs_matrix[i, 1, j] = dplyr::filter(obs_df_filtered,
                                            site_row == i,
                                            date_step == j) %>%
          pull(temp_value)
      }
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

  ###### estimate the spread of your ensembles #####
  Y_mean = matrix(apply(Y[,cur_step,], MARGIN = 1, FUN = mean), nrow = length(Y[,1,1])) # calculating the mean of each temp and parameter estimate
  delta_Y = Y[,cur_step,] - matrix(rep(Y_mean, n_en), nrow = length(Y[,1,1])) # difference in ensemble state/parameter and mean of all ensemble states/parameters

  ###### estimate Kalman gain #########
  K = ((1 / (nEn - 1)) * delta_Y %*% t(delta_Y) %*% t(H[, , cur_step])) %*%
    qr.solve(((1 / (nEn - 1)) * H[, , cur_step] %*% delta_Y %*% t(delta_Y) %*% t(H[, , cur_step]) + R[, , cur_step]))

  ###### update Y vector ######
  for(q in 1:n_en){
    Y[, cur_step, q] = Y[, cur_step, q] + K %*% (obs - H[, , cur_step] %*% Y[, cur_step, q]) # adjusting each ensemble using kalman gain and observations
  }
  return(Y)
}

for(t in 1:n_step){
  for(n in 1:n_en){
    # set parameters / states for model config
    # run model
    Y = model_output # store in Y vector
  }
  if(any(obs)){
    Y = kalman_filter(Y = Y,
                      R = R,
                      cur_obs = obs[t, ],
                      H = H,
                      n_en = n_en,
                      cur_step = t) # updating params / states if obs available
  }
  # update states / params for model config
}

