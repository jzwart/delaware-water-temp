
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
get_Y_vector = function(n_states, n_params_est, n_covar_inf_factor, n_step, n_en){

  Y = array(dim = c(n_states + n_params_est + n_covar_inf_factor, n_step, n_en))

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

  param_var = c()
  if(length(names(param_sd)) > 0){
    for(i in seq_along(names(param_sd))){
      param_var = c(param_var, param_sd[[names(param_sd)[i]]]^2)
    }
  }

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

#' model error matrix, should be a square matrix where
#'   col & row = the number of states and params for which you are estimating
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_param number of parameters for which we are estimates
#' @param n_step number of model timesteps
#' @param state_sd vector of state observation standard deviation; assuming sd is constant through time
#' @param param_sd vector of parmaeter observation standard deviation; assuming sd is constant through time
get_model_error_matrix = function(n_states, n_params, n_step, state_sd, param_sd){

  Q = array(0, dim = c(n_states + n_params, n_states + n_params, n_step))

  state_var = state_sd^2 #variance of temperature observations

  param_var = c()
  if(length(names(param_sd)) > 0){
    for(i in seq_along(names(param_sd))){
      param_var = c(param_var, param_sd[[names(param_sd)[i]]]^2)
    }
  }

  if(n_params_obs > 0){
    all_var = c(state_var, param_var)
  }else{
    all_var = state_var
  }

  for(i in 1:n_step){
    # variance is the same for each segment and time step; updated in DA routine
    Q[,,i] = diag(all_var, n_states + n_params, n_states + n_params)
  }

  return(Q)
}

#' model covariance matrix, should be a square matrix where
#'   col & row = the number of states and params for which you are estimating
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_param number of parameters for which we are estimates
#' @param n_step number of model timesteps
get_covar_matrix = function(n_states, n_params, n_step){

  P = array(NA, dim = c(n_states + n_params, n_states + n_params, n_step))

  return(P)
}

#' Measurement operator matrix saying 1 if there is observation data available, 0 otherwise
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_param_obs number of parameters for which we have observations
#' @param n_params_est number of parameters we're calibrating
#' @param n_step number of model timesteps
#' @param obs observation matrix created with get_obs_matrix function
get_obs_id_matrix = function(n_states_obs,
                             n_states_est,
                             n_params_obs,
                             n_params_est,
                             n_covar_inf_factor,
                             n_step,
                             obs){

  H = array(0, dim=c(n_states_obs + n_params_obs, n_states_est + n_params_est + n_covar_inf_factor, n_step))

  # order goes 1) states, 2)params for which we have obs, 3) params for which we're estimating but don't have obs 4) covariance inflation factor if estimated

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
    dplyr::select(seg_id_nat, date, temp_C) %>%
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
kalman_filter = function(Y,
                         R,
                         obs,
                         H,
                         P,
                         n_en,
                         cur_step,
                         covar_inf_factor,
                         n_states_est,
                         n_params_est,
                         n_covar_inf_factor,
                         localization,
                         distance_matrix){

  cur_obs = obs[ , , cur_step]

  cur_obs = ifelse(is.na(cur_obs), 0, cur_obs) # setting NA's to zero so there is no 'error' when compared to estimated states

  ###### estimate the spread of your ensembles #####
  delta_Y = get_ens_deviate(Y = Y, n_en = n_en, cur_step = cur_step)

  ###### covariance #######
  P_t = get_covar(deviations = delta_Y, n_en = n_en)

  if(covar_inf_factor){ # check to see if I have this right
    covar_inf_mean = mean(Y_mean[(n_states_est+n_params_est+1):(n_states_est+n_params_est+n_covar_inf_factor), ]) # just taking mean for now because I don't think it's easy to make this inflation for any one segment

    # estimate Kalman gain #
    K = ((1 / (n_en - 1)) * covar_inf_mean * delta_Y %*% t(delta_Y) %*% t(H[, , cur_step])) %*%
      qr.solve(((1 / (n_en - 1)) * covar_inf_mean * H[, , cur_step] %*% delta_Y %*% t(delta_Y) %*% t(H[, , cur_step]) + R[, , cur_step]))
  }else{
    # estimate Kalman gain w/o covar_inf_factor #
    if(localization){
      K = P_t %*% t(H[, , cur_step]) %*% qr.solve(((1 / (n_en - 1)) * H[, , cur_step] %*% delta_Y %*% t(delta_Y) %*% t(H[, , cur_step]) * distance_matrix + R[, , cur_step]))
    }else{
      K = P_t %*% t(H[, , cur_step]) %*% qr.solve(((1 / (n_en - 1)) * H[, , cur_step] %*% delta_Y %*% t(delta_Y) %*% t(H[, , cur_step]) + R[, , cur_step]))
    }
  }

  # update Y vector #
  for(q in 1:n_en){
    Y[, cur_step, q] = Y[, cur_step, q] + K %*% (cur_obs - H[, , cur_step] %*% Y[, cur_step, q]) # adjusting each ensemble using kalman gain and observations
  }
  return(Y)
}


##' @param Y vector for holding states and parameters you're estimating
##' @param R observation error matrix
##' @param obs observations at current timestep
##' @param H observation identity matrix
##' @param n_en number of ensembles
##' @param cur_step current model timestep
kalman_filter_rastetter = function(Y,
                                   y_it,
                                   H,
                                   S_t,
                                   P,
                                   R,
                                   cur_step,
                                   localization,
                                   distance_matrix){

  if(localization){
    P_t = P[,,cur_step] * distance_matrix
  }else{
    P_t = P[,,cur_step]
  }

  K = qr.solve(P_t %*% t(H[, , cur_step]) %*% S_t + R[,,cur_step])

  # update Y vector #
  Y_out = Y
  for(q in 1:n_en){
    Y_out[, cur_step, q] = Y[, cur_step, q] + K %*% y_it[,q] # adjusting each ensemble using kalman gain and observations
  }
  return(Y_out)
}

update_model_error = function(Y,
                              R,
                              H,
                              Q,
                              P,
                              Pstar_t,
                              S_t,
                              n_en,
                              cur_step,
                              n_states_est,
                              n_params_est,
                              beta,
                              alpha){

  gamma = get_error_dist(Y = Y, H = H, R = R, P = P, n_en = n_en, cur_step = cur_step, beta = beta)

  Q_hat = gamma %*% (S_t - H[,,cur_step] %*% Pstar_t %*% t(H[,,cur_step]) - R[,,cur_step]) %*% t(gamma)

  Q[,,(cur_step+1)] = alpha * Q[,,cur_step] + (1-alpha)*Q_hat

  return(Q)
}

get_innovations = function(obs,
                           H,
                           Y,
                           R,
                           cur_step,
                           n_en,
                           n_states_est,
                           n_params_est){

  cur_obs = obs[ , , cur_step]

  cur_obs = ifelse(is.na(cur_obs), 0, cur_obs) # setting NA's to zero so there is no 'error' when compared to estimated states

  y_it = matrix(nrow = n_states_est, ncol = n_en)
  for(q in 1:n_en){
    y_it[,q] = cur_obs - H[, , cur_step] %*% Y[, cur_step, q] +
      rnorm(n_states_est, 0, R[q,q,cur_step])
  }

  return(y_it)
}

add_process_error = function(Y,
                             Q,
                             n_en,
                             cur_step){

  # add process error
  for(q in 1:n_en){
    Y[1:dim(Q)[2], cur_step, q] = Y[1:dim(Q)[2], cur_step, q] +
      rnorm(dim(Q)[2], rep(0,dim(Q)[2]), sqrt(abs(diag(Q[,,cur_step]))))
  }

  return(Y)
}

# get ensemble deviations
get_ens_deviate = function(Y,
                           n_en,
                           cur_step){

  Y_mean = matrix(apply(Y[ , cur_step, ], MARGIN = 1, FUN = mean), nrow = length(Y[ , 1, 1])) # calculating the mean of each temp and parameter estimate
  delta_Y = Y[ , cur_step, ] - matrix(rep(Y_mean, n_en), nrow = length(Y[ , 1, 1])) # difference in ensemble state/parameter and mean of all ensemble states/parameters
  return(delta_Y)
}

#get covariance
get_covar = function(deviations,
                     n_en){
  covar = (1 / (n_en - 1)) * deviations %*% t(deviations)
  return(covar)
}

# returns gamma from Restatter
get_error_dist = function(Y,
                          H,
                          R,
                          P,
                          n_en,
                          cur_step,
                          beta){
  # see Rastetter et al Ecological Applications, 20(5), 2010, pp. 1285–1301
  ###### covariance #######
  P_t = P[,,cur_step]

  H_t = H[,,cur_step]

  # had to add R[,,cur_step] to make matrix solve not singular
  gamma = t(((1-beta) * qr.solve(H_t %*% P_t %*% t(H_t) + R[,,cur_step])) %*% H_t %*% P_t %*%
    (diag(1, nrow = dim(H_t)[2], ncol = dim(H_t)[2]) - t(H_t) %*% H_t) + beta*H_t)

  return(gamma)
}



#' initialize Y vector with draws from distribution of obs
#'
#' @param Y Y vector
#' @param obs observation matrix
initialize_Y = function(Y, init_states, init_params, init_covar_inf_factor,
                        n_states_est, n_states_obs, n_params_est,
                        n_params_obs, n_covar_inf_factor, n_step, n_en,
                        state_sd, param_sd, covar_inf_factor_sd){

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

  param_sd_vec = c()
  if(n_params_est > 0){
    param_names = names(init_params) #colnames(init_params)[3:ncol(init_params)]
    first_params = c()
    for(i in seq_along(param_names)){
      cur_param = init_params[[param_names[i]]] #  %>%
        # arrange(as.numeric(model_idx)) %>% pull(2+i)
      first_params = c(first_params, cur_param)
      param_sd_vec = param_sd[[param_names[i]]]
    }
    first_params = as.numeric(first_params)
    param_sd_vec = as.numeric(param_sd_vec)
  }else{
    first_params = NULL
  }

  if(n_covar_inf_factor > 0){
    first_covar_inf_factors = init_covar_inf_factor
    covar_inf_factor_sd_vec = rep(covar_inf_factor_sd, n_covar_inf_factor)
  }else{
    first_covar_inf_factors = NULL
    covar_inf_factor_sd_vec = NULL
  }

  Y[ , 1, ] = array(rnorm(n = n_en * (n_states_est + n_params_est + n_covar_inf_factor),
                          mean = c(first_states, first_params, first_covar_inf_factors),
                          sd = c(state_sd, param_sd_vec, covar_inf_factor_sd_vec)),
                    dim = c(c(n_states_est + n_params_est + n_covar_inf_factor), n_en))

  return(Y)
}

get_updated_params = function(Y, param_names, n_states_est, n_params_est, cur_step, en, model_run_loc, param_default_file){

  updated_params = Y[(n_states_est+1):(n_states_est+n_params_est), cur_step, en]

  out = vector(mode = 'list', length = length(param_names))

  if(length(param_names) == 0){
    out = out
  }else{
    for(i in seq_along(param_names)){

      defaults = get_default_param_vals(param_name = param_names[i],
                                        model_run_loc = model_run_loc,
                                        param_default_file = param_default_file)

      param_loc_start = as.numeric(defaults$size) * (i-1) + 1
      param_loc_end = param_loc_start + as.numeric(defaults$size) - 1

      cur_param_vals = updated_params[param_loc_start:param_loc_end]

      if(any(as.numeric(cur_param_vals) <= as.numeric(defaults$min))){
        range = as.numeric(defaults$max) - as.numeric(defaults$min)
        # add quarter of range from min
        to_add = range *.05
        cur_param_vals[as.numeric(cur_param_vals) <= as.numeric(defaults$min)] = as.character(as.numeric(cur_param_vals[as.numeric(cur_param_vals) <= as.numeric(defaults$min)]) + to_add)
      }
      if(defaults$type == '1'){
        cur_param_vals = as.character(round(as.numeric(cur_param_vals), digits = 0))
      }

      out[[i]] = cur_param_vals
      names(out)[i] = param_names[i]
    }
  }

  return(out)
}

get_updated_covar_inf_factor = function(Y,
                                        n_states_est,
                                        n_params_est,
                                        n_covar_inf_factor,
                                        cur_step,
                                        en){

  out = Y[(n_states_est+n_params_est+1):(n_states_est+n_params_est+n_covar_inf_factor), cur_step, en]

  return(out)
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

# add_process_error = function(preds,
#                              dates,
#                              model_idx,
#                              state_error,
#                              alpha,
#                              beta,
#                              R,
#                              obs,
#                              H,
#                              n_en,
#                              cur_step){
#
#
#   if(length(dates) == 1){
#     q = NA
#     w = rnorm(length(model_idx), 0, 1)
#     q = state_error * w
#
#     preds[1:length(model_idx)] = preds[1:length(model_idx)] + q
#     preds[1:length(model_idx)] = ifelse(preds[1:length(model_idx)] < 0, 0, preds[1:length(model_idx)])
#
#     return(preds)
#   }else{
#     q = NA
#     for(i in seq_along(model_idx)){
#       w = rnorm(length(dates), 0, 1)
#       q[1] = state_error * w[1]
#       for(z in 2:length(dates)){
#         q[z] = alpha * q[z-1] + sqrt(1-alpha^2) * state_error * w[z]
#       }
#       preds$water_temp[preds$model_idx == model_idx[i]] = preds$water_temp[preds$model_idx == model_idx[i]] + q
#       preds$water_temp = ifelse(preds$water_temp < 0, 0,preds$water_temp)
#     }
#
#     return(preds)
#   }
#
# }



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
                subbasin_file = '4_model_calibrate/out/drb_subbasins.rds',
                subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml',
                subbasin_outlet_id = NULL,
                model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp',
                obs_file = '3_observations/in/obs_temp_full.rds',
                init_param_file = '2_3_model_parameters/out/init_params.rds',
                model_run_loc = '4_model/tmp',
                orig_model_loc = '20200207_Delaware_streamtemp_state_adj', # this one has the modified prms.exe
                param_default_file = 'control/delaware.control.par_name',
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
                covar_inf_factor = TRUE,
                covar_inf_factor_sd = 0.5,
                gd_config = 'lib/cfg/gd_config.yml'){


  ############## for debugging purposes ###########
  # source('4_model/src/EnKF_functions.R')
  # source('4_model/src/get_sntemp_values.R')
  # source('4_model/src/run_sntemp.R')
  # source('4_model/src/update_sntemp.R')
  # source('4_model/src/set_sntemp_output.R')
  # source('4_model_calibrate/src/calibrate_sntemp.R')
  # source('4_model_calibrate/src/get_subbasins.R')
  # source('4_model/src/get_upstream_downstream_segs.R')
  # source('4_model_calibrate/src/get_calibration_order.R')
  # source('4_model_calibrate/src/write_pestpp_tpl_files.R')
  # source('4_model_calibrate/src/write_pestpp_ins_files.R')
  # source('4_model_calibrate/src/write_pestpp_pst_files.R')
  # source('2_3_model_parameters/src/add_default_sntemp_params.R')
  # source('2_1_model_fabric/src/get_segment_hrus.R')
  # library(tidyverse)
  # library(igraph)
  # start = '2014-05-01'
  # stop = '2014-05-10'
  # model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
  # obs_file = '3_observations/in/obs_temp_full.rds'
  # model_run_loc = I('4_model/tmp')
  # orig_model_loc = I('20200207_Delaware_streamtemp_state_adj')
  # subbasin_file = '4_model_calibrate/out/drb_subbasins.rds'
  # subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml'
  # subbasin_outlet_id = '4182'
  # param_groups = as_tibble(yaml::read_yaml('4_model/cfg/da_settings.yml')$param_groups)
  # param_default_file = 'control/delaware.control.par_name'
  # n_en = 20
  # time_step = 'days'
  # init_param_file = '2_3_model_parameters/out/init_params.rds'
  # state_names = yaml::read_yaml('4_model/cfg/da_settings.yml')$states_to_update
  # obs_cv = I(0.1)
  # param_cv = I(0.2)
  # init_cond_cv = I(0.1)
  # covar_inf_factor_sd = 0.5
  # assimilate_obs = TRUE
  # covar_inf_factor = TRUE
  #######################################################################

  # copy over original run files to temporary file location
  dir.create(model_run_loc, showWarnings = F)
  print('Copying original model files to model working directory...')
  files_to_transfer = list.files(orig_model_loc)
  file.copy(from = file.path(orig_model_loc, files_to_transfer), to = model_run_loc, overwrite = T, recursive = T)

  # spinning up model for running DA
  # model_spinup(
  #   n_en = n_en,
  #   start = start,
  #   stop = stop,
  #   time_step = I('days'),
  #   model_run_loc = I('4_model/tmp'),
  #   spinup_days = I(730))

  # use this to organize the matrices
  model_fabric = sf::read_sf(model_fabric_file)

  subbasins = readRDS(subbasin_file)

  if(!is.null(subbasin_outlet_id)){
    cur_subbasin = subbasins[subbasin_outlet_id][[subbasin_outlet_id]]
  }

  # get subbasin parameter locations
  cur_model_idxs = as.character(cur_subbasin$model_idx)

  # arrange by model_idx
  model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                           model_idx = as.character(model_fabric$model_idx)) %>%
    dplyr::filter(model_idx %in% cur_model_idxs) %>%
    arrange(as.numeric(model_idx))

  # get model start, stop, full dates, and n_steps
  n_en = as.numeric(n_en)
  start = as.Date(as.character(start))
  stop = as.Date(as.character(stop))
  dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
  n_step = length(dates)

  n_states_est = length(state_names) * nrow(model_locations)  # making this segment based for now, would need to change if updating HRUs
  n_states_obs = nrow(model_locations) # only assimilating temperature obs
  n_params_obs = 0 # include this as input or dynamic
  state_sd = rep(obs_cv * 10, n_states_est)  # UPDATE THIS #########

  # get observation matrix
  obs_df = readRDS(obs_file)

  # get initial parameters; already arranged by model_idx within each param list
  init_params_list = NULL #readRDS(init_param_file)

  n_params_est = sum(unlist(lapply(init_params_list, length)))

  if(n_params_est > 0){
    param_names = names(init_params_list)

    param_sd = vector(mode = 'list', length = length(param_names))
    for(i in seq_along(param_names)){
      cur_param_vals = init_params_list[[param_names[i]]]
      cur_param_sd = param_groups$sd[param_groups$param == param_names[i]]

      param_sd[[i]] = rep(cur_param_sd, length(cur_param_vals))
      names(param_sd)[i] = param_names[i]
    }
  }else{
    param_names = NULL
    param_sd = NULL
  }

  if(covar_inf_factor){
    covar_inf_factor_df = tibble(model_idx = cur_model_idxs,
                                 covar_inf_factor = 1)%>%
      arrange(as.numeric(model_idx))
    n_covar_inf_factor = nrow(covar_inf_factor_df)
  }else{n_covar_inf_factor = 0}

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
                   n_covar_inf_factor = n_covar_inf_factor,
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
                        n_covar_inf_factor = n_covar_inf_factor,
                        n_step = n_step,
                        obs = obs)

  # for initial states, we want to use the observation for the first time step, if avaialble, otherwise use
  #  the mean of the ensembles' initial conditions.

  init_states = model_locations
  for(n in 1:n_en){
    cur_ic = get_sntemp_initial_states(state_names = state_names,
                                       seg_model_idxs = cur_model_idxs,
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
                   init_params = init_params_list,
                   init_covar_inf_factor = covar_inf_factor_df$covar_inf_factor,
                   n_states_est = n_states_est,
                   n_states_obs = n_states_obs,
                   n_params_est = n_params_est,
                   n_params_obs = n_params_obs,
                   n_covar_inf_factor = n_covar_inf_factor,
                   n_step = n_step,
                   n_en = n_en,
                   state_sd = state_sd,
                   param_sd = param_sd,
                   covar_inf_factor_sd = covar_inf_factor_sd)


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
                                              en = n,
                                              model_run_loc = model_run_loc,
                                              param_default_file = param_default_file)
          # added into the get_updated_params() function ;
          # if(any(updated_params<=0)){ # forcing positive for now but should come up with a better search solution
          #   updated_params[updated_params<=0] = 1
          # }
          update_sntemp_params(param_names = param_names,
                               updated_params = updated_params,
                               model_run_loc = model_run_loc)
        }

        updated_states = get_updated_states(Y = Y,
                                            state_names = state_names,
                                            n_states_est = n_states_est,
                                            n_params_est = n_params_est,
                                            cur_step = t-1,
                                            en = n)

        if(t==2){
          update_sntemp_states(state_names = state_names,
                               seg_model_idxs = cur_model_idxs,
                               updated_states = updated_states,
                               ic_file_in = sprintf('prms_ic_spinup_%s.txt', n),
                               ic_file_out = sprintf('prms_ic_%s.txt', n))
        }else{
          update_sntemp_states(state_names = state_names,
                               seg_model_idxs = cur_model_idxs,
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
                                           seg_model_idxs = cur_model_idxs,
                                           model_run_loc = model_run_loc,
                                           ic_file = sprintf('prms_ic_%s.txt', n))
        predicted_states = gather_states(ic_out) # predicted states from model

        if(n_params_est > 0){
          updated_params_vec = c()
          for(i in seq_along(param_names)){
            updated_params_vec = c(updated_params_vec, updated_params[[param_names[i]]])
          }
          updated_params_vec = as.numeric(updated_params_vec)
          if(n_covar_inf_factor>0){
            updated_covar_inf_factor = get_updated_covar_inf_factor(Y = Y,
                                                                    n_states_est = n_states_est,
                                                                    n_params_est = n_params_est,
                                                                    n_covar_inf_factor = n_covar_inf_factor,
                                                                    cur_step = t-1,
                                                                    en = n)
            Y[ , t, n] = c(predicted_states, updated_params_vec, updated_covar_inf_factor) # store in Y vector
          }else{
            Y[ , t, n] = c(predicted_states, updated_params_vec) # store in Y vector
          }
        }else{
          if(n_covar_inf_factor>0){
            updated_covar_inf_factor = get_updated_covar_inf_factor(Y = Y,
                                                                    n_states_est = n_states_est,
                                                                    n_params_est = n_params_est,
                                                                    n_covar_inf_factor = n_covar_inf_factor,
                                                                    cur_step = t-1,
                                                                    en = n)
            Y[ , t, n] = c(predicted_states, updated_covar_inf_factor) # store in Y vector
          }else{
            Y[ , t, n] = predicted_states # only updating states, not params
          }
        }
      }
      if(any(H[,,t]==1)){
        print('updating with Kalman Filter...')
        Y = kalman_filter(Y = Y,
                          R = R,
                          obs = obs,
                          H = H,
                          n_en = n_en,
                          cur_step = t,
                          covar_inf_factor = covar_inf_factor,
                          n_states_est = n_states_est,
                          n_params_est = n_params_est,
                          n_covar_inf_factor = n_covar_inf_factor) # updating params / states if obs available
      }
    }
  }else{
    t = 2
    for(n in 1:n_en){
      # set parameters / states for model config
      if(n_params_est>0){
        updated_params = get_updated_params(Y = Y,
                                            param_names = param_names,
                                            n_states_est = n_states_est,
                                            n_params_est = n_params_est,
                                            cur_step = t-1,
                                            en = n,
                                            model_run_loc = model_run_loc,
                                            param_default_file = param_default_file)
        update_sntemp_params(param_names = param_names,
                             updated_params = updated_params,
                             model_run_loc = model_run_loc)
      }
      updated_states = get_updated_states(Y = Y,
                                          state_names = state_names,
                                          n_states_est = n_states_est,
                                          n_params_est = n_params_est,
                                          cur_step = t-1,
                                          en = n)

      if(t==2){
        update_sntemp_states(state_names = state_names,
                             seg_model_idxs = cur_model_idxs,
                             updated_states = updated_states,
                             ic_file_in = sprintf('prms_ic_spinup_%s.txt', n),
                             ic_file_out = sprintf('prms_ic_%s.txt', n))
      }else{
        update_sntemp_states(state_names = state_names,
                             seg_model_idxs = cur_model_idxs,
                             updated_states = updated_states,
                             ic_file_in = sprintf('prms_ic_%s.txt', n),
                             ic_file_out = sprintf('prms_ic_%s.txt', n))
      }

      # run model
      run_sntemp(start = dates[t],
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
        dplyr::filter(date %in% dates[t:n_step], model_idx %in% cur_model_idxs) %>%
        arrange(date, as.numeric(model_idx))

      out_temp = array(model_output$water_temp, dim = c(nrow(model_locations), n_step-1))

      Y[1:nrow(model_locations), t:n_step, n] = out_temp # store temp in Y vector
      if(n_params_est > 0){
        out_params = array(rep(updated_params, (n_step-1)), dim = c(n_params_est, n_step-1))
        Y[(n_states_est+1):(n_states_est+n_params_est), t:n_step, n] = out_params # store params in Y vector
      }
    }
  }

  out = list(Y = Y, dates = dates, obs = obs, R = R, model_locations = model_locations)

  saveRDS(object = out, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}



