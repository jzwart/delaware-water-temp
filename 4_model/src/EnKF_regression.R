



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
# EnKF = function(ind_file,
#                 n_en = 20,
#                 start,
#                 stop,
#                 time_step = 'days',
#                 subbasin_file = '4_model_calibrate/out/drb_subbasins.rds',
#                 subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml',
#                 subbasin_outlet_id = NULL,
#                 model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp',
#                 obs_file = '3_observations/in/obs_temp_full.rds',
#                 init_param_file = '2_3_model_parameters/out/init_params.rds',
#                 model_run_loc = '4_model/tmp',
#                 orig_model_loc = '20200207_Delaware_streamtemp_state_adj', # this one has the modified prms.exe
#                 param_default_file = 'control/delaware.control.par_name',
#                 state_names,
#                 driver_file = NULL,
#                 n_states_est = 456,
#                 n_states_obs = 456,
#                 n_params_est = 2,
#                 n_param_loc = 456,
#                 n_params_obs = 0,
#                 obs_cv = 0.1,
#                 param_cv = 0.2,
#                 driver_cv = 0.1,
#                 init_cond_cv = 0.1,
#                 assimilate_obs = TRUE,
#                 covar_inf_factor = TRUE,
#                 covar_inf_factor_sd = 0.5,
#                 gd_config = 'lib/cfg/gd_config.yml'){


  ############## for debugging purposes ###########
  source('4_model/src/EnKF_functions.R')
  source('4_model/src/get_sntemp_values.R')
  source('4_model/src/run_sntemp.R')
  source('4_model/src/heat_budget_model.R')
  source('4_model/src/update_sntemp.R')
  source('4_model/src/set_sntemp_output.R')
  source('4_model_calibrate/src/calibrate_sntemp.R')
  source('4_model_calibrate/src/get_subbasins.R')
  source('4_model/src/get_upstream_downstream_segs.R')
  source('4_model_calibrate/src/get_calibration_order.R')
  source('4_model_calibrate/src/write_pestpp_tpl_files.R')
  source('4_model_calibrate/src/write_pestpp_ins_files.R')
  source('4_model_calibrate/src/write_pestpp_pst_files.R')
  source('2_3_model_parameters/src/add_default_sntemp_params.R')
  source('2_1_model_fabric/src/get_segment_hrus.R')
  library(tidyverse)
  library(igraph)
  start = '2014-05-01'
  stop = '2015-05-01'
  model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
  obs_file = '3_observations/in/obs_temp_full.rds'
  model_run_loc = I('4_model/tmp')
  orig_model_loc = I('20200207_Delaware_streamtemp_state_adj')
  subbasin_file = '4_model_calibrate/out/drb_subbasins.rds'
  subbasin_outlet_file = '4_model_calibrate/cfg/subbasin_outlets.yml'
  subbasin_outlet_id = '4182'
  param_groups = as_tibble(yaml::read_yaml('4_model/cfg/da_settings.yml')$param_groups)
  param_default_file = 'control/delaware.control.par_name'
  n_en = 20
  time_step = 'days'
  init_param_file = '2_3_model_parameters/out/init_params.rds'
  state_names = 'seg_tave_water' #  yaml::read_yaml('4_model/cfg/da_settings.yml')$states_to_update
  obs_cv = I(0.1)
  param_cv = I(0.2)
  init_cond_cv = I(0.1)
  covar_inf_factor_sd = 0.5
  assimilate_obs = TRUE
  covar_inf_factor = FALSE
  beta = 0.75 # weighting for how much uncertainty should go to observed vs. unobserved states (lower beta attributes most of the uncertainty for unobserved states, higher beta attributes most uncertainty to observed states)
  alpha = 0.5 # weight for how quickly the process error is allowed to adapt (low alpha quickly changes process error based on current innovations)
  localization = TRUE
  localization_cutoff = .7
  ind_file = sprintf('4_model/out/simple_regression_%s_to_%s_%sbeta_%salpha.rds.ind',
                     start, stop, beta, alpha)
  #######################################################################

  # copy over original run files to temporary file location
  # dir.create(model_run_loc, showWarnings = F)
  # print('Copying original model files to model working directory...')
  # files_to_transfer = list.files(orig_model_loc)
  # file.copy(from = file.path(orig_model_loc, files_to_transfer), to = model_run_loc, overwrite = T, recursive = T)

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

  n_states_est = 1 * nrow(model_locations) # length(state_names) * nrow(model_locations)  # making this segment based for now, would need to change if updating HRUs
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

  # model error matrix
  Q = get_model_error_matrix(n_states = n_states_est,
                             n_params = n_params_est,
                             n_step = n_step,
                             state_sd = rnorm(42, 2, .2),
                             param_sd = param_sd)

  # set up empty matrix for error covariance
  P = get_covar_matrix(n_states = n_states_est,
                       n_params = n_params_est,
                       n_step = n_step)

  # observation identity matrix
  H = get_obs_id_matrix(n_states_obs = n_states_obs,
                        n_states_est = n_states_est,
                        n_params_obs = n_params_obs,
                        n_params_est = n_params_est,
                        n_covar_inf_factor = n_covar_inf_factor,
                        n_step = n_step,
                        obs = obs)

  distance_matrix = readRDS('2_1_model_fabric/in/distance_matrix_subset.rds')$complete # arranged by seg_id_nat
  colnames(distance_matrix) = arrange(model_locations, as.numeric(seg_id_nat)) %>% pull(model_idx)
  rownames(distance_matrix) = arrange(model_locations, as.numeric(seg_id_nat)) %>% pull(model_idx)
  distance_matrix = distance_matrix[order(rownames(distance_matrix)), order(colnames(distance_matrix))]
  distance_matrix = (max(distance_matrix) - distance_matrix) / max(distance_matrix) # between 0 and 1 ; 1 is closer
  distance_matrix = ifelse(distance_matrix < localization_cutoff, 0, distance_matrix)


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

  # get initial model covariance
  P[,,1] = get_covar(deviations = get_ens_deviate(Y = Y, n_en = n_en, cur_step = 1),
                     n_en = n_en)

  # temporary est for delta air temp
  vars = remake::fetch('sntemp_output_vars')
  set_sntemp_output(
    output_names = vars,
    model_run_loc = model_run_loc)
  run_sntemp(start = dates[1] - 1,
             stop = dates[length(dates)],
             spinup = F,
             restart = F,
             save_ic = F,
             model_run_loc = model_run_loc)
  air_T = get_sntemp_intermediates(model_output_file = file.path(model_run_loc,
                                                                 'output/stream_temp.out.nsegment'),
                           model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'),
                           sntemp_vars = vars$sntemp_output) %>%
    dplyr::filter(parameter == 'seg_tave_air', model_idx %in% model_locations$model_idx) %>%
    pivot_wider(names_from = parameter, values_from = parameter_value) %>%
    group_by(model_idx) %>%
    mutate(count = n(),
           delta_air_T = c(NA,diff(seg_tave_air)))

  # start modeling
  if(assimilate_obs){
    for(t in 2:n_step){
      print(sprintf('Starting model runs for %s', dates[t]))
      for(n in 1:n_en){
        # set parameters / states for model config
        updated_states = get_updated_states(Y = Y,
                                            state_names = state_names,
                                            n_states_est = n_states_est,
                                            n_params_est = n_params_est,
                                            cur_step = t-1,
                                            en = n)

        updated_temp = updated_states[1:nrow(model_locations)]
        cur_delta_air_temp = air_T %>%
          dplyr::filter(date == dates[t]) %>%
          arrange(as.numeric(model_idx)) %>%
          pull(delta_air_T)

        # without process error
        stream_temp_out = calc_streamtemp_regression(tw_tminus1 = updated_temp,
                                                     delta_air_temp = cur_delta_air_temp,
                                                     air_temp_int = 0.01,
                                                     air_temp_slope = 0.35,
                                                     model_idx = model_locations$model_idx)
        predicted_states = stream_temp_out$seg_tave_water # predicted states from model

        #random walk
        # predicted_states = updated_temp + rnorm(n = n_states_est, mean = 0, sd = 1)

        Y[ , t, n] = predicted_states # only updating states, not params
      }
      # uncorrupted ensemble deviations before process error
      dstar_t = get_ens_deviate(Y = Y,
                                n_en = n_en,
                                cur_step = t)
      # uncorrupted covariance before process error
      Pstar_t = get_covar(deviations = dstar_t,
                          n_en = n_en)

      # add process error
      Y = add_process_error(Y = Y,
                            Q = Q,
                            n_en = n_en,
                            cur_step = t)
      # ensemble deviations with process error
      d_t = get_ens_deviate(Y = Y,
                            n_en = n_en,
                            cur_step = t)
      # covariance with process error
      P[,,t] = get_covar(deviations = d_t,
                         n_en = n_en)

      y_it = get_innovations(obs = obs,
                             H = H,
                             Y = Y,
                             R = R,
                             cur_step = t,
                             n_en = n_en,
                             n_states_est = n_states_est,
                             n_params_est = n_params_est)
      S_t = get_covar(deviations = y_it, n_en = n_en)

      # update model process error matrix
      if(t < n_step){
        Q = update_model_error(Y = Y,
                               R = R,
                               H = H,
                               Q = Q,
                               P = P,
                               Pstar_t = Pstar_t,
                               S_t = S_t,
                               n_en = n_en,
                               cur_step = t,
                               n_states_est = n_states_est,
                               n_params_est = n_params_est,
                               beta = beta,
                               alpha = alpha)
      }

      if(any(H[,,t]==1)){
        print('updating with Kalman Filter...')
        # Y = kalman_filter_rastetter(Y = Y,
        #                             y_it = y_it,
        #                             S_t = S_t,
        #                             P = P,
        #                             R = R,
        #                             H = H,
        #                             cur_step = t,
        #                             localization = localization,
        #                             distance_matrix = distance_matrix) # updating params / states if obs available
        Y = kalman_filter(Y = Y,
                          R = R,
                          obs = obs,
                          H = H,
                          P,
                          n_en = n_en,
                          cur_step = t,
                          covar_inf_factor = covar_inf_factor,
                          n_states_est = n_states_est,
                          n_params_est = n_params_est,
                          n_covar_inf_factor = n_covar_inf_factor,
                          localization = localization,
                          distance_matrix = distance_matrix)
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

  out = list(Y = Y, dates = dates, obs = obs, R = R, P = P, Q = Q, model_locations = model_locations)

  saveRDS(object = out, file = scipiper::as_data_file(ind_file))
# }



