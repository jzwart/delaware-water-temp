
#' update parameters
#'

update_sntemp_params = function(param_names, updated_params, param_file = 'delaware.control.param', n_segments = 456){

  params = readLines(file.path('20190913_Delaware_streamtemp/control', param_file))

  for(i in 1:length(param_names)){
    param_loc_start = grep(param_names[i], params) + 5
    param_loc_end = param_loc_start + n_segments - 1

    params[param_loc_start:param_loc_end] = as.character(updated_params[[param_names[i]]])
  }

  writeLines(params, file.path('20190913_Delaware_streamtemp/control', param_file))
}



# param_names = c('ss_tau', 'gw_tau')
#
# updated_params = list(ss_tau = as.numeric(cur_param) + rnorm(n = n_segments),
#                       gw_tau = as.numeric(cur_param1) + rnorm(n = n_segments, sd = 5))
#
# updated_params[['ss_tau']]
#
# update_params(param_names = param_names, updated_params = updated_params, n_segments = 456)

