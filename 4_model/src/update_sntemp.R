
#' update parameters
#'
update_sntemp_params = function(param_names, updated_params,
                                model_run_loc = '4_model/tmp',
                                param_file = 'delaware.control.param', n_segments = 456){

  params = readLines(file.path(model_run_loc, 'control', param_file))

  for(i in 1:length(param_names)){
    param_loc_start = grep(param_names[i], params) + 5
    param_loc_end = param_loc_start + n_segments - 1

    params[param_loc_start:param_loc_end] = as.character(updated_params[((i-1)*n_segments+1):(i*n_segments)])
  }

  writeLines(params, file.path(model_run_loc, 'control', param_file))
}

