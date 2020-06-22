

# function for writing template files for pestpp
write_pestpp_tpl_files = function(params,
                                  model_run_loc,
                                  param_file_name,
                                  param_file_out,
                                  delim,
                                  n_digits = 10){

  par_number = seq(1, nrow(params))

  row_number = rep(NA, nrow(params))

  params_out = readLines(file.path(model_run_loc, param_file_name))

  for(i in 1:nrow(params)){
    cur_param_name = params$param_name[i]
    cur_model_idx = as.numeric(params$model_idx[i])

    cur_param_loc = grep(cur_param_name, params_out) + cur_model_idx + 4

    row_number[i] = cur_param_loc
  }
  params$row_number = row_number
  params$tpl = sprintf('%s%s          %s', delim, paste(params$param_name, params$model_idx, sep = '_'), delim)

  params_out[params$row_number] = params$tpl

  params_out = c(sprintf('ptf %s', delim), params_out)


  writeLines(params_out, file.path(model_run_loc, param_file_out))
}



