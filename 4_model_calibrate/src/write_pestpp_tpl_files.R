

# function for writing template files for pestpp
write_pestpp_tpl_files = function(params,
                                  model_run_loc,
                                  param_file_name,
                                  param_file_out,
                                  delim,
                                  n_digits = 10,
                                  n_segments = 456){

  seg_params = params$seg_params
  seg_month_params = params$seg_month_params

  row_number = rep(NA, nrow(seg_params))

  params_out = readLines(file.path(model_run_loc, param_file_name))

  for(i in 1:nrow(seg_params)){
    cur_param_name = seg_params$param_name[i]
    cur_model_idx = as.numeric(seg_params$model_idx[i])

    cur_param_loc = grep(cur_param_name, params_out) + cur_model_idx + 4

    row_number[i] = cur_param_loc
  }
  seg_params$row_number = row_number
  seg_params$tpl = sprintf('%s%s  %s', delim, paste(seg_params$param_name, seg_params$model_idx, sep = '_'), delim)

  params_out[seg_params$row_number] = seg_params$tpl

  # do same for seg x month params
  row_number = rep(NA, nrow(seg_month_params))

  for(i in 1:nrow(seg_month_params)){
    cur_param_name = seg_month_params$param_name[i]
    cur_model_idx = as.numeric(seg_month_params$model_idx[i])
    cur_month = as.numeric(seg_month_params$month[i])

    cur_param_loc = which(params_out == cur_param_name) + cur_model_idx + (n_segments * (cur_month - 1)) + 5

    row_number[i] = cur_param_loc
  }
  seg_month_params$row_number = row_number
  seg_month_params$tpl = sprintf('%s%s  %s',
                                 delim,
                                 paste(seg_month_params$param_name, seg_month_params$model_idx, seg_month_params$month, sep = '_'),
                                 delim)

  params_out[seg_month_params$row_number] = seg_month_params$tpl

  params_out = c(sprintf('ptf %s', delim), params_out)

  writeLines(params_out, file.path(model_run_loc, param_file_out))
}


