

# function for writing param_files used for hydroPSO
write_hydroPSO_params = function(params,
                                 param_ranges,
                                 model_run_loc,
                                 param_file_name = 'control/delaware.control.param',
                                 param_file_out = 'PSO.in/ParamFiles.txt',
                                 param_range_file_out = 'PSO.in/ParamRanges.txt',
                                 col_start = 1,
                                 col_end = 10,
                                 dec_places = 0){

  par_number = seq(1, nrow(params))
  # hack for par_number
  # par_number = ifelse(params$param_name == 'ss_tau', 1, 2)
  par_name = paste(params$param_name, params$model_idx, sep = '_')
  # par_name = params$param_name

  file_name = rep(param_file_name, nrow(params))
  row_number = rep(NA, nrow(params))

  param_loc = readLines(file.path(model_run_loc, param_file_name))

  for(i in 1:nrow(params)){
    cur_param_name = params$param_name[i]
    cur_model_idx = as.numeric(params$model_idx[i])

    cur_param_loc = grep(cur_param_name, param_loc) + cur_model_idx + 4

    row_number[i] = cur_param_loc
  }

  col_start_vec = rep(col_start, nrow(params))
  col_end_vec = rep(col_end, nrow(params))
  dec_places_vec = rep(dec_places, nrow(params))

  # specific format of param_file and param_ranges for hydroPSO - see https://www.rforge.net/hydroPSO/files/hydroPSO_vignette.pdf
  param_files_out = tibble(ParameterNmbr = par_number,
                           ParameterName = par_name,
                           Filename = file_name,
                           Row.Number = row_number,
                           Col.Start = col_start_vec,
                           Col.End = col_end_vec,
                           DecimalPlaces = dec_places_vec)

  params = left_join(params, param_ranges, by = c('param_name' = 'param'))

  param_ranges_out = tibble(ParameterNmbr = par_number,
                        ParameterName = par_name,
                        MinValue = params$min,
                        MaxValue = params$max)

  write.table(param_ranges_out, file = file.path(model_run_loc, param_range_file_out))
  write.table(param_files_out, file = file.path(model_run_loc, param_file_out))
}



