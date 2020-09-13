

# function for writing template files for pestpp
write_pestpp_tpl_files = function(params,
                                  model_run_loc,
                                  param_file_name = 'input/myparam.param',
                                  param_default_file = 'control/delaware.control.par_name',
                                  param_file_out,
                                  delim,
                                  n_digits = 10,
                                  n_segments = 456,
                                  n_hrus = 765,
                                  n_gwr = 765,
                                  n_ssr = 765){

  params_out = readLines(file.path(model_run_loc, param_file_name))

  param_names = names(params)

  for(i in seq_along(param_names)){
    cur_defaults = get_default_param_vals(param_name = param_names[i],
                                          model_run_loc = model_run_loc,
                                          param_default_file = param_default_file)
    cur_params = params[[param_names[i]]]

    if(cur_defaults$dim == 'one'){
      cur_param_loc = which(params_out == param_names[i]) + 5

      params_out[cur_param_loc] = sprintf('%s%s  %s',
                                          delim,
                                          param_names[i],
                                          delim)
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nsegment'){ # a stream segment-based parameter
      # it will be ordered by segment model_idx (e.g. 1, 2, ..., nsegments)
      for(j in seq_len(nrow(cur_params))){
        cur_param_loc = which(params_out == param_names[i]) + 4 + as.numeric(cur_params$seg_model_idx[j])

        params_out[cur_param_loc] = sprintf('%s%s  %s',
                                            delim,
                                            paste(param_names[i], cur_params$seg_model_idx[j], sep = '_'),
                                            delim)
      }
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nhru'){
      # it will be ordered by hru model_idx (e.g. 1, 2, ..., nhru)
      for(j in seq_len(nrow(cur_params))){
        cur_param_loc = which(params_out == param_names[i]) + 4 + as.numeric(cur_params$hru_model_idx[j])

        params_out[cur_param_loc] = sprintf('%s%s  %s',
                                            delim,
                                            paste(param_names[i], cur_params$hru_model_idx[j], sep = '_'),
                                            delim)
      }
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'ngw'){
      # it will be ordered by hru model_idx (e.g. 1, 2, ..., nhru)
      for(j in seq_len(nrow(cur_params))){
        cur_param_loc = which(params_out == param_names[i]) + 4 + as.numeric(cur_params$hru_model_idx[j])

        params_out[cur_param_loc] = sprintf('%s%s  %s',
                                            delim,
                                            paste(param_names[i], cur_params$hru_model_idx[j], sep = '_'),
                                            delim)
      }
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nssr'){
      # it will be ordered by hru model_idx (e.g. 1, 2, ..., nhru)
      for(j in seq_len(nrow(cur_params))){
        cur_param_loc = which(params_out == param_names[i]) + 4 + as.numeric(cur_params$hru_model_idx[j])

        params_out[cur_param_loc] = sprintf('%s%s  %s',
                                            delim,
                                            paste(param_names[i], cur_params$hru_model_idx[j], sep = '_'),
                                            delim)
      }
    }else if(cur_defaults$ndim == '2'){
      if(grepl('nsegment', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
        # per segment x month basis is organized in order of segment model_idx and then month
        #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 455_Dec, 456_Dec
        for(j in seq_len(nrow(cur_params))){
          cur_param_loc = which(params_out == param_names[i]) + 5 + as.numeric(cur_params$seg_model_idx[j]) + (n_segments * (as.numeric(cur_params$month[j]) - 1))

          params_out[cur_param_loc] = sprintf('%s%s  %s',
                                              delim,
                                              paste(param_names[i], cur_params$seg_model_idx[j], cur_params$month[j], sep = '_'),
                                              delim)
        }
      }else if(grepl('nhru', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
        # per hru x month basis is organized in order of hru model_idx and then month
        #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 755_Dec, 756_Dec
        for(j in seq_len(nrow(cur_params))){
          cur_param_loc = which(params_out == param_names[i]) + 5 + as.numeric(cur_params$hru_model_idx[j]) + (n_hrus * (as.numeric(cur_params$month[j]) - 1))

          params_out[cur_param_loc] = sprintf('%s%s  %s',
                                              delim,
                                              paste(param_names[i], cur_params$hru_model_idx[j], cur_params$month[j], sep = '_'),
                                              delim)
        }
      }
    }
  }

  params_out = c(sprintf('ptf %s', delim), params_out)

  writeLines(params_out, file.path(model_run_loc, param_file_out))
}


