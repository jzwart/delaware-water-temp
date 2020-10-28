

get_calibrated_params_forecast = function(param_file,
                                          param_names,
                                          model_run_loc,
                                          n_en,
                                          seg_model_idxs,
                                          cal_params_list,
                                          param_default_file){

  par_cal = data.table::fread(param_file) %>% as_tibble()

  cur_params_to_cal = get_params_by_segment(param_names = param_names,
                                            model_run_loc = model_run_loc,
                                            seg_model_idxs = cur_model_idxs)
  n_cal_ensemble = nrow(par_cal)

  # if forecast ensemble is less than cal ensemble, then randomly pull calibrated param

  # store output in lists
  out = vector(mode = 'list', length = length(param_names))
  out_list_names = paste('out', seq(1, n_en, 1), sep = '_')
  # tmp = vector(mode = 'list', length = length(param_names))
  for(n in seq_along(out_list_names)){
    out[[n]] = cal_params_list
    names(out)[n] = out_list_names[n]
  }

  if(n_en < n_cal_ensemble){
    for(i in seq_along(param_names)){
      cur_defaults = get_default_param_vals(param_name = param_names[i],
                                            model_run_loc = model_run_loc,
                                            param_default_file = param_default_file)
      cur_params = cur_params_to_cal[[param_names[i]]]

      if(cur_defaults$dim == 'one'){
        param_name_out = param_names[i]
        cal_val = par_cal %>% slice(1:n_cal_ensemble) %>% pull(param_name_out) %>% sample(size = n_en, replace = F) %>% round(digits = 6)
        for(n in seq_len(n_en)){
          out[[n]][[param_names[i]]][1] = cal_val[n] # assigning random cal val to given ensemble
        }
      }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nsegment'){ # a stream segment-based parameter
        for(j in seq_len(nrow(cur_params))){
          param_name_out = paste(param_names[i], cur_params$seg_model_idx[j], sep = '_')
          cal_val = par_cal %>% slice(1:n_cal_ensemble) %>% pull(param_name_out) %>% sample(size = n_en, replace = F) %>% round(digits = 6)
          for(n in seq_len(n_en)){
            out[[n]][[param_names[i]]][as.numeric(cur_params$seg_model_idx[j])] = cal_val[n] # assigning random cal val to given ensemble
          }
        }
      }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nhru'){
        for(j in seq_len(nrow(cur_params))){
          param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], sep = '_')
          cal_val = par_cal %>% slice(1:n_cal_ensemble) %>% pull(param_name_out) %>% sample(size = n_en, replace = F) %>% round(digits = 6)
          for(n in seq_len(n_en)){
            out[[n]][[param_names[i]]][as.numeric(cur_params$hru_model_idx[j])] = cal_val[n] # assigning random cal val to given ensemble
          }
        }
      }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'ngw'){
        for(j in seq_len(nrow(cur_params))){
          param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], sep = '_')
          cal_val = par_cal %>% slice(1:n_cal_ensemble) %>% pull(param_name_out) %>% sample(size = n_en, replace = F) %>% round(digits = 6)
          for(n in seq_len(n_en)){
            out[[n]][[param_names[i]]][as.numeric(cur_params$hru_model_idx[j])] = cal_val[n] # assigning random cal val to given ensemble
          }
        }
      }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nssr'){
        for(j in seq_len(nrow(cur_params))){
          param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], sep = '_')
          cal_val = par_cal %>% slice(1:n_cal_ensemble) %>% pull(param_name_out) %>% sample(size = n_en, replace = F) %>% round(digits = 6)
          for(n in seq_len(n_en)){
            out[[n]][[param_names[i]]][as.numeric(cur_params$hru_model_idx[j])] = cal_val[n] # assigning random cal val to given ensemble
          }
        }
      }else if(cur_defaults$ndim == '2'){
        if(grepl('nsegment', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
          # per segment x month basis is organized in order of segment model_idx and then month
          #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 455_Dec, 456_Dec
          for(j in seq_len(nrow(cur_params))){
            param_name_out = paste(param_names[i], cur_params$seg_model_idx[j], cur_params$month[j], sep = '_')
            cal_val = par_cal %>% slice(1:n_cal_ensemble) %>% pull(param_name_out) %>% sample(size = n_en, replace = F) %>% round(digits = 6)
            for(n in seq_len(n_en)){
              out[[n]][[param_names[i]]][as.numeric(cur_params$seg_model_idx[j]) + (as.numeric(cur_params$month[j]) - 1)*n_segments] = cal_val[n] # assigning random cal val to given ensemble
            }
          }
        }else if(grepl('nhru', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
          # per hru x month basis is organized in order of hru model_idx and then month
          #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 755_Dec, 756_Dec
          for(j in seq_len(nrow(cur_params))){
            param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], cur_params$month[j], sep = '_')
            cal_val = par_cal %>% slice(1:n_cal_ensemble) %>% pull(param_name_out) %>% sample(size = n_en, replace = F) %>% round(digits = 6)
            for(n in seq_len(n_en)){
              out[[n]][[param_names[i]]][as.numeric(cur_params$hru_model_idx[j]) + (as.numeric(cur_params$month[j]) - 1)*n_hrus] = cal_val[n] # assigning random cal val to given ensemble
            }
          }
        }
      }
    }
  }

  return(out)
}

