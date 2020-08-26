
# this is only for segment based parameters (i.e. one parameter per segment )
get_init_sntemp_params = function(ind_file,
                                  param_names,
                                  model_run_loc,
                                  model_fabric_file = 'GIS/Segments_subset.shp',
                                  param_file = 'input/myparam.param',
                                  param_default_file = 'control/delaware.control.par_name',
                                  ensure_not_lower_bound = F,
                                  n_segments = 456,
                                  gd_config = 'lib/cfg/gd_config.yml'){

  params = readLines(file.path(model_run_loc, param_file))

  model_fabric = sf::read_sf(file.path(model_run_loc, model_fabric_file))

  # order by model_idx
  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  out = vector(mode = 'list', length = length(param_names))

  if(length(param_names) == 0){
    out = out
  }else{
    for(i in seq_along(param_names)){

      defaults = get_default_param_vals(param_name = param_names[i],
                                        model_run_loc = model_run_loc,
                                        param_default_file = param_default_file)

      param_loc_start = which(params == param_names[i]) + 4 + as.numeric(defaults$ndim)
      param_loc_end = param_loc_start + as.numeric(defaults$size) - 1

      cur_param_vals = params[param_loc_start:param_loc_end]

      if(ensure_not_lower_bound){
        if(any(as.numeric(cur_param_vals) <= as.numeric(defaults$min))){
          range = as.numeric(defaults$max) - as.numeric(defaults$min)
          # add quarter of range from min
          quarter = range / 4
          cur_param_vals[as.numeric(cur_param_vals) <= as.numeric(defaults$min)] = as.character(as.numeric(cur_param_vals[as.numeric(cur_param_vals) <= as.numeric(defaults$min)]) + quarter)
          if(defaults$type == '1'){
            cur_param_vals = as.character(round(as.numeric(cur_param_vals), digits = 0))
          }
        }
      }

      out[[i]] = cur_param_vals
      names(out)[i] = param_names[i]
    }
  }

  param_output_file = as_data_file(ind_file)
  saveRDS(out, param_output_file)
  gd_put(remote_ind = ind_file, local_source = param_output_file, config_file = gd_config)
}

