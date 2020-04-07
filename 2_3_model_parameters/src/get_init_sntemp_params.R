

get_init_sntemp_params = function(ind_file,
                                  param_names,
                                  model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp',
                                  param_file = 'delaware.control.param',
                                  n_segments = 456,
                                  gd_config = 'lib/cfg/gd_config.yml'){

  params = readLines(file.path('20191002_Delaware_streamtemp/control', param_file))

  model_fabric = sf::read_sf(model_fabric_file)

  # order by model_idx
  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  out = seg_ids

  if(length(param_names$params_to_cal) == 0){
    out = out
  }else{
    for(i in 1:length(param_names$params_to_cal)){
      param_loc_start = grep(param_names$params_to_cal[i], params) + 5
      param_loc_end = param_loc_start + n_segments - 1

      cur_param_vals = params[param_loc_start:param_loc_end]

      out = out %>%
        mutate(temp_name = cur_param_vals) %>%
        rename(!!noquote(param_names$params_to_cal[i]) := temp_name)
    }
  }

  param_output_file = as_data_file(ind_file)
  saveRDS(out, param_output_file)
  gd_put(remote_ind = ind_file, local_source = param_output_file, config_file = gd_config)
}

