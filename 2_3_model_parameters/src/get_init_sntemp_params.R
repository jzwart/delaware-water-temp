

get_init_sntemp_params = function(out_file,
                                  param_names,
                                  model_fabric_file = '20190913_Delaware_streamtemp/GIS/Segments_subset.shp',
                                  param_file = 'delaware.control.param',
                                  n_segments = 456){

  params = readLines(file.path('20190913_Delaware_streamtemp/control', param_file))

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  out = seg_ids

  for(i in 1:length(param_names$params_to_cal)){
    param_loc_start = grep(param_names$params_to_cal[i], params) + 5
    param_loc_end = param_loc_start + n_segments - 1

    cur_param_vals = params[param_loc_start:param_loc_end]

    out = out %>%
      mutate(temp_name = cur_param_vals) %>%
      rename(!!noquote(param_names$params_to_cal[i]) := temp_name)
  }

  saveRDS(object = out, file = out_file)
}

