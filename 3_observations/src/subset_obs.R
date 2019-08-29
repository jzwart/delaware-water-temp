

subset_obs = function(ind_file,
                      site_ids,
                      obs,
                      gd_config = 'lib/cfg/gd_config.yml'){

  # geo_boundary = readRDS(geo_boundary)


  saveRDS(object = , file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}
