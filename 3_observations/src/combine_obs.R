


combine_obs = function(ind_file,
                       temp_obs_file,
                       flow_obs_file,
                       gd_config = 'lib/cfg/gd_config.yml'){

  temp_obs = readRDS(temp_obs_file)
  flow_obs = readRDS(flow_obs_file)

  out = list(temp = temp_obs, flow = flow_obs)

  saveRDS(out, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}
