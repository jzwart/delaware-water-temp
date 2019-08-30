

subset_obs = function(ind_file,
                      site_ids,
                      obs,
                      gd_config = 'lib/cfg/gd_config.yml'){

  site_ids = readRDS(site_ids)

  inventory = feather::read_feather('../2wp-temp-observations/2_nwis_pull/inout/nwis_dv_inventory.feather')

  obs = readRDS(obs)

  obs2 = obs %>%
    dplyr::filter(site_no %in% site_ids$Permanent_ID)

  saveRDS(object = , file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}
