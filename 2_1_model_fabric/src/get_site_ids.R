

get_site_ids = function(ind_file,
                        nhd_gpkg,
                        gd_config = 'lib/cfg/gd_config.yml'){

  nhd_flowline_site_ids =  sf::read_sf(nhd_gpkg, 'NHDFlowline') %>%
    dplyr::pull(Permanent_Identifier) %>%
    as_tibble() %>%
    rename(Permanent_ID = value)

  saveRDS(object = nhd_flowline_site_ids, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}
