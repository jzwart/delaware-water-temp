

get_nhd_flowline = function(ind_file,
                   geo_boundary_file,
                   hu_list = '0204',
                   tmp_dir = '2_1_model_fabric/tmp/',
                   gd_config = 'lib/cfg/gd_config.yml'){

  geo_boundary = readRDS(geo_boundary_file) %>% sf::st_transform(crs = 4269)

  if(!dir.exists(file.path(tmp_dir, '02'))){
    # delaware is 0204
    donwload_dir = nhdplusTools::download_nhdplushr(nhd_dir = tmp_dir,
                                                    hu_list = hu_list)

    hr_data <- nhdplusTools::get_nhdplushr(hr_dir = donwload_dir,
                                           out_gpkg = file.path(donwload_dir, 'nhdplus_0204.gpkg'),
                                           layers = c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody"))
  }else{
    hr_data = file.path(tmp_dir, '02/nhdplus_0204.gpkg')
  }

  flowline = sf::read_sf(hr_data, 'NHDFlowline') %>%
    sf::st_transform(crs = sf::st_crs(geo_boundary))

  flowline_subset = sf::st_intersection(sf::st_zm(flowline), sf::st_zm(geo_boundary))

  saveRDS(object = flowline_subset, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}



get_nhd_waterbody = function(ind_file,
                            geo_boundary_file,
                            hu_list = '0204',
                            tmp_dir = '2_1_model_fabric/tmp/',
                            gd_config = 'lib/cfg/gd_config.yml'){

  geo_boundary = readRDS(geo_boundary_file) %>% sf::st_transform(crs = 4269)

  if(!dir.exists(file.path(tmp_dir, '02'))){
    # delaware is 0204
    donwload_dir = nhdplusTools::download_nhdplushr(nhd_dir = tmp_dir,
                                                    hu_list = hu_list)

    hr_data <- nhdplusTools::get_nhdplushr(hr_dir = donwload_dir,
                                           out_gpkg = file.path(donwload_dir, 'nhdplus_0204.gpkg'),
                                           layers = c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody"))
  }else{
    hr_data = file.path(tmp_dir, '02/nhdplus_0204.gpkg')
  }

  waterbody = sf::read_sf(hr_data, 'NHDWaterbody') %>%
    sf::st_transform(crs = sf::st_crs(geo_boundary))

  waterbody_subset = sf::st_intersection(sf::st_zm(waterbody), sf::st_zm(geo_boundary))

  saveRDS(object = waterbody_subset, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}
