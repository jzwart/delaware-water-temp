

get_nhd_flowline = function(ind_file,
                   geo_boundary,
                   hu_list = '0204',
                   tmp_dir = '2_1_model_fabric/tmp/',
                   gd_config = 'lib/cfg/gd_config.yml'){

  geo_boundary = readRDS(geo_boundary)

  # delaware is 0204
  donwload_dir = nhdplusTools::download_nhdplushr(nhd_dir = tmp_dir,
                                   hu_list = '0204')

  hr_data <- nhdplusTools::get_nhdplushr(hr_dir = donwload_dir,
                                         out_gpkg = file.path(donwload_dir, 'nhdplus_0204.gpkg'),
                                         layers = c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody"))

  # waterbody = sf::read_sf(hr_data, 'NHDWaterbody')

  flowline = sf::read_sf(hr_data, 'NHDFlowline') %>%
    sf::st_transform(crs = sf::st_crs(geo_boundary))

  sf::st_intersects(sf::st_zm(flowline$geom), sf::st_zm(geo_boundary$geometry))


  gd_put(remote_ind = ind_file, local_source = , config_file = gd_config)
}
