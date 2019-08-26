

get_geo_boundary = function(ind_file,
                            region,
                            boundary_file_loc = '1_geo_boundary/in',
                            gd_config = 'lib/cfg/gd_config.yml'){


  geo_boundary = sf::read_sf(file.path(boundary_file_loc, region, paste0('drb_bnd_polygon', '.shp')))

  geo_boundary_file = as_data_file(ind_file)
  saveRDS(geo_boundary, geo_boundary_file)
  gd_put(remote_ind = ind_file, local_source = geo_boundary_file, config_file = gd_config)
}

