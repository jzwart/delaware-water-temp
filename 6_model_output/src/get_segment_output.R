

get_segment_from_lat_long = function(lat, long, model_fabric_file){

  model_fabric = readRDS(model_fabric_file)

  lat_long_df = tibble(lat = lat, long = long)

  lat_long_points <- purrr::map2(lat_long_df$long, lat_long_df$lat, function(latitude, longitude) {
    st_point(c(latitude, longitude), dim='XY')
  })
  lat_long_sf <- lat_long_df %>%
    st_set_geometry(st_sfc(lat_long_points)) %>%
    st_set_crs(4326)

  segs_wgs84 = st_transform(model_fabric$edges, crs = 4326)
  points_wgs84 = st_transform(lat_long_sf, crs = 4326)

  closest_seg = segs_wgs84[st_nearest_feature(points_wgs84, segs_wgs84),] %>% pull(seg_id_nat)

  return(closest_seg)
}

# getting segment output based on lat / long input - only takes one lat / long right now
get_segment_output = function(ind_file,
                              lat,
                              long,
                              model_fabric_file,
                              model_output_file,
                              gd_config = 'lib/cfg/gd_config.yml'){

  model_output = feather::read_feather(model_output_file)

  seg = get_segment_from_lat_long(lat = lat, long = long, model_fabric_file = model_fabric_file)

  seg_output = dplyr::filter(model_output, seg_id_nat == as.character(seg))

  seg_output_file = as_data_file(ind_file)
  saveRDS(seg_output, seg_output_file)
  gd_put(remote_ind = ind_file, local_source = seg_output_file, config_file = gd_config)
}
