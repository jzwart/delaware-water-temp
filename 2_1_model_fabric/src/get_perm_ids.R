
#' getting list of flowline and waterbody permanent identifiers
#'
#' @param ind_file indicator file for scipiper target
#' @param huc HUC your region resides in. This is set in the 'lib/cfg/model_settings.yml' file
#' @param layer The NHD layers for which you want permanent ID's. Defaults to NHDFlowline and NHDWaterbody
#' @param tmp_dir temporary directory for downloading NHD gpkg
#' @param gd_config configuration file for pushing to Google Drive. Defaults to 'lib/cfg/gd_config.yml'
get_perm_ids = function(ind_file,
                        huc,
                        layer = c('NHDFlowline', 'NHDWaterbody'),
                        tmp_dir = '2_1_model_fabric/tmp/',
                        gd_config = 'lib/cfg/gd_config.yml'){

  huc2 = substr(huc, 0, 2)

  nhd_gpkg_file = file.path(tmp_dir, huc2, paste0('nhdplus_', huc, '.gpkg'))

  all_out = tibble()
  if('NHDFlowline' %in% layer){
    nhd_flowline_perm_ids =  sf::read_sf(nhd_gpkg_file, 'NHDFlowline') %>%
      dplyr::pull(Permanent_Identifier) %>%
      as_tibble() %>%
      rename(Permanent_ID = value) %>%
      mutate(layer = 'Flowline')

    all_out = bind_rows(all_out, nhd_flowline_perm_ids)
    rm(nhd_flowline_perm_ids)
  }
  if('NHDWaterbody' %in% layer){
    nhd_waterbody_perm_ids =  sf::read_sf(nhd_gpkg_file, 'NHDWaterbody') %>%
      dplyr::pull(Permanent_Identifier) %>%
      as_tibble() %>%
      rename(Permanent_ID = value) %>%
      mutate(layer = 'Waterbody')

    all_out = bind_rows(all_out, nhd_waterbody_perm_ids)
    rm(nhd_waterbody_perm_ids)
  }

  saveRDS(object = all_out, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}
