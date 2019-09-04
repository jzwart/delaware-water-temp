
#' Grabbing subsetted NHD flowline shapefile using the geographic boundary cutout
#'
#' @param ind_file indicator file for scipiper target
#' @param geo_boundary_file geographic region which you want to model pulled in first step of pipeline
#' @param huc HUC your region resides in. This is set in the 'lib/cfg/model_settings.yml' file
#' @param tmp_dir temporary directory for downloading NHD gpkg
#' @param gd_config configuration file for pushing to Google Drive. Defaults to 'lib/cfg/gd_config.yml'

get_nhd_flowline = function(ind_file,
                   geo_boundary_file,
                   huc = '0204',
                   tmp_dir = '2_1_model_fabric/tmp/',
                   gd_config = 'lib/cfg/gd_config.yml'){

  geo_boundary = readRDS(geo_boundary_file) %>% sf::st_transform(crs = 4269)

  huc2 = substr(huc, 0, 2)

  if(!dir.exists(file.path(tmp_dir, huc2))){
    # delaware is 0204
    donwload_dir = nhdplusTools::download_nhdplushr(nhd_dir = tmp_dir,
                                                    hu_list = huc)

    hr_data <- nhdplusTools::get_nhdplushr(hr_dir = donwload_dir,
                                           out_gpkg = file.path(donwload_dir, paste0('nhdplus_', huc, '.gpkg')),
                                           layers = c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody"))
  }else{
    hr_data = file.path(tmp_dir, huc2, paste0('nhdplus_', huc, '.gpkg'))
  }

  flowline = sf::read_sf(hr_data, 'NHDFlowline') %>%
    sf::st_transform(crs = sf::st_crs(geo_boundary))

  # this takes a long time. 90 mins for DRB flowlines and ~15mins for waterbodies.
  #    Seems to scale with number of objects in shapefile (e.g. flowline or waterbody)
  flowline_subset = sf::st_intersection(sf::st_zm(flowline), sf::st_zm(geo_boundary))

  saveRDS(object = flowline_subset, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}

#' Grabbing subsetted NHD waterbody shapefile using the geographic boundary cutout
#'
#' @param ind_file indicator file for scipiper target
#' @param geo_boundary_file geographic region which you want to model pulled in first step of pipeline
#' @param huc HUC your region resides in. This is set in the 'lib/cfg/model_settings.yml' file
#' @param tmp_dir temporary directory for downloading NHD gpkg
#' @param gd_config configuration file for pushing to Google Drive. Defaults to 'lib/cfg/gd_config.yml'

get_nhd_waterbody = function(ind_file,
                            geo_boundary_file,
                            huc = '0204',
                            tmp_dir = '2_1_model_fabric/tmp/',
                            gd_config = 'lib/cfg/gd_config.yml'){

  geo_boundary = readRDS(geo_boundary_file) %>% sf::st_transform(crs = 4269)

  huc2 = substr(huc, 0, 2)

  if(!dir.exists(file.path(tmp_dir, huc2))){
    # delaware is 0204
    donwload_dir = nhdplusTools::download_nhdplushr(nhd_dir = tmp_dir,
                                                    hu_list = huc)

    hr_data <- nhdplusTools::get_nhdplushr(hr_dir = donwload_dir,
                                           out_gpkg = file.path(donwload_dir, paste0('nhdplus_', huc, '.gpkg')),
                                           layers = c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody"))
  }else{
    hr_data = file.path(tmp_dir, huc2, paste0('nhdplus_', huc, '.gpkg'))
  }

  waterbody = sf::read_sf(hr_data, 'NHDWaterbody') %>%
    sf::st_transform(crs = sf::st_crs(geo_boundary))

  # this takes a long time. 90 mins for DRB flowlines and ~15mins for waterbodies.
  #    Seems to scale with number of objects in shapefile (e.g. flowline or waterbody)
  waterbody_subset = sf::st_intersection(sf::st_zm(waterbody), sf::st_zm(geo_boundary))

  saveRDS(object = waterbody_subset, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}
