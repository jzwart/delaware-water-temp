#' Matching NWIS site ID to NHD HighRes Flowline Permanent ID (or COMID)
#'
#' @param ind_file indicator file for scipiper target
#' @param nwis_sites_file file of nwis sites with observations
#' @param gd_config configuration file for pushing to Google Drive. Defaults to 'lib/cfg/gd_config.yml'
nwis_to_nhdhr_flowline = function(ind_file, nwis_uv_sites_file, nwis_dv_sites_file, gd_config = 'lib/cfg/gd_config.yml'){

  # reading in current UV data from NWIS pull
  nwis_uv_sites = feather::read_feather(nwis_uv_sites_file) %>%
    select(site_no, dec_lat_va, dec_long_va) %>%
    distinct()

  # reading in current DV data from NWIS pull
  nwis_dv_sites = feather::read_feather(nwis_dv_sites_file) %>%
    select(site_no, dec_lat_va, dec_long_va) %>%
    distinct()

  nwis_sites = bind_rows(nwis_uv_sites, nwis_dv_sites) %>%
    distinct()

  # nwis_sites = nwis_sites[1:2,] # subsetting for now for testing

  crosswalk = pbapply::pblapply(nwis_sites$site_no, function(cur_site){
    cur = dplyr::filter(nwis_sites, site_no == cur_site)

    nldi_nwis = list(featureSource = "nwissite", featureID = paste0("USGS-", cur$site_no))
    nhd_comid_nwis = discover_nhdplus_id(nldi_feature = nldi_nwis)

    point = sf::st_sfc(sf::st_point(c(cur$dec_long_va, cur$dec_lat_va)), crs = 4326)
    nhd_comid_point = tryCatch(discover_nhdplus_id(point), error = function(e) nhd_comid_point = NA) # adding NA if throws error

    if(length(nhd_comid_nwis) == 0){ # if it doesn't match with nwis site, try with lat /long
      # point = sf::st_sfc(sf::st_point(c(cur$dec_long_va, cur$dec_lat_va)), crs = 4326)
      # nhd_comid_point = discover_nhdplus_id(point)
      nhd_comid_nwis = NA
    }
    cur_out = tibble(nwis_site_no = cur$site_no,
                     nhd_comid_from_nwis_id = as.character(nhd_comid_nwis),
                     nhd_comid_from_nwis_point = as.character(nhd_comid_point))
  }) %>% bind_rows() %>% as_tibble()

  saveRDS(object = crosswalk, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file, local_source = as_data_file(ind_file), config_file = gd_config)
}

