


library(nhdplusTools)
library(dplyr)

# reading in current DV data from NWIS pull
sites = feather::read_feather('2_nwis_pull/inout/nwis_dv_inventory.feather') %>%
  select(site_no, dec_lat_va, dec_long_va) %>%
  distinct()

sites_subset = sites[1:200,] # subsetting for now for testing

crosswalk = pbapply::pblapply(sites_subset$site_no, function(cur_site){
  cur = dplyr::filter(sites_subset, site_no == cur_site)

  nldi_nwis = list(featureSource = "nwissite", featureID = paste0("USGS-", cur$site_no))
  nhd_comid_nwis = discover_nhdplus_id(nldi_feature = nldi_nwis)

  point = sf::st_sfc(sf::st_point(c(cur$dec_long_va, cur$dec_lat_va)), crs = 4326)
  nhd_comid_point = tryCatch(discover_nhdplus_id(point), error = function(e) nhd_comid_point = NA) # adding NA if throws error

  if(length(nhd_comid_nwis) == 0){ # if it doesn't match with nwis site, try with lat /long
    # point = sf::st_sfc(sf::st_point(c(cur$dec_long_va, cur$dec_lat_va)), crs = 4326)
    # nhd_comid_point = discover_nhdplus_id(point)
    nhd_comid_nwis = NA
  }
  cur_out = tibble(site_no = cur$site_no,
                   nhd_comid_nwis = as.character(nhd_comid_nwis),
                   nhd_comid_point = as.character(nhd_comid_point))
}) %>% bind_rows() %>% as_tibble()

