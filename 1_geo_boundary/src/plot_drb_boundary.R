


library(sf)
library(ggplot2)

d = readRDS('2_1_model_fabric/out/nhd_waterbody_subset.rds')
dd = sf::read_sf('1_geo_boundary/in/DRB_Extent/DRB_Extent.shp')
flowlines = readRDS('2_1_model_fabric/out/nhd_flowline_subset.rds')

m = sf::st_as_sf(maps::map(database = 'state', region = c('new york', 'new jersey', 'penn', 'delaware'), fill = T, plot = F))

# ggplot() +
#   geom_sf(data = m) +
#   geom_sf(data = dplyr::select(d, geometry) , col ='blue', alpha = 0, size = 2) +
#   geom_sf(data = dplyr::select(dd, geometry) , col ='red', alpha = 0) +
#   theme_bw()


ggplot() +
  geom_sf(data = m) +
  geom_sf(data = dplyr::select(d, geom)%>%dplyr::slice(1:15000) , col ='blue') +
  geom_sf(data = dplyr::select(dd, geometry) , col ='red', alpha = 0) +
  theme_bw()


############## sites with observations #################

crosswalk = readRDS('3_observations/out/nwis_nhdhr_flowline_crosswalk.rds')

crosswalk$match = F
crosswalk$same_match = NA
for(i in 1:nrow(crosswalk)){
  cur = crosswalk[i,]

  if(!is.na(cur$nhd_comid_from_nwis_id) | !is.na(cur$nhd_comid_from_nwis_point)){
    crosswalk$match[i] = T
  }
  if(!is.na(cur$nhd_comid_from_nwis_id) & !is.na(cur$nhd_comid_from_nwis_point)){
    if(cur$nhd_comid_from_nwis_id == cur$nhd_comid_from_nwis_point){
      crosswalk$same_match[i] = T
    }else{
      crosswalk$same_match[i] = F
    }
  }
}

percent_match = sum(crosswalk$match) / nrow(crosswalk) * 100

percent_same_match = sum(crosswalk$same_match[!is.na(crosswalk$nhd_comid_from_nwis_id) & !is.na(crosswalk$nhd_comid_from_nwis_point)]) / nrow(crosswalk[!is.na(crosswalk$nhd_comid_from_nwis_id) & !is.na(crosswalk$nhd_comid_from_nwis_point),]) *100

sum(duplicated(crosswalk$nhd_comid_from_nwis_point)) / nrow(crosswalk)

obs_sites = flowlines %>%
  dplyr::filter(Permanent_Identifier %in% crosswalk$nhd_comid_from_nwis_point)

ggplot() +
  geom_sf(data = m) +
  geom_sf(data = dplyr::select(d, geom)%>%dplyr::slice(1:15000) , col ='blue') +
  geom_sf(data = dplyr::select(dd, geometry) , col ='red', alpha = 0) +
  theme_bw()

nhdplusV2_flowlines = nhdplusTools:::get_nhdplus_byid(comids = crosswalk$nhd_comid_from_nwis_point[1:100], layer = 'nhdflowline_network')

ggplot() +
  geom_sf(data = nhdplusV2_flowlines, col ='blue') +
  geom_sf(data = dplyr::select(dd, geometry) , col ='red', alpha = 0) +
  theme_bw()





