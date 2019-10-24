


library(sf)
library(ggplot2)

d = readRDS('2_1_model_fabric/out/nhd_waterbody_subset.rds')
dd = sf::read_sf('1_geo_boundary/in/DRB_Extent/DRB_Extent.shp')
flowlines = readRDS('2_1_model_fabric/out/nhd_flowline_subset.rds')

m = sf::st_as_sf(maps::map(database = 'state',
                           region = c('new york', 'new jersey', 'penn', 'delaware', 'maryland'),
                           fill = T, plot = F))


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


###########
# SNTemp files

hru = sf::read_sf('20191002_Delaware_streamtemp/GIS/HRU_subset.shp')# %>%
  # dplyr::slice(seq(nrow(.) -10, nrow(.)))
seg = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') #%>%
  # dplyr::slice(seq(1,20))
seg_old = sf::read_sf('20190913_Delaware_streamtemp/GIS/Segments_subset.shp') #%>%

main_states = sf::st_as_sf(maps::map(database = 'state',
                           region = c('new york', 'new jersey', 'penn', 'delaware'),
                           fill = T, plot = F))
other_states = sf::st_as_sf(maps::map(database = 'state',
                                      region = c('maryland', 'west virginia','maine','virginia',
                                                 'ohio', 'vermont','new hampshire', 'massachusettes'),
                                      fill = T, plot = F))

windows()
base = ggplot() +
  # geom_sf(data = dd) +
  geom_sf(data = main_states) +
  geom_sf(data = other_states, fill ='white') +
  geom_sf(data = dplyr::select(dd, geometry) , col ='red', alpha = 0) +
  theme_minimal()

base

fabric = ggplot() +
  # geom_sf(data = hru, col = 'grey80', fill = 'tan') +
  geom_sf(data = seg, col = 'blue') +
  # geom_sf(data = seg_old, col = 'red') +
  theme_minimal()

fabric


output = read.csv('20191002_Delaware_streamtemp/output/seg_tave_water.csv', header = T)

colnames(output)

output = output[,-1]

range(output)
output_long = tidyr::gather(output)

hist(output_long[,2], xlim=c(-5,60))

ggplot(output_long) +
  geom_histogram(aes(x = value)) +
  xlim(c(-5,60))+
  theme_classic() +
  xlab('SNTemp Est. (C)')

density()



