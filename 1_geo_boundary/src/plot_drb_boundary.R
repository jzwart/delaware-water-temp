


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
  # geom_sf(data = dplyr::select(dd, geometry) , col ='red', alpha = 0) +
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



######################
# figures for GLEON
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)

seg = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') %>%
  mutate(seg_id_nat = as.character(seg_id_nat))
obs = readRDS('3_observations/in/obs_temp_full.rds')
boundary = sf::read_sf('1_geo_boundary/in/DRB_Extent/DRB_Extent.shp')
waterbodies = readRDS('2_1_model_fabric/out/nhd_waterbody_subset.rds') %>%
  dplyr::filter(FType %in% c(463, 390))

obs_agg = obs %>%
  group_by(seg_id_nat) %>%
  summarise(obs_dates = n()) %>%
  ungroup()

seg_obs = left_join(seg, obs_agg, by = "seg_id_nat") %>%
  mutate(obs_dates = ifelse(is.na(obs_dates), 0, obs_dates),
         obs_bins = cut(obs_dates, breaks = c(-1,0,10,100,1000,10000, 1000000000000),
                                     labels = c('0','1-10','11-100','101-1000','1001-10000','>10001')))


cols = viridis::viridis(n = 6, direction = -1)

col_labs = c('0' = 'grey', '1-10' = cols[2],'11-100' = cols[3],
             '101-1000' = cols[4],'1001-10000' = cols[5],'>10001' = cols[6])

main_states = sf::st_as_sf(maps::map(database = 'state',
                                     region = c('new york', 'new jersey', 'penn', 'delaware',
                                                'maryland', 'west virginia','maine','virginia',
                                                'ohio', 'vermont','new hampshire', 'massachusettes'),
                                     fill = T, plot = F))
us_map = sf::st_as_sf(maps::map(database = 'state', fill = T, plot = F, col = 'white', border = 'grey'))


seg_plot = ggplot() +
  geom_sf(data = main_states, fill = 'grey93', color = 'grey80') +
  # geom_sf(data = seg_obs, aes(col = obs_bins, fill = obs_bins), size = 1.1) +
  geom_sf(data = seg_obs, col = 'blue', fill = 'blue', size = 1.1) +
  geom_sf(data = boundary$geometry, col = 'black', alpha = 0) +
  #scale_color_manual(values = col_labs) +   scale_fill_manual(values = col_labs) +
  theme_minimal()+
  ylim(c(38.5,42.5))+
  xlim(c(76.5,74)) #+
  #guides(color=guide_legend(title="Days of observations"),fill=guide_legend(title="Days of observations"))

seg_plot
ggsave(filename = 'C:/Users/jzwart/Documents/Jake/Conferences/2019/GLEON/stream_map.png',
       plot = seg_plot, width = 6, height = 12, units = 'in', dpi = 400)

sites_v_obs = seg_obs %>% select(obs_dates) %>% arrange(obs_dates) %>% mutate(seq_id = seq(1,nrow(.))) %>%
  group_by(obs_dates) %>%
  summarise(n_sites = max(seq_id)) %>%
  ungroup() %>% mutate(n_sites = 457 - n_sites)

obs_days = ggplot(dplyr::filter(sites_v_obs, obs_dates >0), aes(x = obs_dates, y = n_sites)) +
  geom_line(size = 2) +
  scale_x_log10() +
  theme_classic() +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 26)) +
  ylab('Number of stream sites') +
  xlab('Days of observations')

obs_days
ggsave(filename = 'C:/Users/jzwart/Documents/Jake/Conferences/2019/GLEON/stream_obs_days.png',
       plot = obs_days, width = 8, height = 8, units = 'in', dpi = 400)


map_of_us = ggplot() +
  geom_sf(data = us_map, color = 'grey70', fill ='grey96') +
  geom_sf(data = boundary$geometry, col = 'black', fill = 'blue') +
  theme_minimal()

map_of_us
ggsave(filename = 'C:/Users/jzwart/Documents/Jake/Conferences/2019/GLEON/us_map.png',
       plot = map_of_us, width = 8, height = 8, units = 'in', dpi = 400)

lakes_sub = dplyr::filter(waterbodies, AreaSqKm >= 0.04)

lakes_map = ggplot() +
  geom_sf(data = main_states, fill = 'grey93', color = 'grey80') +
  geom_sf(data = lakes_sub, color = 'blue', fill = 'lightblue') +
  geom_sf(data = boundary$geometry, col = 'black', alpha = 0) +
  theme_minimal()+
  ylim(c(38.5,42.5))+
  xlim(c(76.5,74))

lakes_map
ggsave(filename = 'C:/Users/jzwart/Documents/Jake/Conferences/2019/GLEON/lakes_map.png',
       plot = lakes_map, width = 6, height = 12, units = 'in', dpi = 400)


# combine NWIS and WQP inventories

nwis_dv <- feather::read_feather('../2wp-temp-observations/2_nwis_pull/inout/nwis_dv_inventory.feather') %>%
  select(site_id = site_no, latitude = dec_lat_va, longitude = dec_long_va)

wqp <- feather::read_feather('../2wp-temp-observations/1_wqp_pull/inout/wqp_inventory.feather') %>%
  select(site_id = MonitoringLocationIdentifier, latitude, longitude)

nwis_uv <- feather::read_feather('../2wp-temp-observations/2_nwis_pull/inout/nwis_uv_inventory_reduced.feather') %>%
  select(site_id = site_no, latitude = dec_lat_va, longitude = dec_long_va)

sites <- bind_rows(nwis_dv, wqp, nwis_uv) %>%
  distinct()


collapsed_dat <- readRDS('../2wp-temp-observations/5_data_munge/out/daily_temperatures.rds') %>%
  left_join(sites) %>%
  group_by(site_id, latitude, longitude) %>%
  summarize(count = n()) %>%
  filter(!latitude > 49.3457868, !latitude < 24.7433195, !longitude < -124.7844079, !longitude > -66.9513812)


obs_site_points <- purrr::map2(collapsed_dat$longitude, collapsed_dat$latitude, function(latitude, longitude) {
  st_point(c(latitude, longitude), dim='XY')
})
collapsed_dat_sf <- collapsed_dat %>%
  st_set_geometry(st_sfc(obs_site_points)) %>%
  st_set_crs(4326) %>%
  st_transform(crs=st_crs(boundary$geometry))

drb_sites = collapsed_dat_sf[st_intersects(boundary, collapsed_dat_sf)[[1]], ]

drb_sites_trans = drb_sites %>%
  st_transform(crs=st_crs(lakes_sub$geom))

lakes_out = lakes_sub %>%
  mutate(obs_days = 0)
for(i in 1:length(lakes_out$Permanent_Identifier)){

  cur_lake = drb_sites_trans[st_intersects(lakes_out[i,], drb_sites_trans)[[1]], ]
  if(length(cur_lake$site_id) == 0){
    next
  }else{
    lakes_out$obs_days[i] = sum(cur_lake$count)
  }
}

lakes_obs = mutate(lakes_out, obs_bins = cut(obs_days, breaks = c(-1,0,10,100,1000,10000, 1000000000000),
                      labels = c('0','1-10','11-100','101-1000','1001-10000','>10001')))

cols = viridis::viridis(n = 6, direction = -1)

col_labs = c('0' = 'grey', '1-10' = cols[2],'11-100' = cols[3],
             '101-1000' = cols[4],'1001-10000' = cols[5],'>10001' = cols[6])

lakes_plot = ggplot() +
  geom_sf(data = main_states, fill = 'grey93', color = 'grey80') +
  geom_sf(data = lakes_obs, aes(col = obs_bins, fill = obs_bins)) +
  geom_sf(data = boundary$geometry, col = 'black', alpha = 0) +
  scale_color_manual(values = col_labs) +   scale_fill_manual(values = col_labs) +
  theme_minimal()+
  ylim(c(38.5,42.5))+
  xlim(c(76.5,74)) +
  guides(color=guide_legend(title="Days of observations"),fill=guide_legend(title="Days of observations"))

lakes_plot
ggsave(filename = 'C:/Users/jzwart/Documents/Jake/Conferences/2019/GLEON/lake_obs_map.png',
       plot = lakes_plot, width = 6, height = 12, units = 'in', dpi = 400)


lake_sites_v_obs = lakes_obs %>% select(obs_days) %>% arrange(obs_days) %>% mutate(seq_id = seq(1,nrow(.))) %>%
  group_by(obs_days) %>%
  summarise(n_sites = max(seq_id)) %>%
  ungroup() %>% mutate(n_sites = nrow(lakes_obs) - n_sites)

lake_obs_days = ggplot(dplyr::filter(lake_sites_v_obs, obs_days >0), aes(x = obs_days, y = n_sites)) +
  geom_line(size = 2) +
  geom_line(data= dplyr::filter(sites_v_obs, obs_dates >0), aes(x = obs_dates, y = n_sites), size = 2, col= 'grey') +
  scale_x_log10() +
  theme_classic() +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 26)) +
  ylab('Number of sites') +
  xlab('Days of observations')

lake_obs_days
ggsave(filename = 'C:/Users/jzwart/Documents/Jake/Conferences/2019/GLEON/lake_obs_days.png',
       plot = lake_obs_days, width = 8, height = 8, units = 'in', dpi = 400)



#example covariance matrix

covary_dat <- readRDS('../2wp-temp-observations/5_data_munge/out/daily_temperatures.rds') %>%
  dplyr::filter(site_id %in% drb_sites$site_id)

sites_summary = covary_dat %>%
  group_by(site_id) %>%
  summarise(obs_days = n()) %>% ungroup() %>% arrange(desc(obs_days))

sites = sites_summary$site_id[sample(x = 1:100, size = 40, replace = F)]

covary_dat_sub = dplyr::filter(covary_dat, site_id %in% sites) %>%
  spread(key = site_id, value = temp_degC)

cor_mat = matrix(NA,ncol = 40,nrow = 40)
for(i in 1:40){
  for(j in 1:40){
    cur_dat = na.omit(cbind(covary_dat_sub[,(i+3)], covary_dat_sub[,(j+3)]))

    cur_cor = cor(cur_dat[,1], cur_dat[,2])
    cor_mat[i,j] = cur_cor
  }
}

cor_mat = ifelse(is.na(cor_mat), 0, cor_mat)

library(plot.matrix)

plot(cor_mat, col = topo.colors)




######################
# figures for AGU 2020
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)

seg = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') %>%
  mutate(seg_id_nat = as.character(seg_id_nat))
obs = readRDS('3_observations/in/obs_temp_full.rds')
boundary = sf::read_sf('1_geo_boundary/in/DRB_Extent/DRB_Extent.shp')
waterbodies = readRDS('2_1_model_fabric/out/nhd_waterbody_subset.rds') %>%
  dplyr::filter(FType %in% c(463, 390))

obs_agg = obs %>%
  group_by(seg_id_nat) %>%
  summarise(obs_dates = n()) %>%
  ungroup()
obs_agg_forecast = obs %>%
  dplyr::filter(date >= as.Date('2019-03-01'), date <= as.Date('2019-09-01')) %>%
  group_by(seg_id_nat) %>%
  summarise(obs_dates = n()) %>%
  ungroup()

da_out = readRDS('4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[TRUE]_driver[TRUE]_init[TRUE].rds')
all_dates = da_out$dates
cur_model_idxs = da_out$model_locations$model_idx

seg_obs = left_join(seg, obs_agg, by = "seg_id_nat") %>%
  mutate(obs_dates = ifelse(is.na(obs_dates), 0, obs_dates),
         obs_bins = cut(obs_dates, breaks = c(-1,0,10,100,1000,10000, 1000000000000),
                        labels = c('0','1-10','11-100','101-1000','1001-10000','>10001')))
subset_seg_obs = seg_obs %>%
  dplyr::filter(as.character(model_idx) %in% cur_model_idxs)

seg_obs_forecast = left_join(seg, obs_agg_forecast, by = "seg_id_nat") %>%
  mutate(obs_dates = ifelse(is.na(obs_dates), 0, obs_dates),
         obs_bins = cut(obs_dates, breaks = c(-1,0,10,100,1000),
                        labels = c('0','1-10','11-100','>101')))
subset_seg_obs_forecast = seg_obs_forecast %>%
  mutate(model_idx = as.character(model_idx)) %>%
  dplyr::filter(model_idx %in% cur_model_idxs)


cols = viridis::viridis(n = 6, direction = -1)

col_labs = c('0' = 'grey', '1-10' = cols[2],'11-100' = cols[3],
             '101-1000' = cols[4],'1001-10000' = cols[5],'>10001' = cols[6])

main_states = sf::st_as_sf(maps::map(database = 'state',
                                     region = c('new york', 'new jersey', 'penn', 'delaware',
                                                'maryland', 'west virginia','maine','virginia',
                                                'ohio', 'vermont','new hampshire', 'massachusettes'),
                                     fill = T, plot = F))
us_map = sf::st_as_sf(maps::map(database = 'state', fill = T, plot = F, col = 'white', border = 'grey'))


seg_plot = ggplot() +
  geom_sf(data = main_states, fill = 'white', color = 'white') +
  # geom_sf(data = seg_obs, aes(col = obs_bins, fill = obs_bins), size = 1.1) +
  geom_sf(data = seg_obs, col = 'grey', fill = 'grey', size = 1) +
  geom_sf(data = subset_seg_obs, aes(col = obs_bins, fill = obs_bins), size = 3) +
  # geom_sf(data = boundary$geometry, col = 'black', alpha = 0) +
  scale_color_manual(name = '# of Obs', values = col_labs) +
  scale_fill_manual(name = '# of Obs', values = col_labs) +
  theme_minimal()+
  ylim(c(39.6,40.2))+
  xlim(c(76,75.5)) #+
#guides(color=guide_legend(title="Days of observations"),fill=guide_legend(title="Days of observations"))

seg_plot

col_labs = c('0' = 'grey', '1-10' = cols[2],'11-100' = cols[3],
             '>101' = cols[4],'1001-10000' = cols[5],'>10001' = cols[6])

seg_forecast_plot = ggplot() +
  geom_sf(data = main_states, fill = 'white', color = 'white') +
  # geom_sf(data = seg_obs, aes(col = obs_bins, fill = obs_bins), size = 1.1) +
  geom_sf(data = seg_obs, col = 'grey', fill = 'grey', size = 1) +
  geom_sf(data = subset_seg_obs_forecast, aes(col = obs_bins, fill = obs_bins), size = 3) +
  # geom_sf(data = boundary$geometry, col = 'black', alpha = 0) +
  scale_color_manual(name = '# of Obs', values = col_labs) +
  scale_fill_manual(name = '# of Obs', values = col_labs) +
  theme_minimal()+
  ylim(c(39.6,40.2))+
  xlim(c(76,75.5)) #+
#guides(color=guide_legend(title="Days of observations"),fill=guide_legend(title="Days of observations"))

seg_forecast_plot

# output = da_out$Y[1:42,,] %>%
#   reshape2::melt(varnames = c('model_idx', 'issue_time', 'ensemble')) %>%
#   mutate(issue_time = all_dates[issue_time],
#          model_idx = cur_model_idxs[model_idx],
#          valid_time = issue_time,
#          ensemble = ensemble) %>%
#   rename(seg_tave_water = value) %>%
#   as_tibble()
output = feather::read_feather('4_model_for_PGDL/out/sntemp_input_output_subset.feather') %>%
  select(model_idx, date, seg_tave_water)

example_day = output %>%
  dplyr::filter(date == as.Date('2019-07-01')) %>%
  group_by(model_idx) %>%
  summarise(seg_tave_water = mean(seg_tave_water)) %>%
  ungroup()
example_day = left_join(subset_seg_obs_forecast, example_day, by = c('model_idx'))

example_day_plot = ggplot() +
  geom_sf(data = main_states, fill = 'white', color = 'white') +
  # geom_sf(data = seg_obs, aes(col = obs_bins, fill = obs_bins), size = 1.1) +
  geom_sf(data = seg_obs, col = 'grey', fill = 'grey', size = 1) +
  geom_sf(data = example_day, aes(col = seg_tave_water, fill = seg_tave_water), size = 3) +
  # geom_sf(data = boundary$geometry, col = 'black', alpha = 0) +
  scale_color_gradient(name = 'Water Temp (C)') +
  scale_fill_gradient(name = 'Water Temp (C)') +
  theme_minimal()+
  ylim(c(39.6,40.2))+
  xlim(c(76,75.5)) #+
#guides(color=guide_legend(title="Days of observations"),fill=guide_legend(title="Days of observations"))

example_day_plot
