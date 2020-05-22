
library(tidyverse)
library(igraph)
source('4_model/src/get_upstream_downstream_segs.R')


old_drivers = get_segment_drivers_return(
  model_run_loc = I('4_model_for_PGDL/tmp'),
  param_file = I('input/myparam.param'),
  start = '2015-01-01',
  stop = '2020-01-01')


new_drivers = get_segment_drivers_return(
  model_run_loc = I('2_2_model_drivers/tmp'),
  param_file = I('input/myparam.param'),
  start = '2015-01-01',
  stop = '2020-01-01')


compare = left_join(old_drivers,
                    new_drivers,
                    by = c('seg_id_nat', 'model_idx','date'),
                    suffix = c('_old','_new'))

windows()
ggplot(compare, aes(x = seg_tmin_air_old, y = seg_tmin_air_new)) +
  geom_point(alpha=0.1)

windows()
ggplot(compare, aes(x = seg_tmax_air_old, y = seg_tmax_air_new)) +
  geom_point(alpha=0.1)

windows()
ggplot(compare, aes(x = seg_prcp_old, y = seg_prcp_new)) +
  geom_point(alpha=0.1)

seg = dplyr::filter(compare, seg_id_nat == '2047') %>% select(-seg_id_nat) %>%
  pivot_longer(starts_with('seg'), names_to = 'driver', values_to = 'value')

windows()
ggplot(dplyr::filter(seg, driver %in% c('seg_tmin_air_old','seg_tmin_air_new')),
       aes(x = date, y = value, color = driver, group = driver)) +
  geom_line()


seg_1 = dplyr::filter(seg, date < as.Date('2017-01-01')) %>%
  group_by(driver, model_idx) %>%
  summarise(mean_val = mean(value, na.rm=T)) %>%
  ungroup()

seg_1

summ =

# calculating area-weighted average of hru for each segment
get_segment_drivers_return = function(model_run_loc = '4_model_for_PGDL/tmp',
                                      param_file = 'input/myparam.param',
                                      start,
                                      stop,
                                      model_fabric_file = 'GIS/Segments_subset.shp',
                                      n_hru = 765,
                                      n_segments = 456,
                                      gd_config = 'lib/cfg/gd_config.yml'){

  model_fabric = sf::read_sf(file.path(model_run_loc, model_fabric_file))

  model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                           model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  params = readLines(file.path(model_run_loc, param_file))

  # hru_segment is the parameter that tells which hru's map to which segments
  param_loc_start = grep('hru_segment', params) + 5
  param_loc_end = param_loc_start + n_hru - 1

  hru_to_seg_vec = params[param_loc_start:param_loc_end]

  # weight by HRU area (units are acres)
  param_loc_start = grep('hru_area', params) + 5
  param_loc_end = param_loc_start + n_hru - 1

  hru_area_vec = params[param_loc_start:param_loc_end]

  # read in driver data
  tmin = read.table(file.path(model_run_loc,'input/tmin.cbh'), skip = 3, sep = ' ')
  tmax = read.table(file.path(model_run_loc,'input/tmax.cbh'), skip = 3, sep = ' ')
  prcp = read.table(file.path(model_run_loc,'input/prcp.cbh'), skip = 3, sep = ' ')

  dates = as.Date(paste(tmin[,1],tmin[,2],tmin[,3]), format = '%Y %m %d')

  out = tibble()
  # map driver data to HRU and do area-weighted average of driver
  for(seg in unique(model_locations$model_idx)){
    cur_hrus = which(hru_to_seg_vec %in% seg)

    # some segments don't have HRU's contributing to it. In this case, follow what they do in
    #  prms-sntemp code: https://github.com/jzwart/prms/blob/f20c0bbb072222a31215dfe0cd45e35f186e8210/prms/stream_temp.f90#L818-L827
    #   - they first look upstream, then downstream, then offstream for closest segment
    #  https://github.com/jzwart/prms/blob/f20c0bbb072222a31215dfe0cd45e35f186e8210/prms/stream_temp.f90#L548-L554
    #
    if(length(cur_hrus) == 0){
      close_segs = get_direct_upstream_segs(seg_id_nat = model_locations$seg_id_nat[model_locations$model_idx == seg],
                                            model_run_loc = model_run_loc)
      close_segs_idx = model_locations$model_idx[model_locations$seg_id_nat %in% close_segs]
      cur_hrus = which(hru_to_seg_vec %in% close_segs_idx)
      if(length(close_segs) == 0 | length(cur_hrus) == 0){
        close_segs = get_direct_downstream_segs(seg_id_nat = model_locations$seg_id_nat[model_locations$model_idx == seg],
                                                model_run_loc = model_run_loc)
      }
      close_segs_idx = model_locations$model_idx[model_locations$seg_id_nat %in% close_segs]
      cur_hrus = which(hru_to_seg_vec %in% close_segs_idx)
    }

    cur_hru_areas = as.numeric(hru_area_vec[cur_hrus]) # in acres

    cur_hru_tmin = tmin[, (cur_hrus + 6)]
    cur_hru_tmax = tmax[, (cur_hrus + 6)]
    cur_hru_prcp = prcp[, (cur_hrus + 6)]

    area_weighted_tmin = matrixStats::rowWeightedMeans(as.matrix(cur_hru_tmin), w = cur_hru_areas)
    area_weighted_tmax = matrixStats::rowWeightedMeans(as.matrix(cur_hru_tmax), w = cur_hru_areas)
    area_weighted_prcp = matrixStats::rowWeightedMeans(as.matrix(cur_hru_prcp), w = cur_hru_areas)

    cur_out = tibble(model_idx = rep(seg, length(area_weighted_tmin)),
                     date = dates,
                     seg_tmin_air = area_weighted_tmin,
                     seg_tmax_air = area_weighted_tmax,
                     seg_prcp = area_weighted_prcp)

    out = bind_rows(out, cur_out)
  }

  drivers_out = left_join(model_locations, out, by = 'model_idx') %>%
    dplyr::filter(date >= as.Date(start) & date <= as.Date(stop))

  # conversion to metric
  inches_to_m = 0.0254
  drivers_out$seg_tmin_air = (drivers_out$seg_tmin_air - 32) * 5/ 9
  drivers_out$seg_tmax_air = (drivers_out$seg_tmax_air - 32) * 5/ 9
  drivers_out$seg_prcp = drivers_out$seg_prcp * inches_to_m

  return(drivers_out)
  # out_file = as_data_file(ind_file)
  # feather::write_feather(x = drivers_out, path = out_file)
  # gd_put(remote_ind = ind_file, local_source = out_file, config_file = gd_config)
}

