
library(dplyr)

# order of states in IC file
state_order = tibble(state_name = c('basin_ppt','basin_rain','basin_snow','basin_obs_ppt','basin_temp','basin_orad','basin_tmax','basin_tmin','solrad_tmax','solrad_tmin','seg_inflow','seg_outflow','basin_gwflow','segment_delta_flow','outflow_ts','seg_tave_water', 'gw_silo', 'ss_silo', 'gw_sum','ss_sum','gw_index','ss_index'),
                     row_idx = c(5,6,7,8,9,10,11,12,13,14,17,18,70,73,75,seq(77, 83)))

saveRDS(state_order, file = '4_model/cfg/state_order.rds')

