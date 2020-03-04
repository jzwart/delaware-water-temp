
library(dplyr)

# order of states in IC file
state_order = tibble(state_name = c('segment_delta_flow','seg_tave_water', 'gw_silo', 'ss_silo', 'gw_sum','ss_sum','gw_index','ss_index'),
                     row_idx = c(73,seq(77, 83)))

saveRDS(state_order, file = '4_model/cfg/state_order.rds')

