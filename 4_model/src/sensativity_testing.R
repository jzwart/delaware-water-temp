

# sensativity test to updating initial conditions
library(tidyverse)

source('4_model/src/run_sntemp.R')
source('4_model/src/update_sntemp.R')
source('4_model/src/get_sntemp_values.R')

dates = c('2014-06-01', '2014-06-02')
model_run_loc = '4_model/sens/'
state_order = readRDS('4_model/cfg/state_order.rds')

# run_sntemp(start = dates[1],
#            stop = dates[1],
#            spinup = T,
#            restart = T, spinup_days = 700,
#            model_run_loc = model_run_loc,
#            var_init_file = 'prms_ic_spinup.txt',
#            var_save_file = 'prms_ic_spinup.txt')
# should only need to run spinup once ^

state_order

state_names = 'seg_tave_water'
by_seg = F
state_cv = 0.8

cur_states = get_sntemp_initial_states(state_names = state_names,
                                       by_seg = by_seg,
                                       model_run_loc = model_run_loc,
                                       ic_file = 'prms_ic_spinup.txt')

median_state = median(cur_states[[state_names]])
median_state = ifelse(median_state == 0, mean(cur_states[[state_names]]), median_state)

# forcing positive for sensativity
cur_states[,state_names] = abs(cur_states[,state_names] + rnorm(n = nrow(cur_states),
                       mean = 0,
                       sd = median_state * state_cv))

file.copy(from = file.path(model_run_loc, 'prms_ic_spinup.txt'),
          to = file.path(model_run_loc, 'prms_ic_orig.txt'), overwrite = T)
file.copy(from = file.path(model_run_loc, 'prms_ic_spinup.txt'),
          to = file.path(model_run_loc, 'prms_ic_update.txt'), overwrite = T)

# update the states
update_sntemp_states(state_names = state_names,
                     by_seg = by_seg,
                     updated_states = cur_states[[state_names]],
                     model_run_loc = model_run_loc,
                     ic_file = 'prms_ic_update.txt')


run_sntemp(start = dates[2],
           stop = dates[2],
           spinup = F,
           restart = T,
           model_run_loc = model_run_loc,
           var_init_file = 'prms_ic_orig.txt',
           var_save_file = 'prms_ic_orig.txt')

orig_out = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                  model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

run_sntemp(start = dates[2],
           stop = dates[2],
           spinup = F,
           restart = T,
           model_run_loc = model_run_loc,
           var_init_file = 'prms_ic_update.txt',
           var_save_file = 'prms_ic_update.txt')

modified_out = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                      model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

windows()
max = max(c(orig_out$water_temp, modified_out$water_temp), na.rm = T)
plot(orig_out$water_temp ~ modified_out$water_temp, ylim = c(0,max), xlim = c(0,max),
     main = paste('sensativity to',state_names))
text(paste('RMSE = ',
           round(Metrics::rmse(orig_out$water_temp[orig_out$water_temp>0],
                               modified_out$water_temp[orig_out$water_temp>0]),digits = 2), 'C'),
     x = max/5, y = max-(max/4))
abline(0,1)



# length(ic[[1]])
#
# ic_details = tibble(row = seq(1,length(ic)), n_states = unlist(lapply(ic, length)))
