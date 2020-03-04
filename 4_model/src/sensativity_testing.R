

# sensativity test to updating initial conditions
library(tidyverse)

source('4_model/src/run_sntemp.R')
source('4_model/src/update_sntemp.R')
source('4_model/src/get_sntemp_values.R')

dates = c('2014-06-01', '2014-06-02')
model_run_loc = '4_model/sens/'

run_sntemp(start = dates[1],
           stop = dates[1],
           spinup = T,
           restart = F, spinup_days = 20,
           model_run_loc = model_run_loc)

state_names = 'segment_delta_flow'
state_cv = 0.3

cur_states = get_sntemp_initial_states(state_names = state_names,
                                       model_run_loc = model_run_loc,
                                       ic_file = 'prms_ic.txt')

median_state = median(cur_states[[3]])

cur_states[,3] = cur_states[,3] + rnorm(n = nrow(cur_states),
                       mean = 0,
                       sd = median_state * state_cv)

file.copy(from = file.path(model_run_loc, 'prms_ic.txt'),
          to = file.path(model_run_loc, 'prms_ic_update.txt'), overwrite = T)

# update the states
update_sntemp_states(state_names = state_names,
                     updated_states = cur_states[[3]],
                     model_run_loc = model_run_loc,
                     ic_file = 'prms_ic_update.txt')


run_sntemp(start = dates[2],
           stop = dates[2],
           spinup = F,
           restart = T,
           model_run_loc = model_run_loc,
           var_init_file = 'prms_ic.txt')

orig_out = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                  model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

run_sntemp(start = dates[2],
           stop = dates[2],
           spinup = F,
           restart = T,
           model_run_loc = model_run_loc,
           var_init_file = 'prms_ic_update.txt')

modified_out = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                      model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

windows()
max = max(c(orig_out$water_temp, modified_out$water_temp), na.rm = T)
plot(orig_out$water_temp ~ modified_out$water_temp, ylim = c(0,max), xlim = c(0,max))
abline(0,1)
