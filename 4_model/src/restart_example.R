
library(ggplot2)
library(dplyr)
library(tidyr)
source('4_model/src/run_sntemp.R')
source('4_model/src/get_sntemp_values.R')

starts = seq.Date(from = as.Date('2000-06-01'), to = as.Date('2000-06-20'), by = 'days')
stops = starts

model_output_file = '4_model/tmp/output/seg_tave_water.csv'
model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'

out = data.frame()

spinup_days = 20

for(i in 1:length(starts)){
  if(i == 1){
    # run spinup period if first timestep and save ending conditions for initial conditions in next timestep
    run_sntemp(start = starts[i], stop = stops[i], spinup = T, spinup_days = spinup_days, restart = T)
  }else{
    # PRMS-SNTemp run for a single day using ending conditions from previous model run (starts[i-1]) as
    #  initial conditions for current (starts[i]) model run
    run_sntemp(start = starts[i], stop = stops[i], spinup = F, restart = T)
  }

  # extracting temperature and discuarge from model run
  stream_temp = get_sntemp_temperature(model_output_file = model_output_file,
                                        model_fabric_file = model_fabric_file)

  stream_discharge = get_sntemp_discharge(model_output_file = '4_model/tmp/output/seg_outflow.csv',
                                           model_fabric_file = model_fabric_file)

  stream_temp = left_join(stream_temp, stream_discharge, by = c('seg_id_nat', 'model_idx', 'date'))

  out = rbind(out, dplyr::filter(stream_temp, as.character(date) == as.character(stops[i])))
}


# # running model w/o restarting to compare to restarts
run_sntemp(start = starts[1]-spinup_days-1, stop = stops[length(stops)], spinup = F, restart = F)
no_restart = get_sntemp_temperature(model_output_file = model_output_file,
                                     model_fabric_file = model_fabric_file) %>%
  dplyr::filter(date %in% out$date)

no_restart_discharge = get_sntemp_discharge(model_output_file = '4_model/tmp/output/seg_outflow.csv',
                                         model_fabric_file = model_fabric_file)

no_restart = left_join(no_restart, no_restart_discharge, by = c('seg_id_nat', 'model_idx', 'date'))


# plot temp and discharge for single segment to see how well restarts compare to model run w/o restarts
model_seg = '200'
seg = dplyr::filter(out, model_idx == model_seg)
seg_no_restart = dplyr::filter(no_restart, model_idx == model_seg)

plot(seg$water_temp ~ seg$date, type = 'l')
lines(seg_no_restart$water_temp ~ seg_no_restart$date, col ='red')

all_out = left_join(out, no_restart, by = c('model_idx', 'date'), suffix = c('', '_no_restart'))
plot(all_out$water_temp ~ all_out$water_temp_no_restart, ylim = c(0, 60), xlim = c(0, 60))
abline(0,1)

ggplot(all_out, aes(y = water_temp, x = water_temp_no_restart, group = model_idx)) +
  geom_smooth(method = 'lm', se = F, fullrange = F) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 2, col = 'red') +
  geom_point(alpha = .1) +
  theme_classic() +
  xlim(0,30) + ylim(0,30) +
  ylab('Water Temp With Restarts') +
  xlab('Water Temp Baseline Model Run')


ggplot(all_out, aes(y = discharge, x = discharge_no_restart, group = model_idx)) +
  geom_smooth(method = 'lm', se = F, fullrange = F) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 2, col = 'red') +
  geom_point(alpha = .1) +
  theme_classic() +
  ylab('Discharge With Restarts') +
  xlab('Discharge Baseline Model Run')

ggplot(all_out, aes(y = discharge, x = discharge_no_restart, group = model_idx)) +
  geom_smooth(method = 'lm', se = F, fullrange = F) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 2, col = 'red') +
  geom_point(alpha = .1) +
  theme_classic() +
  ylab('Discharge With Restarts') +
  xlab('Discharge Baseline Model Run') +
  ylim(c(0,1000)) +
  xlim(c(0,1000))

# temp and discharge values match exactly for restart and model run w/o restarts
Metrics::rmse(all_out$discharge, all_out$discharge_no_restart)
Metrics::rmse(all_out$water_temp, all_out$water_temp_no_restart)


plot(seg$discharge ~ seg$date, type = 'l', ylab= 'Discharge' ,xlab ='' , lwd =4)
lines(seg_no_restart$discharge ~ seg_no_restart$date, col ='red',lwd =2)
abline(v = seg$date, lty =2)

plot(seg$water_temp ~ seg$date, type = 'l', ylab= 'Water Temperature' ,xlab ='' , lwd =4)
lines(seg_no_restart$water_temp ~ seg_no_restart$date, col ='red', lwd =2)
abline(v = seg$date, lty =2)



