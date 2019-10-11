
#'run SNTemp model executable
#'
#'@param start start date for SNTemp model
#'@param stop stop date for SNTemp model
#'@param spinup T/F for whether or not to run a spinup period (this is necessary for most data assimilation appications)
#'@param spinup_days number of days of the spinup period. The spinup period will be from the start date - spinup_days to start date - 1
#'@param restart T/F for whether or not to initialize SNTemp based on end of previous run
#'@param control_file name of the control file to set variables
run_sntemp = function(start, stop, spinup = F, spinup_days = 730, restart = F, control_file = 'delaware.control'){

  if(spinup){
    print('Running spinup period...')
    run_spinup(spinup_days = spinup_days, start = start, control_file = control_file)
  }
  if(restart){
    ctrl = readLines(file.path('20190913_Delaware_streamtemp/control', control_file)) # read in control file

    init_vars_loc = grep('init_vars_from_file', ctrl) + 3
    save_vars_loc = grep('save_vars_to_file', ctrl) + 3
    ctrl[init_vars_loc] = '1'
    ctrl[save_vars_loc] = '1'

    writeLines(text = ctrl, con = file.path('20190913_Delaware_streamtemp/control', control_file))
  }else{
    ctrl = readLines(file.path('20190913_Delaware_streamtemp/control', control_file)) # read in control file

    init_vars_loc = grep('init_vars_from_file', ctrl) + 3
    save_vars_loc = grep('save_vars_to_file', ctrl) + 3
    ctrl[init_vars_loc] = '0'
    ctrl[save_vars_loc] = '1'

    writeLines(text = ctrl, con = file.path('20190913_Delaware_streamtemp/control', control_file))
  }

  set_start_stop(start = start , stop = stop, control_file = control_file)

  current.wd = getwd() # getting current project root wd to reset after running batch file

  setwd(file.path(current.wd, '20190913_Delaware_streamtemp/')) # set wd to where batch file lives
  shell('delaware.bat') # run batch file

  setwd(current.wd) # set wd back to root of project
}

run_spinup = function(spinup_days = 730, start, control_file = 'delaware.control'){

  # how many days before start date. end date should be one day before start date
  spinup_start = as.Date(start) - spinup_days - 1
  spinup_stop = as.Date(start) - 1

  ctrl = readLines(file.path('20190913_Delaware_streamtemp/control', control_file)) # read in control file

  init_vars_loc = grep('init_vars_from_file', ctrl) + 3
  save_vars_loc = grep('save_vars_to_file', ctrl) + 3

  start_year_loc = grep('start_time', ctrl) + 3
  start_month_loc = grep('start_time', ctrl) + 4
  start_day_loc = grep('start_time', ctrl) + 5

  stop_year_loc = grep('end_time', ctrl) + 3
  stop_month_loc = grep('end_time', ctrl) + 4
  stop_day_loc = grep('end_time', ctrl) + 5

  # update init_vars so that it doesn't read initial conditions file since this is spinup period for producing an initial condition file
  ctrl[init_vars_loc] = '0'
  ctrl[save_vars_loc] = '1'

  # update start / stop
  ctrl[start_year_loc] = as.character(lubridate::year(spinup_start))
  ctrl[start_month_loc] = as.character(lubridate::month(spinup_start))
  ctrl[start_day_loc] = as.character(lubridate::day(spinup_start))

  ctrl[stop_year_loc] = as.character(lubridate::year(spinup_stop))
  ctrl[stop_month_loc] = as.character(lubridate::month(spinup_stop))
  ctrl[stop_day_loc] = as.character(lubridate::day(spinup_stop))

  writeLines(text = ctrl, con = file.path('20190913_Delaware_streamtemp/control', control_file))

  current.wd = getwd() # getting current project root wd to reset after running batch file

  setwd(file.path(current.wd, '20190913_Delaware_streamtemp/')) # set wd to where batch file lives
  shell('delaware.bat') # run batch file

  setwd(current.wd) # set wd back to root of project
}



#'
#'@param start start date of the model run
#'@param stop stop date of the model run
#'@param control_file name of the control file
#'
set_start_stop = function(start, stop, control_file = 'delaware.control'){

  start = as.Date(start)
  stop = as.Date(stop)

  ctrl = readLines(file.path('20190913_Delaware_streamtemp/control', control_file)) # read in control file

  start_year_loc = grep('start_time', ctrl) + 3
  start_month_loc = grep('start_time', ctrl) + 4
  start_day_loc = grep('start_time', ctrl) + 5

  stop_year_loc = grep('end_time', ctrl) + 3
  stop_month_loc = grep('end_time', ctrl) + 4
  stop_day_loc = grep('end_time', ctrl) + 5

  # update start / stop
  ctrl[start_year_loc] = as.character(lubridate::year(start))
  ctrl[start_month_loc] = as.character(lubridate::month(start))
  ctrl[start_day_loc] = as.character(lubridate::day(start))

  ctrl[stop_year_loc] = as.character(lubridate::year(stop))
  ctrl[stop_month_loc] = as.character(lubridate::month(stop))
  ctrl[stop_day_loc] = as.character(lubridate::day(stop))

  writeLines(text = ctrl, con = file.path('20190913_Delaware_streamtemp/control', control_file))
}













starts = seq.Date(from = as.Date('2000-06-01'), to = as.Date('2000-08-10'), by = 'days')
stops = starts

model_output_file = '20190913_Delaware_streamtemp/output/seg_tave_water.csv'
model_fabric_file = '20190913_Delaware_streamtemp/GIS/Segments_subset.shp'

out = data.frame()

spinup_days = 730

for(i in 1:length(starts)){
  if(i == 1){
    # run spinup period if first timestep
    run_sntemp(start = starts[i], stop = stops[i], spinup = T, spinup_days = spinup_days, restart = T)
  }else{
    run_sntemp(start = starts[i], stop = stops[i], spinup = F, restart = T)
  }

  stream_temp = get_modeled_temperature(model_output_file = model_output_file,
                                        model_fabric_file = model_fabric_file)

  stream_discharge = get_modeled_discharge(model_output_file = '20190913_Delaware_streamtemp/output/seg_outflow.csv',
                                           model_fabric_file = model_fabric_file)

  stream_temp = left_join(stream_temp, stream_discharge, by = c('seg_id_nat', 'model_idx', 'date'))

  out = rbind(out, dplyr::filter(stream_temp, as.character(date) == as.character(stops[i])))
}


# seg = dplyr::filter(out, model_idx == '2')
# plot(seg$water_temp, type = 'l')


# running model w/o restarting to compare to restarts
run_sntemp(start = starts[1]-spinup_days-1, stop = stops[length(stops)], spinup = F, restart = F)
no_restart = get_modeled_temperature(model_output_file = model_output_file,
                                     model_fabric_file = model_fabric_file) %>%
  dplyr::filter(date %in% out$date)

no_restart_discharge = get_modeled_discharge(model_output_file = '20190913_Delaware_streamtemp/output/seg_outflow.csv',
                                         model_fabric_file = model_fabric_file)

no_restart = left_join(no_restart, no_restart_discharge, by = c('seg_id_nat', 'model_idx', 'date'))



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
  xlim(c(0,60)) +
  ylim(c(0,60)) +
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

Metrics::rmse(all_out$discharge, all_out$discharge_no_restart)
Metrics::rmse(all_out$water_temp, all_out$water_temp_no_restart)


plot(seg$discharge ~ seg$date, type = 'l', ylab= 'Discharge' ,xlab ='' )
lines(seg_no_restart$discharge ~ seg_no_restart$date, col ='red')

plot(seg$water_temp ~ seg$date, type = 'l')
lines(seg_no_restart$water_temp ~ seg_no_restart$date, col ='red')


