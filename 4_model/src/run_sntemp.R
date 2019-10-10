
#'
#'run SNTemp model executable
#'
run_sntemp = function(){
  current.wd = getwd() # getting current project root wd to reset after running batch file

  setwd(file.path(current.wd, '20190913_Delaware_streamtemp/')) # set wd to where batch file lives
  shell('delaware.bat') # run batch file

  setwd(current.wd) # set wd back to root of project
}


# run_sntemp()



#'
#'@param start start date of the model run
#'@param stop stop date of the model run
#'@param control_file name of the control file
#'
set_start_stop = function(start, stop, control_file){

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


set_start_stop(start = '1990-01-01' , stop = '2012-01-23',control_file = 'delaware.control')




