

#' function for appending newly pulled driver data onto previously existing driver data
#'
#' @param orig_driver_file The location of the original, unmodified driver data
#' @param new_driver_file The location of the new updated driver data (with most recent dates)
#' @param model_run_loc root folder for storing model run files
#' @param driver_name the name of the driver data to set - tmax, tmin, p
append_new_driver_data <- function(orig_driver_file,
                                   new_driver_file,
                                   model_run_loc,
                                   driver_name){
  # read in original, unmodified driver data
  orig_driver = read.table(orig_driver_file, skip = 3, sep = ' ')

  new_driver = read.table(new_driver_file, skip = 3, sep = ' ')

  new_driver = rbind(orig_driver, new_driver)

  write.table(new_driver,
              file.path(model_run_loc, 'input', sprintf('%s.cbh', driver_name)),
              row.names = F, col.names = F)
  tmp = readLines(file.path(model_run_loc, 'input', sprintf('%s.cbh', driver_name)))
  tmp = c('Written by Bandit', sprintf('%s 765', driver_name),
          '########################################', tmp)
  writeLines(tmp, file.path(model_run_loc, 'input', sprintf('%s.cbh', driver_name)))
}

# need to update sf_data after updating driver dates, otherwise model won't run
update_sf_data <- function(new_driver_file,
                           model_run_loc){

  new_driver = read.table(new_driver_file, skip = 3, sep = ' ')

  new_driver_dates = new_driver[, 1:6]
  new_driver_dates$V7 = -999

  write.table(new_driver_dates,
              file.path(model_run_loc, 'input/sf_data'),
              row.names = F, col.names = F)

  tmp = readLines(file.path(model_run_loc, 'input/sf_data'))
  heading = c('Created by Bandit',
              '/////////////////////////////////////////////////////////////////////////',
              '// Station IDs for runoff:',
              '// ID',
              '// 00000000',
              '/////////////////////////////////////////////////////////////////////////',
              '// Unit: runoff = cfs',
              '/////////////////////////////////////////////////////////////////////////',
              'runoff 0',
              '#########################################################')

  tmp = c(heading, tmp)

  writeLines(tmp, file.path(model_run_loc, 'input/sf_data'))
}

