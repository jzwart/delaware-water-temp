

#' function for generating ensemble specific drivers based on uncertainty of drivers and mean driver data file
#'
#' @param orig_driver_file The location of the original, unmodified driver data
#' @param model_run_loc root folder for storing model run files
#' @param driver_name the name of the driver data to set - tmax, tmin, p
#' @param driver_sd the standard deviation of the driver data
#' @param n_en the number of model ensembles
set_driver_data <- function(orig_driver_file,
                            model_run_loc = '4_model/tmp',
                            driver_name,
                            driver_sd,
                            n_en){
  # read in original, unmodified driver data
  orig_driver = read.table(orig_driver_file, skip = 3, sep = ' ')

  for(i in 1:n_en$n_en){
    if(driver_name == 'prcp'){
      mat = array(rnorm(n = nrow(orig_driver) * (ncol(orig_driver)-6),
                        mean = gather(orig_driver[,7:ncol(orig_driver)])$value,
                        sd = driver_sd),
                  dim = c(nrow(orig_driver), (ncol(orig_driver)-6)))
      mat = ifelse(mat < 0, 0, mat)

      orig_driver[,7:ncol(orig_driver)] = round(mat, digits = 2)

      write.table(orig_driver,
                  file.path(model_run_loc, 'input', sprintf('%s_%s.cbh', driver_name, i)),
                  row.names = F, col.names = F)
      tmp = readLines(file.path(model_run_loc, 'input', sprintf('%s_%s.cbh', driver_name, i)))
      tmp = c('Written by Bandit', sprintf('%s 765', driver_name),
              '########################################', tmp)
      writeLines(tmp, file.path(model_run_loc, 'input', sprintf('%s_%s.cbh', driver_name, i)))
    }else{
      mat = array(rnorm(n = nrow(orig_driver) * (ncol(orig_driver)-6),
                        mean = gather(orig_driver[,7:ncol(orig_driver)])$value,
                        sd = driver_sd),
                  dim = c(nrow(orig_driver), (ncol(orig_driver)-6)))

      orig_driver[,7:ncol(orig_driver)] = round(mat, digits = 2)

      write.table(orig_driver,
                  file.path(model_run_loc, 'input', sprintf('%s_%s.cbh', driver_name, i)),
                  row.names = F, col.names = F)
      tmp = readLines(file.path(model_run_loc, 'input', sprintf('%s_%s.cbh', driver_name, i)))
      tmp = c('Written by Bandit', sprintf('%s 765', driver_name),
              '########################################', tmp)
      writeLines(tmp, file.path(model_run_loc, 'input', sprintf('%s_%s.cbh', driver_name, i)))
    }
  }

}

