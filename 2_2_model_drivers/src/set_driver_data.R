


set_driver_data <- function(orig_driver_file, driver_file_loc, model_run_loc, driver_cv, n_en){
  # read in original, unmodified driver data
  orig_driver = read.table(orig_driver_file, skip = 3, sep = ' ')

  for(i in 1:n_en){
    mat = array(rnorm(n = nrow(orig_driver) * (ncol(orig_driver)-6),
                      mean = gather(orig_driver[,7:ncol(orig_driver)])$value,
                      sd = 2),
                dim = c(nrow(orig_driver), (ncol(orig_driver)-6)))

    orig_driver[,7:ncol(orig_driver)] = round(mat, digits = 2)

    write.table(orig_driver, file.path(model_run_loc, 'input', sprintf('%s_%s.cbh', i)), row.names = F, col.names = F)
    tmp = readLines(file.path(model_run_loc, 'input', sprintf('tmax_%s.cbh', i)))
    tmp = c('Written by Bandit', 'tmax 765', '########################################', tmp)
    writeLines(tmp, file.path(model_run_loc, 'input', sprintf('tmax_%s.cbh', i)))
  }

}

