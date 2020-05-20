archive_forecast <- function(working_directory,
                             folder,
                             forecast_base_name,
                             forecast_location,
                             push_to_git,
                             save_file_name,
                             time_of_forecast_string){
  
  ###ARCHIVE AND CLEAN UP FORECAST
  
  forecast_archive_dir_name <- paste0(save_file_name, "_", time_of_forecast_string)

  if(is.na(forecast_location)){
    forecast_archive_dir <- paste0(folder, "/", "Forecasts", "/" ,forecast_archive_dir_name)
  }else{
    forecast_archive_dir <- paste0(forecast_location, "/", forecast_archive_dir_name)
  }
  
  file.rename(from = paste0(forecast_location, "/", save_file_name, ".nc"), 
              to = paste0(forecast_location, "/", forecast_archive_dir_name, ".nc"))

  files <- list.files(paste0(working_directory), full.names = TRUE)
  files_pdf <- list.files(paste0(working_directory), pattern = ".pdf", full.names = TRUE)
  #files_nc <- list.files(paste0(working_directory),
  #                       pattern = paste0(forecast_archive_dir_name, ".nc"), 
  #                       full.names = TRUE)

  tmp <- file.copy(from = files_pdf, to = forecast_location)
  #tmp <- file.copy(from = files_nc, to = forecast_location)

  netcdf_name <- paste0(forecast_archive_dir_name, ".nc")

  if(push_to_git){
    setwd(forecast_location)
    system(paste0('git add ',netcdf_name))
    system('git commit -m "forecast" ')
    system("git push")
  }
  return(list(paste0(forecast_location, "/", netcdf_name)))
}