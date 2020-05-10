# forecast id generator

# this should be created every time a new forecast is generated
create_forecast_id <- function(forecast_project_id){

  Forecast_id = paste(forecast_project_id$forecast_project_id, uuid::UUIDgenerate(), sep = '_')
  return(Forecast_id)
}
