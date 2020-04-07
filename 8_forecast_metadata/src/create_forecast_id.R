# forecast id generator

# this should be created every time a new forecast is generated
create_forecast_id <- function(forecast_project_id){

  Forecast_id = paste0(forecast_project_id$forecast_project_id, uuid::UUIDgenerate())
  return(Forecast_id)
}
