


# plan for converting NOAA GEFS forecasts into driver data for PRMS.
# 1) find max temp, min temp, and accumulated precipitation at daily time scale. forecast resolution is in 6 to 3 hr valid times so need to aggregate those forecast times to daily
# 2) use each 11 ensemble as a daily forecast and draw from mean and sd of daily spread to fill out rest of PRMS-SNTemp ensembles
# 3) put in output in netcdf - will pull from this when running forecasts


## NOAA GEFS archive
library(ncdf4)
library(tidyverse)
library(ggplot2)
library(sf)
library(raster)
library(exactextractr)
source('4_model/src/EnKF_functions.R')
source('4_model_forecast/src/nc_forecast_utils.R')
start = '2019-01-01'
stop = '2020-01-01'
time_step = 'days'
n_en = 20
start = as.Date(as.character(start))
stop = as.Date(as.character(stop))
dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
forecast_horizon = 8 # days
#K_to_F =  # conversion for Kelvin to Farenheight
# extracting weights for cells
hrus = sf::read_sf('20191002_Delaware_streamtemp/GIS/HRU_subset.shp') # polygons
crs = "+init=epsg:4326"
hrus = sf::st_transform(hrus, crs) %>% arrange(model_idx)

nc_file = '2_2_model_drivers/in/NOAA GEFS/tmin_2m_gaussian_all_20190101_20200831_jzwaIGwGA2.nc'
forecast_driver <- nc_open(filename = nc_file)

temp <- ncvar_get(nc = forecast_driver, varid = 'Minimum_temperature') # air Temp in Kelvin; should be five dimensions [lon, lat, forecast hours, ensemble, issue date]
issue_time <- ncvar_get(nc = forecast_driver, varid = 'intTime') # forecast issue time in YYYYMMDDHH; one dimension
valid_time <- ncvar_get(nc = forecast_driver, varid = 'intValidTime') # forecast valid time in YYYYMMDDHH; should be two dimensions, forecast hours x issue date
lon = ncvar_get(nc = forecast_driver, varid = 'lon')
lon = -(360-lon) # converting to E / W
lat = ncvar_get(nc = forecast_driver, varid = 'lat')
ens = ncvar_get(nc = forecast_driver, varid = 'ens')
time = ncvar_get(nc = forecast_driver, varid = 'time')
fhours = ncvar_get(nc = forecast_driver, varid = 'fhour')
fillval = ncatt_get(forecast_driver, 'Minimum_temperature', '_FillValue')

dim(temp)

nc_close(forecast_driver)

# change any fill value to NA
temp[temp == fillval$value]= NA

dates_nc = strftime(strptime(dates, format ='%Y-%m-%d'), format = '%Y%m%d%H')
forecast_days = seq(0,forecast_horizon-1, 1)

# 1) find max temp, min temp, and accumulated precipitation at daily time scale. forecast resolution is in 6 to 3 hr valid times so need to aggregate those forecast times to daily
for(t in seq_along(dates_nc)){
  cur_issue_time = which(dates_nc[t] == as.character(issue_time))
  for(n in seq(1,n_en,1)){
    out_df = tibble()
    cur_en = n
    for(f in forecast_days){
      cur_day_hours = f * 24 + 24
      cur_fhours = which(as.numeric(fhours) <= cur_day_hours & as.numeric(fhours) >= cur_day_hours-23)
      if(cur_en < 12){ # if less than number of GEFS ensembles, then use the ensemble number
        cur_temp = temp[,,cur_fhours, cur_en, cur_issue_time]
        # take min, max, or sum of values depending on driver (e.g. take min if calculating min temp)
        cur_temp = apply(cur_temp, MARGIN = c(1,2), 'min')
        cur_temp = (cur_temp - 273.15) * 9/5 + 32 # convert from K to F

        cur_temp = raster(t(cur_temp), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
                          crs = CRS(crs))
        cur_temp = flip(cur_temp, direction = 'y')

        w_mean = exactextractr::exact_extract(cur_temp, hrus, 'weighted_mean', weights = area(cur_temp)) # calculating the weighted mean of the driver data (per hru)
        print(sprintf('%s, %s', dates[t], cur_en))

        cur_out_df = tibble(date = dates[t], hru_model_idx = hrus$model_idx, tmin = w_mean)

      }else{ # otherwise use mean and sd of driver spread to get value
        cur_temp = temp[,,cur_fhours, , cur_issue_time]
        cur_mean_temp = apply(cur_temp, MARGIN = c(1,2,3), 'mean')
        cur_sd_temp = apply(cur_temp, MARGIN = c(1,2,3), 'sd')
        n_out = dim(cur_mean_temp)[1] * dim(cur_mean_temp)[2] * dim(cur_mean_temp)[3]
        cur_temp = array(rnorm(n = n_out, mean = cur_mean_temp, sd = cur_sd_temp), dim = dim(cur_mean_temp))
        # take min, max, or sum of values depending on driver (e.g. take min if calculating min temp)
        cur_temp = apply(cur_temp, MARGIN = c(1,2), 'min')
        cur_temp = (cur_temp - 273.15) * 9/5 + 32 # convert from K to F

        cur_temp = raster(t(cur_temp), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
                          crs = CRS(crs))
        cur_temp = flip(cur_temp, direction = 'y')

        w_mean = exactextractr::exact_extract(cur_temp, hrus, 'weighted_mean', weights = area(cur_temp)) # calculating the weighted mean of the driver data (per hru)
        print(sprintf('%s, %s', dates[t], cur_en))

        cur_out_df = tibble(date = dates[t], hru_model_idx = hrus$model_idx, tmin = w_mean)

      }
      out_df = bind_rows(out_df, cur_out_df)
    }
    out_df = arrange(out_df, date, as.numeric(hru_model_idx))
    nc_drivers_put(var_df = out_df,
                   var_name = 'tmin',
                   en = n,
                   issue_date = dates[t],
                   nc_name_out = '2_2_model_drivers/out/forecasted_drivers.nc')
  }
}




test = nc_drivers_get(nc_file = '2_2_model_drivers/out/forecasted_drivers.nc',
                      ens = c(2,10), fdays = c(0,1,2),
                      issue_dates = '2019-03-08')

test

# tmax

## NOAA GEFS archive
library(ncdf4)
library(tidyverse)
library(ggplot2)
library(sf)
library(raster)
library(exactextractr)
source('4_model/src/EnKF_functions.R')
source('4_model_forecast/src/nc_forecast_utils.R')
start = '2019-01-01'
stop = '2020-01-01'
time_step = 'days'
n_en = 20
start = as.Date(as.character(start))
stop = as.Date(as.character(stop))
dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
forecast_horizon = 8 # days
#K_to_F =  # conversion for Kelvin to Farenheight
# extracting weights for cells
hrus = sf::read_sf('20191002_Delaware_streamtemp/GIS/HRU_subset.shp') # polygons
crs = "+init=epsg:4326"
hrus = sf::st_transform(hrus, crs) %>% arrange(model_idx)

nc_file = '2_2_model_drivers/in/NOAA GEFS/tmax_2m_latlon_all_20190101_20200831_jzwaCL7vW_.nc'
forecast_driver <- nc_open(filename = nc_file)

temp <- ncvar_get(nc = forecast_driver, varid = 'Maximum_temperature') # air Temp in Kelvin; should be five dimensions [lon, lat, forecast hours, ensemble, issue date]
issue_time <- ncvar_get(nc = forecast_driver, varid = 'intTime') # forecast issue time in YYYYMMDDHH; one dimension
valid_time <- ncvar_get(nc = forecast_driver, varid = 'intValidTime') # forecast valid time in YYYYMMDDHH; should be two dimensions, forecast hours x issue date
lon = ncvar_get(nc = forecast_driver, varid = 'lon')
lon = -(360-lon) # converting to E / W
lat = ncvar_get(nc = forecast_driver, varid = 'lat')
ens = ncvar_get(nc = forecast_driver, varid = 'ens')
time = ncvar_get(nc = forecast_driver, varid = 'time')
fhours = ncvar_get(nc = forecast_driver, varid = 'fhour')
fillval = ncatt_get(forecast_driver, 'Maximum_temperature', '_FillValue')

dim(temp)

nc_close(forecast_driver)

# change any fill value to NA
temp[temp == fillval$value]= NA

dates_nc = strftime(strptime(dates, format ='%Y-%m-%d'), format = '%Y%m%d%H')
forecast_days = seq(0,forecast_horizon-1, 1)

# 1) find max temp, min temp, and accumulated precipitation at daily time scale. forecast resolution is in 6 to 3 hr valid times so need to aggregate those forecast times to daily
for(t in seq_along(dates_nc)){
  cur_issue_time = which(dates_nc[t] == as.character(issue_time))
  for(n in seq(1,n_en,1)){
    out_df = tibble()
    cur_en = n
    for(f in forecast_days){
      cur_day_hours = f * 24 + 24
      cur_fhours = which(as.numeric(fhours) <= cur_day_hours & as.numeric(fhours) >= cur_day_hours-23)
      if(cur_en < 12){ # if less than number of GEFS ensembles, then use the ensemble number
        cur_temp = temp[,,cur_fhours, cur_en, cur_issue_time]
        # take min, max, or sum of values depending on driver (e.g. take min if calculating min temp)
        cur_temp = apply(cur_temp, MARGIN = c(1,2), 'max')
        cur_temp = (cur_temp - 273.15) * 9/5 + 32 # convert from K to F

        cur_temp = raster(t(cur_temp), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
                          crs = CRS(crs))
        cur_temp = flip(cur_temp, direction = 'y')

        w_mean = exactextractr::exact_extract(cur_temp, hrus, 'weighted_mean', weights = area(cur_temp)) # calculating the weighted mean of the driver data (per hru)
        print(sprintf('%s, %s', dates[t], cur_en))

        cur_out_df = tibble(date = dates[t], hru_model_idx = hrus$model_idx, tmax = w_mean)

      }else{ # otherwise use mean and sd of driver spread to get value
        cur_temp = temp[,,cur_fhours, , cur_issue_time]
        cur_mean_temp = apply(cur_temp, MARGIN = c(1,2,3), 'mean')
        cur_sd_temp = apply(cur_temp, MARGIN = c(1,2,3), 'sd')
        n_out = dim(cur_mean_temp)[1] * dim(cur_mean_temp)[2] * dim(cur_mean_temp)[3]
        cur_temp = array(rnorm(n = n_out, mean = cur_mean_temp, sd = cur_sd_temp), dim = dim(cur_mean_temp))
        # take min, max, or sum of values depending on driver (e.g. take min if calculating min temp)
        cur_temp = apply(cur_temp, MARGIN = c(1,2), 'max')
        cur_temp = (cur_temp - 273.15) * 9/5 + 32 # convert from K to F

        cur_temp = raster(t(cur_temp), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
                          crs = CRS(crs))
        cur_temp = flip(cur_temp, direction = 'y')

        w_mean = exactextractr::exact_extract(cur_temp, hrus, 'weighted_mean', weights = area(cur_temp)) # calculating the weighted mean of the driver data (per hru)
        print(sprintf('%s, %s', dates[t], cur_en))

        cur_out_df = tibble(date = dates[t], hru_model_idx = hrus$model_idx, tmax = w_mean)

      }
      out_df = bind_rows(out_df, cur_out_df)
    }
    out_df = arrange(out_df, date, as.numeric(hru_model_idx))
    nc_drivers_put(var_df = out_df,
                   var_name = 'tmax',
                   en = n,
                   issue_date = dates[t],
                   nc_name_out = '2_2_model_drivers/out/forecasted_drivers.nc')
  }
}



# prcp

## NOAA GEFS archive
library(ncdf4)
library(tidyverse)
library(ggplot2)
library(sf)
library(raster)
library(exactextractr)
source('4_model/src/EnKF_functions.R')
source('4_model_forecast/src/nc_forecast_utils.R')
start = '2019-01-01'
stop = '2020-01-01'
time_step = 'days'
n_en = 20
start = as.Date(as.character(start))
stop = as.Date(as.character(stop))
dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
forecast_horizon = 8 # days
#K_to_F =  # conversion for Kelvin to Farenheight
# extracting weights for cells
hrus = sf::read_sf('20191002_Delaware_streamtemp/GIS/HRU_subset.shp') # polygons
crs = "+init=epsg:4326"
hrus = sf::st_transform(hrus, crs) %>% arrange(model_idx)

nc_file = '2_2_model_drivers/in/NOAA GEFS/apcp_sfc_gaussian_all_20190101_20200831_jzway3plqx.nc'
forecast_driver <- nc_open(filename = nc_file)

prcp <- ncvar_get(nc = forecast_driver, varid = 'Total_precipitation') # air Temp in Kelvin; should be five dimensions [lon, lat, forecast hours, ensemble, issue date]
issue_time <- ncvar_get(nc = forecast_driver, varid = 'intTime') # forecast issue time in YYYYMMDDHH; one dimension
valid_time <- ncvar_get(nc = forecast_driver, varid = 'intValidTime') # forecast valid time in YYYYMMDDHH; should be two dimensions, forecast hours x issue date
lon = ncvar_get(nc = forecast_driver, varid = 'lon')
lon = -(360-lon) # converting to E / W
lat = ncvar_get(nc = forecast_driver, varid = 'lat')
ens = ncvar_get(nc = forecast_driver, varid = 'ens')
time = ncvar_get(nc = forecast_driver, varid = 'time')
fhours = ncvar_get(nc = forecast_driver, varid = 'fhour')
fillval = ncatt_get(forecast_driver, 'Total_precipitation', '_FillValue')

dim(prcp)

nc_close(forecast_driver)

# change any fill value to NA
prcp[prcp == fillval$value]= NA

dates_nc = strftime(strptime(dates, format ='%Y-%m-%d'), format = '%Y%m%d%H')
forecast_days = seq(0,forecast_horizon-1, 1)

# 1) find max temp, min temp, and accumulated precipitation at daily time scale. forecast resolution is in 6 to 3 hr valid times so need to aggregate those forecast times to daily
for(t in seq_along(dates_nc)){
  cur_issue_time = which(dates_nc[t] == as.character(issue_time))
  for(n in seq(1,n_en,1)){
    out_df = tibble()
    cur_en = n
    for(f in forecast_days){
      cur_day_hours = f * 24 + 24
      cur_fhours = which(as.numeric(fhours) <= cur_day_hours & as.numeric(fhours) >= cur_day_hours-23)
      if(cur_en < 12){ # if less than number of GEFS ensembles, then use the ensemble number
        cur_prcp = prcp[,,cur_fhours, cur_en, cur_issue_time]
        # take min, max, or sum of values depending on driver (e.g. take min if calculating min temp)
        cur_prcp = apply(cur_prcp, MARGIN = c(1,2), 'sum')
        cur_prcp = cur_prcp * 0.0393701 # convert from mm to in

        cur_prcp = raster(t(cur_prcp), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
                          crs = CRS(crs))
        cur_prcp = flip(cur_prcp, direction = 'y')

        w_mean = exactextractr::exact_extract(cur_prcp, hrus, 'weighted_mean', weights = area(cur_prcp)) # calculating the weighted mean of the driver data (per hru)
        print(sprintf('%s, %s', dates[t], cur_en))

        cur_out_df = tibble(date = dates[t], hru_model_idx = hrus$model_idx, prcp = w_mean)

      }else{ # otherwise use mean and sd of driver spread to get value
        cur_prcp = prcp[,,cur_fhours, , cur_issue_time]
        cur_mean_prcp = apply(cur_prcp, MARGIN = c(1,2,3), 'mean')
        cur_sd_prcp = apply(cur_prcp, MARGIN = c(1,2,3), 'sd')
        n_out = dim(cur_mean_prcp)[1] * dim(cur_mean_prcp)[2] * dim(cur_mean_prcp)[3]
        cur_prcp = array(rnorm(n = n_out, mean = cur_mean_prcp, sd = cur_sd_prcp), dim = dim(cur_mean_prcp))
        # take min, max, or sum of values depending on driver (e.g. take min if calculating min temp)
        cur_prcp = apply(cur_prcp, MARGIN = c(1,2), 'sum')
        cur_prcp = cur_prcp * 0.0393701 # convert from mm to in
        cur_prcp = ifelse(cur_prcp < 0, 0, cur_prcp)

        cur_prcp = raster(t(cur_prcp), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
                          crs = CRS(crs))
        cur_prcp = flip(cur_prcp, direction = 'y')

        w_mean = exactextractr::exact_extract(cur_prcp, hrus, 'weighted_mean', weights = area(cur_prcp)) # calculating the weighted mean of the driver data (per hru)
        print(sprintf('%s, %s', dates[t], cur_en))

        cur_out_df = tibble(date = dates[t], hru_model_idx = hrus$model_idx, prcp = w_mean)

      }
      out_df = bind_rows(out_df, cur_out_df)
    }
    out_df = arrange(out_df, date, as.numeric(hru_model_idx))
    nc_drivers_put(var_df = out_df,
                   var_name = 'prcp',
                   en = n,
                   issue_date = dates[t],
                   nc_name_out = '2_2_model_drivers/out/forecasted_drivers.nc')
  }
}

