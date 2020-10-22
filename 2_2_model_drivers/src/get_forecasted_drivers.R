
# trying out noaaGEFSpoint https://github.com/rqthomas/noaaGEFSpoint

library(noaaGEFSpoint)
library(tidyverse)

site_file <- system.file("extdata", "noaa_download_site_list.csv", package = "noaaGEFSpoint")
neon_sites <- read_csv(site_file) %>%
  slice(1:3)

output_dir <- '2_2_model_drivers/gefs'

noaaGEFSpoint::noaa_gefs_download_downscale(site_list = neon_sites$site_id,
                                            lat_list = neon_sites$latitude,
                                            lon_list = neon_sites$longitude,
                                            output_directory = output_dir,
                                            forecast_time = "all",
                                            forecast_date = "all",
                                            downscale = TRUE,
                                            run_parallel = FALSE,
                                            num_cores = 1,
                                            overwrite = FALSE)

library(ncdf4)
nc_file = '2_2_model_drivers/gefs/NOAAGEFS_6hr/LIRO/20201016/00/NOAAGEFS_6hr_LIRO_2020-10-16T00_2020-11-01T00_ens00.nc'
forecast_driver <- nc_open(filename = nc_file)

dates <- ncvar_get(nc = forecast_driver, varid = 'time') # hours since forecast issue time
temp <- ncvar_get(nc = forecast_driver, varid = 'air_temperature') # air temperature in Kelvin
precip <- ncvar_get(nc = forecast_driver, varid = )


forecast_issue_time <- ncatt_get(nc = forecast_driver, varid = 0, attname = '')


nc_close(forecast_driver)




## NOAA GEFS archive
library(ncdf4)
library(tidyverse)
library(ggplot2)
nc_file = '../../jzwart/Downloads/NOAA GEFS/tmax_2m_latlon_all_20200101_20200831_jzwalEY3fw.nc'
forecast_driver <- nc_open(filename = nc_file)

temp <- ncvar_get(nc = forecast_driver, varid = 'Maximum_temperature', ) # air Temp in Kelvin; should be five dimensions [lon, lat, forecast hours, ensemble, issue date]
issue_time <- ncvar_get(nc = forecast_driver, varid = 'intTime') # forecast issue time in YYYYMMDDHH; should be 244 days (Jan 1 to Aug 31, 2020) ; one dimension
valid_time <- ncvar_get(nc = forecast_driver, varid = 'intValidTime') # forecast valid time in YYYYMMDDHH; should be two dimensions, forecast hours x issue date


# temp = forecast_driver$var[[3]]
# varsize = temp$varsize
# ndims = temp$ndims
# nt = varsize[ndims]
# # example for getting one time out
# for(i in 1:nt){
#   start = rep(1, ndims)
#   start[ndims] = i
#   count = varsize
#   count[ndims] = 1
#   cur = ncvar_get(forecast_driver, temp, start = start, count = count)
#   timeval = ncvar_get(forecast_driver, temp$dim[[ndims]]$name, start = i, count = 1)
# }

# example for getting multiple dimensions out
lon_dim = 1
lat_dim = 2
fhours_dim = 3
en_dim = 4
time_dim = 5

k_to_c = -273.15 # conversion from Kelvin to Celsius

temp = forecast_driver$var[['Maximum_temperature']]
varsize = temp$varsize
n_dims = temp$ndims
n_time = 150 # varsize[time_dim]
n_lon = 1 #varsize[lon_dim]
n_lat = 1 # varsize[lat_dim]
n_fhours = varsize[fhours_dim]
n_en = varsize[en_dim]
# example for getting one time out
out=tibble()
for(lon in 1:n_lon){
  for(lat in 1:n_lat){
    # for(fhours in 1:n_fhours){
      # for(en in 1:n_en){
        for(t in 150:n_time){
          start = rep(1, n_dims)
          start[time_dim] = t
          start[lon_dim] = lon
          start[lat_dim] = lat
          start[fhours_dim] = 1 #fhours
          start[en_dim] = 1 #en

          count = varsize
          count[time_dim] = 1 # one time step
          count[lon_dim] = 1
          count[lat_dim] = 1
          cur = ncvar_get(forecast_driver, temp, start = start, count = count) + k_to_c # [forecast hours x ensemble] matrix
          time_val = ncvar_get(forecast_driver, temp$dim[[time_dim]]$name, start = t, count = 1) # hours since 1800-01-01 00:00:00
          issue_date = as.POSIXct('1800-01-01 00:00:00', tz = 'GMT') + as.difftime(time_val, units = 'hours', tz = 'GMT')
          lon_val = ncvar_get(forecast_driver, temp$dim[[lon_dim]]$name, start = lon, count = 1) # degrees east
          lat_val = ncvar_get(forecast_driver, temp$dim[[lat_dim]]$name, start = lat, count = 1) # degrees north
          fhours_val = ncvar_get(forecast_driver, temp$dim[[fhours_dim]]$name) # degrees north

          mu = apply(cur, 1, mean)
          s = apply(cur, 1, sd)
          datetime = issue_date + as.difftime(fhours_val, units = 'hours', tz = 'GMT')
          colnames(cur) = paste('en', 1:11, sep ='_')
          cur_out = as_tibble(cur) %>%
            mutate(valid_time = datetime) %>%
            pivot_longer(cols = -valid_time, names_to = 'ensemble', values_to = 'max_temp_c') %>%
            mutate(issue_time = issue_date) %>%
            group_by(valid_time) %>%
            mutate(mean_max_temp = mean(max_temp_c)) %>%
            ungroup()

          out = rbind(out, cur_out)

          # windows()
          # plot(mu ~ datetime, type = 'l', lwd = 2, ylim = range(c(mu-s,mu+s)))
          # arrows(datetime, mu-s, datetime, mu+s, length=0.05, angle=90, code=3)
        }
      # }
    # }
  }
}

ggplot(out, aes(x = valid_time, y = mean_max_temp, group = issue_time, color = issue_time))+
  geom_line(size =3) +
  geom_line(data = out, aes(x = valid_time, y = max_temp_c, group = interaction(ensemble, issue_time), color = issue_time))+
  ylab('Maximum Air Temperature (C)') + xlab('') +
  theme_minimal()



# quick plot for presentation
windows()
ggplot(out, aes(x = valid_time, y = max_temp_c, group = interaction(ensemble, issue_time)))+
  geom_line(size =1.5, color = 'grey') +
  ylab('Maximum Air Temperature (C)') + xlab('') +
  theme_classic()


# some questions: Are time values local time? are the valid times for the daily maximum temperature or for the maximum temperature at that time?


forecast_issue_time <- ncatt_get(nc = forecast_driver, varid = 0, attname = '')


nc_close(forecast_driver)









####

library(rNOMADS)

get_forecasted_drivers = function(){


}


abbrev <- "gfs4"
model.date <- paste0(format(Sys.time(), "%Y"), "0101")
## Not run:
gfs.available.models <- CheckNOMADSArchive(abbrev, model.date)


CheckNOMADSArchive(abbrev = 'gfs4')

NOMADSArchiveList('grib')


library(RCurl)

url = 'ftp://ftp.cdc.noaa.gov/Projects/Reforecast2/2020/202005/2020050100/p01/latlon/'

files = getURL(url = url, ftp.use.epsv = F, dirlistonly = T)
files = strsplit(files, "\r\n")
files = unlist(files)
files = files[grep('tmin', files)]
for (file in files) {
  download.file(paste(url, file, sep = ""),
                destfile = file.path('2_2_model_drivers/out', file))
}

# details for getting wgrib2 on computer: https://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/compile_questions.html

ReadGrib(file.names = '2_2_model_drivers/out/tmin_2m_2020050100_p01.grib2')



