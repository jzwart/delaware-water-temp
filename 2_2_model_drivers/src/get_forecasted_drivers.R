
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



