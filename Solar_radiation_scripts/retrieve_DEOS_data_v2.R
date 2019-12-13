library(XML)
library(dplyr)
library(lubridate)

# get a sequence of dates
# looks like you can only get solar radiation from hourly data

all_days <- seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by="days")

base_url <- 'http://deos.udel.edu/odd-divas/station_daily.php?network=DEOS&station=DWCH&'
dat_all <- data.frame()

for (i in 1:length(all_days)) {
  date_extension <- sprintf('month=%s&day=%s&year=%s', lubridate::month(all_days[i]), lubridate::day(all_days[i]), lubridate::year(all_days[i]))
  new_url <- paste0(base_url, date_extension)

  # export message so you know what's going on
  message('Retrieving data for ', all_days[i])

  # get that day's data
  dat_list <- XML::readHTMLTable(new_url, stringsAsFactors = FALSE)
  dat <- dat_list[[2]][c(1,3,6,17,19)] %>%
    mutate(date = all_days[i])

  dat_all <- rbind(dat, dat_all)
}

names(dat_all) <- c('Hour', 'Temp_degC', 'Relative_humidity_%', 'Solar_radiation_wattsperm2', 'Rainfall_mm', 'Date')

# write hourly data in case you want to go back and do different summaries
write.csv(dat_all, 'data-raw/DTLY_station_2015_weather_data_hourly.csv', row.names = FALSE)

# reduce to daily values

# delete if previous chunk of code succesfully re-run
hourly_tmp <- read.csv('data-raw/DTLY_station_2015_weather_data.csv', header = TRUE)

# change to dat_all instead of hourly_tmp if previously chunk of code successfully re-run
dat_daily <- hourly_tmp %>%
  group_by(Date) %>%
  summarize(solar_radiation_mean = mean(as.numeric(Solar_radiation_wattsperm2)),
            solar_radiation_max = max(as.numeric(Solar_radiation_wattsperm2)),
            solar_radiation_sum = sum(as.numeric(Solar_radiation_wattsperm2)),
            temp_mean = mean(as.numeric(Temp_degC)),
            rel_humidity_mean = mean(as.numeric(`Relative_humidity_.`)),
            rainfall_sum = sum(as.numeric(Rainfall_mm)))

write.csv(dat_daily, 'data-raw/data_daily/DTLY_station_2015_weather_data_daily.csv', row.names = FALSE)

# plot
library(ggplot2)

# plot all variables on same date scale
dat_daily_long <- dat_daily %>%
  tidyr::gather(key = variable, value = value, -Date)

ggplot(dat_daily_long, aes(y = value, x = Date)) +
  geom_point() +
  facet_wrap(~variable, ncol = 1, scales = 'free_y') +
  theme_bw()

# just plot a single variable

ggplot(dat_daily, aes(y = solar_radiation_sum, x = Date)) +
  geom_point() +
  geom_line()

# gut check of temp vs solar radiation
ggplot(dat_daily, aes(y = temp_mean, x = solar_radiation_sum)) +
  geom_point()




