# library(scipiper)
# install.packages('googledrive')
# library(googledrive)
# scmake('data_for_Xiaowei/sntemp_input_output.feather')
# scmake('6_model_output/out/sw_rad_station_location.rds.ind')

# load necessary libraries
library("dplyr")
library("data.table")

# Set working directory
dir_sntemp <- "C:/Users/Hayley/Documents/USGS/R/delaware-water-temp"
setwd(dir_sntemp)

# read in model output
sntemp_swrad = readRDS('data-raw/SNTemp_sw_rad_station_location_39.933_-75.55.rds')

# create temporary dataset
sntemp_tmp <- sntemp_swrad
# Add new column named "Date" that is duplicate of "date" column
# Was having trouble renaming "date" "Date" below
# And convert humidity in SN_temp to percentage (currently fixed at 0.7)
sntemp_tmp <- sntemp_tmp %>%
  mutate(., Date = date) %>%
  mutate(., rel_humidity_mean = seg_humid*100)

# Create new dataset that is subset of full dataset, with data for 2015 only
sntemp_tmp_rad <- sntemp_tmp %>%
  select(Date, rel_humidity_mean, seg_tave_air, seginc_swrad) %>%
  rename(temp_mean = seg_tave_air, solar_radiation_mean = seginc_swrad) %>%
  filter(Date %like% "2015")

# Write new dataset to a .csv
write.csv(sntemp_tmp_rad, 'data-raw/data_daily/SNTemp_sw_rad_39.933_-75.55_2015.csv', row.names = FALSE)




