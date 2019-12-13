# Install tidyverse
#install.packages("tidyverse")

# Load necessary packages
library(tidyverse)
library(ggplot2)
library(gridExtra)

##### Load Files #####
# find .csv files in data_daily subfolder of data-raw
files <- list.files("data-raw/data_daily", "*.csv", full.names = TRUE)
# name list of files
names(files) <- c("DEOS", "PSM", "SNtemp")
# check names
files

##### Plot data #####
# Create data frame of all three .csvs
df <- map_df(files, ~read_csv(.), .id = "Data_source")

# All three data sources plotted on top of one another
df %>% ggplot()+
  aes(x = Date,y = solar_radiation_mean, color = Data_source)+
  geom_point() +
  geom_line() +
  theme_light() +
  labs(title="Daily solar radiation (2015)",
       x="Date", y="Mean solar radiation (W/m2)")


##### Solar radiation plotted as panels, with same y-scale for each panel #####
# Find maximum solar radiation in dataframe
sr_max <- max(df["solar_radiation_mean"])
# Make plot
df %>% ggplot()+
  aes(x = Date,y = solar_radiation_mean, color = Data_source)+
  geom_point() +
  geom_line() +
  facet_wrap(~Data_source, ncol=1, scales = 'free_y') +
  coord_cartesian(ylim = c(0, sr_max)) +
  theme_light()+
  labs(title="Daily solar radiation (2015)",
       x="Date", y="Mean solar radiation (w/m2)")

##### air temp vs solar radiation #####
# Find min and max air temp in dataframe
t_min <- min(df["temp_mean"])
t_max <- max(df["temp_mean"])
# Make plot
df %>% ggplot()+
  aes(x = solar_radiation_mean, y=temp_mean, color=Data_source) +
  geom_point() +
  geom_line() +
  facet_wrap(~Data_source, ncol=1, scales = 'free_y') +
  coord_cartesian(ylim = c(t_min, t_max)) +
  theme_light() +
  labs(title="Mean daily air temperature vs. mean daily solar radiation (2015)",
       x="Mean solar radiation (w/m2)", y="Mean temperature (deg C)")

##### relative humidity vs solar radiation #####
df %>% ggplot()+
  aes(x = solar_radiation_mean, y=rel_humidity_mean, color=Data_source) +
  geom_point() +
  geom_line() +
  facet_wrap(~Data_source, ncol=1, scales = 'free_y') +
  coord_cartesian(ylim = c(0, 100)) +
  theme_light() +
  labs(title="Mean daily relative humidity vs. mean daily solar radiation (2015)",
       x = "Mean solar radiation (w/m2)", y="Mean relative humidity (%)")

##### one-to-one comparison plots #####
df_subset <- df %>%
  select(Date,Data_source,solar_radiation_mean,temp_mean,rel_humidity_mean)
df_long <- df_subset %>%
  tidyr::pivot_wider(names_from = Data_source, values_from = c(solar_radiation_mean, temp_mean, rel_humidity_mean))

# SNtemp vs. DEOS
SNtemp_v_DEOS <- df_long %>% ggplot() +
  aes(x = solar_radiation_mean_DEOS, y=solar_radiation_mean_SNtemp) +
  geom_point() +
  coord_cartesian(xlim = c(0, sr_max), ylim = c(0, sr_max)) +
  geom_smooth(method="lm") +
  geom_abline(slope =1, linetype = 'dashed') +
  theme_light() +
  labs(title="SNtemp estimated solar radiation vs. DEOS measured solar radiation (2015)",
       x="Measured mean solar radiation (w/m2)", y="Estimated mean solar radiation (w/m2)")
# View plot
SNtemp_v_DEOS

# SNtemp vs PSM
SNtemp_v_PSM <- df_long %>% ggplot() +
  aes(x = solar_radiation_mean_PSM, y = solar_radiation_mean_SNtemp) +
  geom_point() +
  coord_cartesian(xlim = c(0, sr_max), ylim = c(0, sr_max)) +
  geom_smooth(method="lm") +
  geom_abline(slope =1, linetype = 'dashed') +
  theme_light() +
  labs(title="SNtemp estimated solar radiation vs. PSM gridded solar radiation (2015)",
       x = "Gridded mean solar radiation (w/m2)", y="Estimated mean solar radiation (w/m2)")
# View plot
SNtemp_v_PSM

# Pair plots
grid.arrange(SNtemp_v_DEOS, SNtemp_v_PSM, nrow=1)

# PSM vs. DEOS
df_long %>% ggplot() +
  aes(x = solar_radiation_mean_DEOS, y=solar_radiation_mean_PSM) +
  geom_point() +
  coord_cartesian(xlim = c(0, sr_max), ylim = c(0,sr_max)) +
  geom_smooth(method="lm") +
  geom_abline(slope =1, linetype = 'dashed') +
  theme_light() +
  labs(title="PSM gridded solar radiation vs. DEOS measured solar radiation (2015)",
       x="Measured mean solar radiation (w/m2)", y="Gridded mean solar radiation (w/m2)")


