
# Load necessary packages
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

##### All station plot function #####
plot_all_stations <- function(files, site_list, sr_year){
  ### Create data frame of files
  solar_data <- map_df(files, ~read_csv(.), .id = NULL)
  # select data for year of interest
  df <- solar_data %>%
    filter(Date %like% sr_year)

  ### For all stations, plot mean solar radiation (in one plot)
  plot_A_title <- paste0("Daily solar radiation (", sr_year, ") - All stations")
  plot_A <- df %>% ggplot()+
    aes(x = Date,y = solar_radiation_mean, color = Data_source)+
    geom_point(size = 0.5) +
    theme_light() +
    labs(title=plot_A_title,
         x="Date", y="Mean solar radiation (W/m2)")
  plot_A_name <- paste0("data-raw/SR_plots/SR_AllStations_", sr_year,".jpg")
  ggsave(filename=plot_A_name, plot=plot_A, dpi=400, width=8, height=5)

  ### For all stations, plot mean solar radiation, by data source
  plot_B_title <- paste0("Daily solar radiation (", sr_year, ") - All stations")
  # find maximum solar radiation for year
  sr_max <- max(df["solar_radiation_mean"], na.rm = TRUE)
    # plot data
  plot_B <- df %>% ggplot()+
    aes(x = Date,y = solar_radiation_mean, color = Data_source)+
    geom_point(size = 0.5) +
    facet_wrap(~Data_source, ncol=1, scales = 'free_y') +
    coord_cartesian(ylim = c(0, sr_max)) +
    theme_light()+
    labs(title=plot_A_title,
         x="Date", y="Mean solar radiation (w/m2)")
  plot_B_name <- paste0("data-raw/SR_plots/SR_AllStations_DataSource_", sr_year,".jpg")
  ggsave(filename=plot_B_name, plot=plot_B, dpi=400, width=8, height=5)

  ### For all stations, plot air temperature vs. mean solar radiation, by data source
  plot_C_title <- paste0("Mean daily air temperature vs. mean daily solar radiation (", sr_year, ") - All stations")
  # Find min and max air temp in dataframe
  t_min <- min(df["temp_mean"], na.rm = TRUE)
  t_max <- max(df["temp_mean"], na.rm = TRUE)
  # Make plot
  plot_C <- df %>% ggplot()+
    aes(x = solar_radiation_mean, y=temp_mean, color=Data_source) +
    geom_point(size = 0.5) +
    facet_wrap(~Data_source, ncol=1, scales = 'free_y') +
    coord_cartesian(ylim = c(t_min, t_max), xlim = c(0, sr_max)) +
    theme_light() +
    labs(title= plot_B_title,
         x="Mean solar radiation (w/m2)", y="Mean temperature (deg C)")
  plot_C_name <- paste0("data-raw/SR_plots/TA_SR_AllStations_DataSource_", sr_year,".jpg")
  ggsave(filename=plot_C_name, plot=plot_C, dpi=400, width=8, height=5)

  ### For all stations, plot relative humidity vs. mean solar radiation, by data source
  plot_D_title <- paste0("Mean daily relative humidity vs. mean daily solar radiation (", sr_year, ") - All stations")
  plot_D <- df %>% ggplot()+
    aes(x = solar_radiation_mean, y=rel_humidity_mean, color=Data_source) +
    geom_point(size = 0.5) +
    facet_wrap(~Data_source, ncol=1, scales = 'free_y') +
    coord_cartesian(ylim = c(0, 100), xlim = c(0, sr_max)) +
    theme_light() +
    labs(title = plot_D_title,
         x = "Mean solar radiation (w/m2)", y="Mean relative humidity (%)")
  plot_D_name <- paste0("data-raw/SR_plots/RH_SR_AllStations_DataSource_", sr_year,".jpg")
  ggsave(filename=plot_D_name, plot=plot_D, device = NULL, dpi=400, width=8, height=5)
}

##### Site-specific plot function #####
plot_site_data <- function(files, site_list, sr_year) {
  ### Create data frame of files
  solar_data <- map_df(files, ~read_csv(.), .id = NULL)
  # select data for year of interest
  df <- solar_data %>%
    filter(Date %like% sr_year)

  # Creating bounding parameters for plots
  sr_max <- max(df["solar_radiation_mean"], na.rm = TRUE)
  t_min <- min(df["temp_mean"], na.rm = TRUE)
  t_max <- max(df["temp_mean"], na.rm = TRUE)

  # for each station, color coded by source...
  for (row in 1:nrow(site_list)){
    # Define site name
    site_name <- site_list[row, "Station"]

    # filter data
    df_site <- filter(df, Station == site_name)

    # Plot solar radation data
    site_plot_A_title <- paste0("Daily solar radiation (", sr_year, ") - ", site_name)
    site_plot_A <- df_site %>% ggplot()+
      aes(x = Date,y = solar_radiation_mean, color = Data_source)+
      geom_point(size = 0.5) +
      geom_line() +
      theme_light() +
      labs(title=site_plot_A_title,
           x="Date", y="Mean solar radiation (W/m2)")
    site_plot_A_name <- paste0("data-raw/SR_plots/Site_specific/SR_", site_name, "_", sr_year,".jpg")
    ggsave(filename=site_plot_A_name, plot=site_plot_A, device = NULL, dpi=400, width=8, height=5)

    # Plot solar radiation data, by data source
    site_plot_B_title <- paste0("Daily solar radiation (", sr_year, ") - ", site_name)
    site_plot_B <- df_site %>% ggplot()+
      aes(x = Date,y = solar_radiation_mean, color = Data_source)+
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~Data_source, ncol=1, scales = 'free_y') +
      coord_cartesian(ylim = c(0, sr_max)) +
      theme_light()+
      labs(title=site_plot_B_title,
           x="Date", y="Mean solar radiation (w/m2)")
    site_plot_B_name <- paste0("data-raw/SR_plots/Site_specific/SR_", site_name, "_DataSource_", sr_year,".jpg")
    ggsave(filename=site_plot_B_name, plot=site_plot_B, device = NULL, dpi=400, width=8, height=5)

    # Plot air temperature vs solar radation (using max T in all data as y limit)
    site_plot_C_title <- paste0("Mean daily air temperature vs. mean daily solar radiation (", sr_year, ") - ", site_name)
    # Make plot
    site_plot_C <- df_site %>% ggplot()+
      aes(x = solar_radiation_mean, y=temp_mean, color=Data_source) +
      geom_point(size = 0.5) +
      facet_wrap(~Data_source, ncol=1, scales = 'free_y') +
      coord_cartesian(ylim = c(t_min, t_max), xlim = c(0, sr_max)) +
      theme_light() +
      labs(title=site_plot_C_title,
           x="Mean solar radiation (w/m2)", y="Mean temperature (deg C)")
    site_plot_C_name <- paste0("data-raw/SR_plots/Site_specific/AT_SR_", site_name, "_DataSource_", sr_year,".jpg")
    ggsave(filename=site_plot_C_name, plot=site_plot_C, device = NULL, dpi=400, width=8, height=5)

    # Plot relative humidity vs solar radiation
    site_plot_D_title <- paste0("Mean daily relative humidity vs. mean daily solar radiation (", sr_year, ") - ", site_name)
    site_plot_D <- df_site %>% ggplot()+
      aes(x = solar_radiation_mean, y=rel_humidity_mean, color=Data_source) +
      geom_point(size = 0.5) +
      facet_wrap(~Data_source, ncol=1, scales = 'free_y') +
      coord_cartesian(ylim = c(0, 100), xlim = c(0, sr_max)) +
      theme_light() +
      labs(title=site_plot_D_title,
           x = "Mean solar radiation (w/m2)", y="Mean relative humidity (%)")
    site_plot_D_name <- paste0("data-raw/SR_plots/Site_specific/RH_SR_", site_name, "_DataSource_", sr_year,".jpg")
    ggsave(filename=site_plot_D_name, plot=site_plot_D, device = NULL, dpi=400, width=8, height=5)
  }
}


##### One-to-one plot function #####
plot_comparisons <- function(files, site_list, sr_year) {
  ### Create data frame of files
  solar_data <- map_df(files, ~read_csv(.), .id = NULL)
  # select data for year of interest
  df <- solar_data %>%
    filter(Date %like% sr_year)

  # Creating bounding parameters for plots
  sr_max <- max(df["solar_radiation_mean"], na.rm = TRUE)
  t_min <- min(df["temp_mean"], na.rm = TRUE)
  t_max <- max(df["temp_mean"], na.rm = TRUE)

  # For each station, color coded by source...
  for (row in 1:nrow(site_list)){
    # Define site name
    site_name <- site_list[row, "Station"]

    # filter data
    df_site <- filter(df, Station == site_name)

    # Reformat data to wide format
    df_site_subset <- df_site %>%
      select(Date,Data_source,solar_radiation_mean,temp_mean,rel_humidity_mean)
    df_site_wide <- df_site_subset %>%
      tidyr::pivot_wider(names_from = Data_source, values_from = c(solar_radiation_mean, temp_mean, rel_humidity_mean))

    # Plot SNTemp solar radiation vs. DEOS solar radiation
    SNTemp_v_DEOS_title <- paste0("SNTemp estimated solar radiation vs. DEOS measured solar radiation (", sr_year, ") - ", site_name)
    SNTemp_v_DEOS <- df_site_wide %>% ggplot() +
      aes(x = solar_radiation_mean_DEOS, y=solar_radiation_mean_SNTemp) +
      geom_point(size = 0.5) +
      coord_cartesian(xlim = c(0, sr_max), ylim = c(0, sr_max)) +
      geom_smooth(method="lm") +
      geom_abline(slope =1, linetype = 'dashed') +
      theme_light() +
      labs(title=SNTemp_v_DEOS_title,
           x="DEOS measured mean solar radiation (w/m2)", y="SNTemp estimated mean solar radiation (w/m2)")
    SNTemp_v_DEOS_name <- paste0("data-raw/SR_plots/Scatter_plots/SNTemp_v_DEOS_", site_name, "_", sr_year,".jpg")
    ggsave(filename= SNTemp_v_DEOS_name, plot=SNTemp_v_DEOS, device = NULL, dpi=400, width=8, height=5)

    # Plot SNTemp solar radiation vs. PSM solar radiation
    SNTemp_v_PSM_title <- paste0("SNTemp estimated solar radiation vs. PSM gridded solar radiation (", sr_year, ") - ", site_name)
    SNTemp_v_PSM <- df_site_wide %>% ggplot() +
      aes(x = solar_radiation_mean_PSM, y = solar_radiation_mean_SNTemp) +
      geom_point(size = 0.5) +
      coord_cartesian(xlim = c(0, sr_max), ylim = c(0, sr_max)) +
      geom_smooth(method="lm") +
      geom_abline(slope =1, linetype = 'dashed') +
      theme_light() +
      labs(title=SNTemp_v_PSM_title,
           x = "PSM gridded mean solar radiation (w/m2)", y="SNTemp estimated mean solar radiation (w/m2)")
    SNTemp_v_PSM_name <- paste0("data-raw/SR_plots/Scatter_plots/SNTemp_v_PSM_", site_name, "_", sr_year,".jpg")
    ggsave(filename= SNTemp_v_PSM_name, plot=SNTemp_v_PSM, device = NULL, dpi=400, width=8, height=5)


    ### pair plots
    SNTemp_v_PSM_DEOS <- grid.arrange(SNTemp_v_DEOS, SNTemp_v_PSM, nrow=1)
    SNTemp_v_PSM_DEOS_name <- paste0("data-raw/SR_plots/Scatter_plots/SNTemp_v_PSM_DEOS_", site_name, "_", sr_year,".jpg")
    ggsave(filename= SNTemp_v_PSM_DEOS_name, plot=SNTemp_v_PSM_DEOS, device = NULL, dpi=400, width=16, height=5)


    # Plot PSM vs. DEOS
    PSM_v_DEOS_title <- paste0("PSM gridded solar radiation vs. DEOS measured solar radiation (", sr_year, ") - ", site_name)
    PSM_v_DEOS <- df_site_wide %>% ggplot() +
      aes(x = solar_radiation_mean_DEOS, y=solar_radiation_mean_PSM) +
      geom_point(size = 0.5) +
      coord_cartesian(xlim = c(0, sr_max), ylim = c(0, sr_max)) +
      geom_smooth(method="lm") +
      geom_abline(slope =1, linetype = 'dashed') +
      theme_light() +
      labs(title=PSM_v_DEOS_title,
           x="DEOS measured mean solar radiation (w/m2)", y="PSM gridded mean solar radiation (w/m2)")
    PSM_v_DEOS_name <- paste0("data-raw/SR_plots/Scatter_plots/PSM_v_DEOS_", site_name, "_", sr_year,".jpg")
    ggsave(filename= PSM_v_DEOS_name, plot=PSM_v_DEOS, device = NULL, dpi=400, width=8, height=5)
  }
}



##### Get parameters for functions #####
sr_files <- list.files("data-raw/Formatted_daily_data", "*.csv", full.names = TRUE)
site_list <- read.csv('data-raw/DEOS/DEOS_DRB.csv', header = TRUE)
sr_year <- 2015

##### Call functions #####
plot_all_stations(files = sr_files, site_list = site_list, sr_year = sr_year)
plot_site_data(files = sr_files, site_list = site_list, sr_year = sr_year)
plot_comparisons(files = sr_files, site_list = site_list, sr_year = sr_year)
