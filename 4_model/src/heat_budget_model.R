# simple heat budget ; 2020-11-23

#'temp yesterday
#'
#'
calc_streamtemp_regression = function(tw_tminus1,
                                      delta_air_temp,
                                      air_temp_int,
                                      air_temp_slope,
                                      model_idx){

  # simple regression model
  tw = tw_tminus1 + air_temp_slope * delta_air_temp + air_temp_int

  out = tibble(model_idx = model_idx, seg_tave_water = tw)
  return(out)
}


# Simple heat budget based on Sanders et al 2017 and Herbert et al. 2011 and Cole et al. 2014

calc_streamtemp_heat_budget = function(tw_tminus1,
                                       cur_prms_data,
                                       network_map,
                                       model_idx,
                                       cur_step,
                                       dates){
  rho = 1000 # density of water in kg / m^3
  C_p = 4200 # specific heat of water in J / kg/ C
  sigma = 5.670373E-8 # Stefan-Boltzmann constant in J/(sec * m^2 * K^4)
  emiss_water = 0.9526 # emissivity of water from Sanders et al. 2017
  emiss_veg = 0.9526 # emissivity of riparian vegetation from Sanders et al. 2017
  r_l = 0 # longwave relfection fraction ; from Sanders et al. 2017
  albedo = 0.1 # albedo or fraction reflected by stream; dimensionless
  K_g = 1.65 # thermal conductivity of the streambed; W/m^2/C ; from Sanders et al. 2017
  delta_Z = 1 # equilibrium depth from the water-streambed interface at which temperature is groundwater temperature ; m ; from Sanders et al. 2017

  out = tibble(model_idx = sort(as.numeric(model_idx)),
               tw_tminus1 = tw_tminus1,
               seg_tave_water = NA,
               H_n = NA,
               H_a = NA,
               H_v = NA,
               H_w = NA,
               H_s = NA,
               H_e = NA,
               H_c = NA,
               H_d = NA,
               H_f = NA)

  network_map = network_map[!is.na(network_map$to_seg_id_nat), ] # only care about upstream idxs; getting rid of NA's in to
  prms_t = dplyr::filter(cur_prms_data, date == dates[cur_step])
  prms_tminus1 = dplyr::filter(cur_prms_data, date == dates[cur_step-1])

  for(i in seq_along(model_idx)){
    cur_seg = model_idx[i]
    cur_upstream_segs = network_map$from_seg_id_nat[network_map$to_seg_id_nat == cur_seg]

    # calculating heat fluxes (from Sanders et al. 2017)
    elev = prms_t$seg_elev[prms_t$model_idx == cur_seg] # segment elevation; m
    slope = prms_t$seg_slope[prms_t$model_idx == cur_seg] # segment slope ; dimensionless
    width = prms_t$seg_width[prms_t$model_idx == cur_seg] # segment width; m
    C_l = prms_t$seg_ccov[prms_t$model_idx == cur_seg] # cloud cover fraction; dimensionless
    Sh = prms_t$seg_shade[prms_t$model_idx == cur_seg] # shade fraction; dimensionless
    air_T = prms_t$seg_tave_air[prms_t$model_idx == cur_seg] # air temperature C
    sw_rad = prms_t$seginc_swrad[prms_t$model_idx == cur_seg] # solar radiation ; W/m^2
    potet = prms_t$seginc_potet[prms_t$model_idx == cur_seg] # potential evaporation or free-water surface evaporation; meters
    rh = prms_t$seg_humid[prms_t$model_idx == cur_seg] # relative humidity; decimal fraction
    gw_T = 9 # prms_t$seg_tave_gw[prms_t$model_idx == cur_seg] # groundwater temperature; C
    sat_vapor_press = 6.11 * exp((17.27 * air_T) / (273.16 + air_T)) # saturated vapor pressure; mb
    vapor_press = rh * sat_vapor_press # vapor pressure at current air temp and humidity; mb ; https://www.weather.gov/media/epz/wxcalc/vaporPressure.pdf
    emiss_air = 0.61 + 0.05 * sqrt(vapor_press) # emissivity of air

    H_a = (1 - r_l) * (1 - Sh) * (1 + 0.17 * C_l^2) * emiss_air * sigma * (air_T + 273.16)^4 # atmospheric longwave radiation; W/m^2

    H_v = emiss_veg * sigma * Sh * (air_T + 273.16)^4 # Riparian vegetation longwave radiation; W/m^2

    H_w = emiss_water * sigma * (out$tw_tminus1[out$model_idx == cur_seg] + 273.16)^4 # water-emitted longwave radiation; W/m^2  #### this might be a negative number? or subtracted?? currently added in Sanders et al. 2017 ####

    H_s = (1 - albedo) * (1 - Sh) * sw_rad # Shortwave solar radiation; W/m^2

    gamma = 2495 * rho - 2360 * out$tw_tminus1[out$model_idx == cur_seg]
    H_e = gamma * (potet / 86400) * rho # Evaporation heat flux; W/m^2 ; converting potet to m / sec
    H_e = (6 + 3*4)*(sat_vapor_press/1.33322 - vapor_press/1.33322) # from Herbert et al. 2011 - seems to give more reasonable evaporative heat flux but this should be looked at again

    atm_press = press_from_elev(temp = air_T, elev = elev) # atmospheric pressure based on elevation and temp; mb
    B_c = (0.00061 * atm_press) / (sat_vapor_press - vapor_press) # Bowen coefficient; dimensionless
    B_o = B_c * (out$tw_tminus1[out$model_idx == cur_seg] - air_T) # Bowen Ratio
    H_c = B_o * H_e  # conductive and convective heat flux; W/m^2

    H_d = (K_g * (gw_T - out$tw_tminus1[out$model_idx == cur_seg])) / delta_Z # streambed conduction ; W/m^2

    H_f = 9805 * (prms_tminus1$seg_outflow[prms_tminus1$model_idx == cur_seg] * slope) / width # stream friction ; W/m^2

    H_n = H_a + H_v - H_w + H_s - H_e - H_c + H_d + H_f # net heat flux (non-advective) ; W/m^2
    H_n = H_a + H_v + H_s - H_e - H_c + H_f # trying to get some more stability
    H_n = 0 # debugging

    # calculate advective heat flux
    if(length(cur_upstream_segs) > 0){ # if there are upstream segments, calculate advective heat flux from those segments
      upstream_heat = 0
      upstream_flow = 0
      for(j in seq_along(cur_upstream_segs)){
        cur_up_seg = cur_upstream_segs[j]
        upstream_heat = upstream_heat + out$tw_tminus1[out$model_idx == cur_up_seg] * prms_tminus1$seg_outflow[prms_tminus1$model_idx == cur_up_seg]
        upstream_flow = upstream_flow +prms_tminus1$seg_outflow[prms_tminus1$model_idx == cur_up_seg]
      }
      # upstream_temp = upstream_heat / upstream_flow # average upstream temp
      heat_outflow = out$tw_tminus1[out$model_idx == cur_seg] * prms_tminus1$seg_outflow[prms_tminus1$model_idx == cur_seg]
      upstream_temp = upstream_heat / upstream_flow

      net_advect_heat = upstream_heat - heat_outflow

      # from Sanders et al 2017 ; equation 27
      lat_flow = upstream_flow / prms_tminus1$seg_length[prms_tminus1$model_idx == cur_seg] # m2 / sec
      cur_outflow = prms_tminus1$seg_outflow[prms_tminus1$model_idx == cur_seg] # cms
      cur_temp = out$tw_tminus1[out$model_idx == cur_seg] # C
      cur_width = prms_tminus1$seg_width[prms_tminus1$model_idx == cur_seg] # m

      dT_dlength = ((lat_flow / cur_outflow)*(upstream_temp - cur_temp)) +
        ((cur_width * H_n) / (cur_outflow * rho * C_p))
      dT = dT_dlength * prms_tminus1$seg_length[prms_tminus1$model_idx == cur_seg]

      out$seg_tave_water[out$model_idx == cur_seg] = out$tw_tminus1[out$model_idx == cur_seg] + dT

    }else{ # headwater segment
      # need to update for headwaters
      # out$seg_tave_water[out$model_idx == cur_seg] = out$tw_tminus1[out$model_idx == cur_seg] + rnorm(1, 0, .2)
      dT_dlength = ((cur_width * H_n) / (cur_outflow * rho * C_p))
      dT = dT_dlength * prms_tminus1$seg_length[prms_tminus1$model_idx == cur_seg]
      # debugging; headwaters responding to air temp
      dT = (air_T - out$tw_tminus1[out$model_idx == cur_seg]) / 3

      out$seg_tave_water[out$model_idx == cur_seg] = out$tw_tminus1[out$model_idx == cur_seg] + dT
    }
    out$H_n[out$model_idx == cur_seg] = H_n
    out$H_a[out$model_idx == cur_seg] = H_a
    out$H_v[out$model_idx == cur_seg] = H_v
    out$H_w[out$model_idx == cur_seg] = H_w
    out$H_s[out$model_idx == cur_seg] = H_s
    out$H_e[out$model_idx == cur_seg] = H_e
    out$H_c[out$model_idx == cur_seg] = H_c
    out$H_d[out$model_idx == cur_seg] = H_d
    out$H_f[out$model_idx == cur_seg] = H_f
  }
  # check for bad values; ideally we wouldn't have to look for these upper bounds
  out$seg_tave_water = ifelse(out$seg_tave_water < 0, 0, out$seg_tave_water)
  out$seg_tave_water = ifelse(out$seg_tave_water > 36, 36, out$seg_tave_water)

  return(out)
}








# function for returning PRMS values for heat budget

data_for_heat_budget = function(model_run_loc,
                                model_output_file,
                                model_fabric_file,
                                sntemp_vars){

  stream_temp_intermediates = get_sntemp_intermediates(model_output_file = file.path(model_run_loc,
                                                                                     model_output_file),
                                                       model_fabric_file = file.path(model_run_loc,
                                                                                     model_fabric_file),
                                                       sntemp_vars = sntemp_vars[[1]])

  inches_to_m = 0.0254
  cfs_to_m3sec = 1/3.28084^3
  langleys_day_to_w_m2 = 11.63/24 # see https://www.nrcs.usda.gov/wps/portal/nrcs/detailfull/null/?cid=stelprdb1043619 for conversion
  stream_temp_intermediates = stream_temp_intermediates %>%
    mutate(parameter_value = case_when(parameter == 'seg_rain' ~ parameter_value * inches_to_m,
                                       parameter == 'seg_outflow' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seginc_gwflow' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seginc_sroff' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seginc_ssflow' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seg_upstream_inflow' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seg_potet' ~ parameter_value * inches_to_m,
                                       parameter == 'seginc_swrad' ~ parameter_value * langleys_day_to_w_m2,
                                       TRUE ~ parameter_value))


  stream_temp_intermediates_wide = stream_temp_intermediates %>%
    spread(key = 'parameter', value = 'parameter_value')

  # get static variables
  hru_mapping = read.table(file.path(model_run_loc, 'input/myparam.param'), skip = 4, stringsAsFactors = F)

  seg_length = hru_mapping[(grep('seg_length',hru_mapping[,1])+5):(grep('seg_length',hru_mapping[,1])+460),] # seg_length, units are in m
  seg_length = tibble(model_idx = as.character(seq(1,456)), seg_length = as.numeric(seg_length)) # in meters

  seg_slope = hru_mapping[(grep('seg_slope',hru_mapping[,1])+5):(grep('seg_slope',hru_mapping[,1])+460),] # seg_slope, units dimensionless
  seg_slope = tibble(model_idx = as.character(seq(1,456)), seg_slope = as.numeric(seg_slope)) # dimensionless

  seg_elev = hru_mapping[(grep('seg_elev',hru_mapping[,1])+5):(grep('seg_elev',hru_mapping[,1])+460),] # seg_elev, units are in m
  seg_elev = tibble(model_idx = as.character(seq(1,456)), seg_elev = as.numeric(seg_elev)) # in meters


  stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_length, by = 'model_idx')
  stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_slope, by = 'model_idx')
  stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_elev, by = 'model_idx')

  return(stream_temp_intermediates_wide)
}

# from LakeMetabolizer https://github.com/GLEON/LakeMetabolizer/blob/master/R/o2.at.sat.R
press_from_elev = function(temp, elev){

  # Conversion from mL/L (the usual output of the garcia, weiss, etc. equations)
  # to mg/L per USGS memo 2011.03
  mgL.mlL <- 1.42905

  # Correction for air pressure; incorportes effects of altitude & vapor pressure of water
  mmHg.mb <- 0.750061683 # conversion from mm Hg to millibars

  mmHg.inHg <- 25.3970886 # conversion from inches Hg to mm Hg
  standard.pressure.sea.level <- 29.92126 # Pb, inches Hg
  standard.temperature.sea.level <- 15 + 273.15 # Tb, 15 C = 288.15 K
  gravitational.acceleration <- 9.80665 # g0, m/s^2
  air.molar.mass <- 0.0289644 # M, molar mass of Earth's air (kg/mol)
  universal.gas.constant <- 8.31447 #8.31432 # R*, N*m/(mol*K)

  # estimate pressure by the barometric formula
  baro <- (1/mmHg.mb) * mmHg.inHg * standard.pressure.sea.level *
    exp( (-gravitational.acceleration * air.molar.mass * elev) / (universal.gas.constant * standard.temperature.sea.level) )

  # pressure correction per USGS memos 81.11 and 81.15. calculate u by Antoine equation.
  u <- 10 ^ (8.10765 - 1750.286 / (235 + temp)) # u is vapor pressure of water; water temp is used as an approximation for water & air temp at the air-water boundary
  press.corr <- (baro*mmHg.mb - u) / (760 - u) # pressure correction is ratio of current to standard pressure after correcting for vapor pressure of water. 0.750061683 mmHg/mb
  press.out <- baro * press.corr

  return(press.out)
}

# library(dplyr)
# d = feather::read_feather('4_model_for_PGDL/out/sntemp_input_output_subset.feather')
#
# air_temp = d[,c('seg_id_nat','model_idx','date', 'seg_tave_air')]
# air_temp
#
# obs = readRDS('3_observations/out/obs_temp_flow.rds')$temp %>%
#   arrange(seg_id_nat, date)
#
# all = left_join(air_temp, obs, by = c('seg_id_nat','date'))
# sequential_dates = na.omit(all) %>%
#   arrange(model_idx, date)
# sequential_dates$seg_tave_air_1 = c(NA, sequential_dates$seg_tave_air[1:(nrow(sequential_dates)-1)])
# sequential_dates$temp_C_1 = c(NA, sequential_dates$temp_C[1:(nrow(sequential_dates)-1)])
# sequential_dates = sequential_dates %>%
#   mutate(delta_stream_T = temp_C - temp_C_1,
#          delta_air_T = seg_tave_air - seg_tave_air_1)
#
# summary(lm(all$temp_C ~ all$seg_tave_air))
# summary(lm(sequential_dates$delta_stream_T ~ sequential_dates$delta_air_T))
#
#
#
