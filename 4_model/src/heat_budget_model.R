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
    gw_T = prms_t$seg_tave_gw[prms_t$model_idx == cur_seg] # groundwater temperature; C
    sat_vapor_press = 6.11 * exp((17.27 * air_T) / (273.16 + air_T)) # saturated vapor pressure; mb
    vapor_press = rh * sat_vapor_press # vapor pressure at current air temp and humidity; mb ; https://www.weather.gov/media/epz/wxcalc/vaporPressure.pdf
    emiss_air = 0.61 + 0.05 * sqrt(vapor_press) # emissivity of air

    H_a = (1 - r_l) * (1 - Sh) * (1 + 0.17 * C_l^2) * emiss_air * sigma * (air_T + 273.16)^4 # atmospheric longwave radiation; W/m^2

    H_v = emiss_veg * sigma * Sh * (air_T + 273.16)^4 # Riparian vegetation longwave radiation; W/m^2

    H_w = emiss_water * sigma * (out$tw_tminus1[out$model_idx == cur_seg] + 273.16)^4 # water-emitted longwave radiation; W/m^2  #### this might be a negative number? or subtracted?? currently added in Sanders et al. 2017 ####
    if(H_w > 400){H_w = 400}

    H_s = (1 - albedo) * (1 - Sh) * sw_rad # Shortwave solar radiation; W/m^2

    gamma = 2495 * rho - 2360 * out$tw_tminus1[out$model_idx == cur_seg]
    H_e = gamma * (potet / 86400) * rho # Evaporation heat flux; W/m^2 ; converting potet to m / sec
    H_e = (6 + 3*4)*(sat_vapor_press/1.33322 - vapor_press/1.33322) # from Herbert et al. 2011 - seems to give more reasonable evaporative heat flux but this should be looked at again

    atm_press = press_from_elev(temp = air_T, elev = elev) # atmospheric pressure based on elevation and temp; mb
    B_c = (0.00061 * atm_press) / (sat_vapor_press - vapor_press) # Bowen coefficient; dimensionless
    B_o = B_c * (out$tw_tminus1[out$model_idx == cur_seg] - air_T) # Bowen Ratio
    H_c = B_o * H_e  # conductive and convective heat flux; W/m^2

    # from Hipsey et al 2019
    rho_a = calc_air_density(air_T, rh, vapor_press) # air density (kg/m3)
    c_a = 1005 # specific heat capacity of air in J/(kg*degC)
    C_H = 0.0013 # bulk aerodynamic coefficient for sensible heat transfer (unitless)
    U_10 = 4 # wind speed at 10m height (m/s)

    # (kg/m3) * J/(kg*degC) * 1 * (m/s) * degC = (J/s)/m2 = W/m2
    H_c = rho_a * c_a * C_H * U_10 * (out$tw_tminus1[out$model_idx == cur_seg] - air_T)

    H_d = (K_g * (gw_T - out$tw_tminus1[out$model_idx == cur_seg])) / delta_Z # streambed conduction ; W/m^2

    H_f = 9805 * (prms_t$seg_upstream_inflow[prms_t$model_idx == cur_seg] * slope) / width # stream friction ; W/m^2

    H_n = H_a + H_v - H_w + H_s - H_e - H_c + H_d + H_f # net heat flux (non-advective) ; W/m^2
    #H_n = H_a + H_v - H_w + H_s - H_e - H_c + H_f
    #H_n = H_a + H_v + H_s - H_e - H_c + H_f # trying to get some more stability
    #H_n = 0 # debugging

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
      lat_flow = upstream_flow / prms_t$seg_length[prms_t$model_idx == cur_seg] # m2 / sec
      #lat_flow = prms_t$seg_inflow[prms_t$model_idx == cur_seg] / prms_t$seg_length[prms_t$model_idx == cur_seg] # m2 / sec
      lat_temp = (prms_t$seg_tave_gw[prms_t$model_idx == cur_seg] * prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
                  prms_t$seg_tave_sroff[prms_t$model_idx == cur_seg] * prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
                  prms_t$seg_tave_ss[prms_t$model_idx == cur_seg] * prms_t$seginc_ssflow[prms_t$model_idx == cur_seg]) /
        (prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
         prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
         prms_t$seginc_ssflow[prms_t$model_idx == cur_seg]) # C
      cur_outflow = prms_t$seg_outflow[prms_t$model_idx == cur_seg] # cms
      cur_temp = out$tw_tminus1[out$model_idx == cur_seg] # C
      cur_width = width # m

      dT_dlength = ((lat_flow / cur_outflow)*(upstream_temp - cur_temp)) +
        ((cur_width * H_n) / (cur_outflow * rho * C_p))
      #dT_dlength = ((lat_flow / cur_outflow)*(lat_temp - cur_temp)) +
      #  ((cur_width * H_n) / (cur_outflow * rho * C_p))
      #dT_dlength = ((cur_width * H_n) / (cur_outflow * rho * C_p))

      dT = dT_dlength * prms_t$seg_length[prms_t$model_idx == cur_seg]
      #dT = H_n / (C_p * rho * 2)

      out$seg_tave_water[out$model_idx == cur_seg] = out$tw_tminus1[out$model_idx == cur_seg] + dT

    }else{ # headwater segment
      # need to update for headwaters
      # out$seg_tave_water[out$model_idx == cur_seg] = out$tw_tminus1[out$model_idx == cur_seg] + rnorm(1, 0, .2)
      dT_dlength = ((cur_width * H_n) / (cur_outflow * rho * C_p))
      dT = dT_dlength * prms_t$seg_length[prms_t$model_idx == cur_seg]
      # debugging; headwaters responding to air temp
      #dT = (air_T - out$tw_tminus1[out$model_idx == cur_seg]) / 3
      dT = H_n / (C_p * rho * 1)

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


# Simple heat budget based on Sanders et al 2017 and Herbert et al. 2011 and Cole et al. 2014

calc_streamtemp_heat_budget_2 = function(tw_tminus1,
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
    length = prms_t$seg_length[prms_t$model_idx == cur_seg] # segment length; m
    C_l = prms_t$seg_ccov[prms_t$model_idx == cur_seg] # cloud cover fraction; dimensionless
    Sh = prms_t$seg_shade[prms_t$model_idx == cur_seg] # shade fraction; dimensionless
    air_T = prms_t$seg_tave_air[prms_t$model_idx == cur_seg] # air temperature C
    inflow = prms_t$seg_inflow[prms_t$model_idx == cur_seg] # inflowing water to stream segment; cms
    sw_rad = prms_t$seginc_swrad[prms_t$model_idx == cur_seg] # solar radiation ; W/m^2
    potet = prms_t$seginc_potet[prms_t$model_idx == cur_seg] # potential evaporation or free-water surface evaporation; meters
    rh = prms_t$seg_humid[prms_t$model_idx == cur_seg] # relative humidity; decimal fraction
    gw_T = prms_t$seg_tave_gw[prms_t$model_idx == cur_seg] # groundwater temperature; C
    sat_vapor_press = 6.11 * exp((17.27 * air_T) / (273.16 + air_T)) # saturated vapor pressure; mb
    vapor_press = rh * sat_vapor_press # vapor pressure at current air temp and humidity; mb ; https://www.weather.gov/media/epz/wxcalc/vaporPressure.pdf
    emiss_air = 0.61 + 0.05 * sqrt(vapor_press) # emissivity of air

    H_a = (1 - r_l) * (1 - Sh) * (1 + 0.17 * C_l^2) * emiss_air * sigma * (air_T + 273.16)^4 # atmospheric longwave radiation; W/m^2

    H_v = emiss_veg * sigma * Sh * (air_T + 273.16)^4 # Riparian vegetation longwave radiation; W/m^2

    # H_w = emiss_water * sigma * (out$tw_tminus1[out$model_idx == cur_seg] + 273.16)^4 # water-emitted longwave radiation; W/m^2  #### this might be a negative number? or subtracted?? currently added in Sanders et al. 2017 ####
    #if(H_w > 400){H_w = 400}

    H_s = (1 - albedo) * (1 - Sh) * sw_rad # Shortwave solar radiation; W/m^2

    # gamma = 2495 * rho - 2360 * out$tw_tminus1[out$model_idx == cur_seg]
    potet = potet / 86400 # converting potet to m / sec
    # H_e = gamma * (potet) * rho # Evaporation heat flux; W/m^2 ;
    # H_e = (6 + 3*4)*(sat_vapor_press/1.33322 - vapor_press/1.33322) # from Herbert et al. 2011 - seems to give more reasonable evaporative heat flux but this should be looked at again

    atm_press = press_from_elev(temp = air_T, elev = elev) # atmospheric pressure based on elevation and temp; mb
    B_c = (0.00061 * atm_press) / (sat_vapor_press - vapor_press) # Bowen coefficient; dimensionless
    # B_o = B_c * (out$tw_tminus1[out$model_idx == cur_seg] - air_T) # Bowen Ratio
    # H_c = B_o * H_e  # conductive and convective heat flux; W/m^2

    # from Hipsey et al 2019
    # rho_a = calc_air_density(air_T, rh, vapor_press) # air density (kg/m3)
    # c_a = 1005 # specific heat capacity of air in J/(kg*degC)
    # C_H = 0.0013 # bulk aerodynamic coefficient for sensible heat transfer (unitless)
    # U_10 = 4 # wind speed at 10m height (m/s)
    #
    # # (kg/m3) * J/(kg*degC) * 1 * (m/s) * degC = (J/s)/m2 = W/m2
    # H_c = rho_a * c_a * C_H * U_10 * (out$tw_tminus1[out$model_idx == cur_seg] - air_T)

    # H_d = (K_g * (gw_T - out$tw_tminus1[out$model_idx == cur_seg])) / delta_Z # streambed conduction ; W/m^2

    H_f = 9805 * (inflow * slope) / width # stream friction ; W/m^2

    # H_n = H_a + H_v - H_w + H_s - H_e - H_c + H_d + H_f # net heat flux (non-advective) ; W/m^2
    #H_n = H_a + H_v - H_w + H_s - H_e - H_c + H_f
    #H_n = H_a + H_v + H_s - H_e - H_c + H_f # trying to get some more stability
    #H_n = 0 # debugging

    # calculate advective heat flux
    # if(length(cur_upstream_segs) > 0){ # if there are upstream segments, calculate advective heat flux from those segments
    upstream_heat = 0
    upstream_flow = 0
    if(length(cur_upstream_segs) > 0){
      for(j in seq_along(cur_upstream_segs)){
        cur_up_seg = cur_upstream_segs[j]
        upstream_heat = upstream_heat + out$tw_tminus1[out$model_idx == cur_up_seg] * prms_t$seg_upstream_inflow[prms_t$model_idx == cur_up_seg]
        upstream_flow = upstream_flow +prms_t$seg_upstream_inflow[prms_t$model_idx == cur_up_seg]
      }
    }

    upstream_temp = upstream_heat / upstream_flow # average upstream temp
    # heat_outflow = out$tw_tminus1[out$model_idx == cur_seg] * prms_tminus1$seg_outflow[prms_tminus1$model_idx == cur_seg]
    # upstream_temp = upstream_heat / upstream_flow
    #
    # net_advect_heat = upstream_heat - heat_outflow
    #
    # # from Sanders et al 2017 ; equation 27
    # lat_flow = upstream_flow / prms_t$seg_length[prms_t$model_idx == cur_seg] # m2 / sec
    # #lat_flow = prms_t$seg_inflow[prms_t$model_idx == cur_seg] / prms_t$seg_length[prms_t$model_idx == cur_seg] # m2 / sec
    # lat_temp = (prms_t$seg_tave_gw[prms_t$model_idx == cur_seg] * prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
    #               prms_t$seg_tave_sroff[prms_t$model_idx == cur_seg] * prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
    #               prms_t$seg_tave_ss[prms_t$model_idx == cur_seg] * prms_t$seginc_ssflow[prms_t$model_idx == cur_seg]) /
    #   (prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
    #      prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
    #      prms_t$seginc_ssflow[prms_t$model_idx == cur_seg]) # C
    cur_outflow = prms_t$seg_outflow[prms_t$model_idx == cur_seg] # cms
    # cur_temp = out$tw_tminus1[out$model_idx == cur_seg] # C
    # cur_width = width # m
    #
    # dT_dlength = ((lat_flow / cur_outflow)*(upstream_temp - cur_temp)) +
    #   ((cur_width * H_n) / (cur_outflow * rho * C_p))
    # #dT_dlength = ((lat_flow / cur_outflow)*(lat_temp - cur_temp)) +
    # #  ((cur_width * H_n) / (cur_outflow * rho * C_p))
    # #dT_dlength = ((cur_width * H_n) / (cur_outflow * rho * C_p))
    #
    # dT = dT_dlength * prms_t$seg_length[prms_t$model_idx == cur_seg]
    # #dT = H_n / (C_p * rho * 2)
    #
    # out$seg_tave_water[out$model_idx == cur_seg] = out$tw_tminus1[out$model_idx == cur_seg] + dT

    # }else{ # headwater segment
    #   # need to update for headwaters
    #   # out$seg_tave_water[out$model_idx == cur_seg] = out$tw_tminus1[out$model_idx == cur_seg] + rnorm(1, 0, .2)
    #   dT_dlength = ((cur_width * H_n) / (cur_outflow * rho * C_p))
    #   dT = dT_dlength * prms_t$seg_length[prms_t$model_idx == cur_seg]
    #   # debugging; headwaters responding to air temp
    #   #dT = (air_T - out$tw_tminus1[out$model_idx == cur_seg]) / 3
    #   dT = H_n / (C_p * rho * 1)
    #
    #   out$seg_tave_water[out$model_idx == cur_seg] = out$tw_tminus1[out$model_idx == cur_seg] + dT
    # }

    # out$H_n[out$model_idx == cur_seg] = H_n
    # out$H_a[out$model_idx == cur_seg] = H_a
    # out$H_v[out$model_idx == cur_seg] = H_v
    # out$H_w[out$model_idx == cur_seg] = H_w
    # out$H_s[out$model_idx == cur_seg] = H_s
    # out$H_e[out$model_idx == cur_seg] = H_e
    # out$H_c[out$model_idx == cur_seg] = H_c
    # out$H_d[out$model_idx == cur_seg] = H_d
    # out$H_f[out$model_idx == cur_seg] = H_f

    # compute average stream temperature
    # twavg(fs, t_o, qlat, seg_tave_lat(i), te, ak1, ak2, seg_width(i), seg_length(i)) - from https://github.com/nhm-usgs/prms/blob/master/prms/stream_temp.f90
    # fs = is upstream flow (e.g. seg_outflow from upstream segments)
    # t_o = the temperature of the water at the beginning of the time step (this is To in equation 32) - average upstream temp and lateral flows
    # qlat = sum of lateral inflows
    # seg_tave_lat = average temeprature of lateral inflows
    # te = t_o
    # ak1 =
    # ak2 =
    # seg_width = stream width
    # seg_length = stream length
    qlat = prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
      prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
      prms_t$seginc_ssflow[prms_t$model_idx == cur_seg] # cms
    lat_temp = (prms_t$seg_tave_gw[prms_t$model_idx == cur_seg] * prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
                  prms_t$seg_tave_sroff[prms_t$model_idx == cur_seg] * prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
                  prms_t$seg_tave_ss[prms_t$model_idx == cur_seg] * prms_t$seginc_ssflow[prms_t$model_idx == cur_seg]) /
      (prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
         prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
         prms_t$seginc_ssflow[prms_t$model_idx == cur_seg]) # C

    # get t_o, the initial temperature at the beginning of the time step
    if(qlat > 0 & upstream_flow == 0){ # no upstream flow, only lateral flow
      t_o = lat_temp
    }else if(qlat == 0 & upstream_flow > 0){ # no lateral flow but there is upstream flow
      t_o = upstream_temp
    }else if(qlat > 0 & upstream_flow > 0){
      t_o = (upstream_temp * upstream_flow + qlat * lat_temp) / (upstream_flow + qlat)
      t_o = out$tw_tminus1[out$model_idx == cur_seg]
    }


    equilb_list = equilb(ak1d = 0,
                         ak2d = 0,
                         air_T = air_T,
                         t_o = t_o,
                         inflow = inflow,
                         sw_rad = sw_rad,
                         rh = rh,
                         B_c = B_c,
                         potet = potet,
                         H_a = H_a,
                         H_f = H_f,
                         H_s = H_s,
                         H_v = H_v,
                         seg_tave_gw = gw_T)
    ted = equilb_list$ted
    ak1 = equilb_list$ak1d
    ak2 = equilb_list$ak2d
    H_n = equilb_list$hnet

    out$H_n[out$model_idx == cur_seg] = H_n
    out$H_n[out$model_idx == cur_seg] = ak1 * (ted - t_o) + ak2 * (ted - t_o)^2

    # H_e = (6 + 3*4)*(sat_vapor_press/1.33322 - vapor_press/1.33322)
    # H_w = emiss_water * sigma * (ted + 273.16)^4
    # B_o = B_c * (ted - air_T) # Bowen Ratio
    # H_c = B_o * H_e
    # H_d = (K_g * (gw_T - ted)) / delta_Z
    #
    # H_n = H_a + H_v - H_w + H_s - H_e - H_c + H_d + H_f
    # dT_dlength = ((qlat / cur_outflow)*(lat_temp - ted)) +
    #      ((cur_width * H_n) / (cur_outflow * rho * C_p))
    # dT = dT_dlength * prms_t$seg_length[prms_t$model_idx == cur_seg]
    # out$H_a[out$model_idx == cur_seg] = dT
    # out$H_n[out$model_idx == cur_seg] = H_n
    # out$seg_tave_water[out$model_idx == cur_seg] = t_o + dT
    #
    # out$H_n[out$model_idx == cur_seg] = H_n
    # out$H_a[out$model_idx == cur_seg] = H_a
    # out$H_v[out$model_idx == cur_seg] = H_v
    # out$H_w[out$model_idx == cur_seg] = H_w
    # out$H_s[out$model_idx == cur_seg] = H_s
    # out$H_e[out$model_idx == cur_seg] = H_e
    # out$H_c[out$model_idx == cur_seg] = H_c
    # out$H_d[out$model_idx == cur_seg] = H_d
    # out$H_f[out$model_idx == cur_seg] = H_f

    out$seg_tave_water[out$model_idx == cur_seg] = compute_stream_tave(fs = upstream_flow,
                                                                       t_o = t_o,
                                                                       qlat = qlat,
                                                                       seg_tave_lat = lat_temp,
                                                                       te = ted,
                                                                       ak1 = ak1,
                                                                       ak2 = ak2,
                                                                       seg_width = width,
                                                                       seg_length = length)

  }
  # check for bad values; ideally we wouldn't have to look for these upper bounds
  # out$seg_tave_water = ifelse(out$seg_tave_water < 0, 0, out$seg_tave_water)
  # out$seg_tave_water = ifelse(out$seg_tave_water > 36, 36, out$seg_tave_water)

  return(out)
}



calc_streamtemp_heat_budget_3 = function(tw_tminus1,
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
               H_f = NA,
               H_u = NA,
               dT = NA)

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
    length = prms_t$seg_length[prms_t$model_idx == cur_seg] # segment length; m
    C_l = prms_t$seg_ccov[prms_t$model_idx == cur_seg] # cloud cover fraction; dimensionless
    Sh = prms_t$seg_shade[prms_t$model_idx == cur_seg] # shade fraction; dimensionless
    air_T = prms_t$seg_tave_air[prms_t$model_idx == cur_seg] # air temperature; C
    water_T = prms_t$seg_tave_water[prms_t$model_idx == cur_seg] # water temperature; C
    inflow = prms_t$seg_inflow[prms_t$model_idx == cur_seg] # inflowing water to segment; cms
    outflow = prms_t$seg_outflow[prms_t$model_idx == cur_seg] # segment outflow; cms
    sw_rad = prms_t$seginc_swrad[prms_t$model_idx == cur_seg] # solar radiation ; W/m^2
    potet = prms_t$seginc_potet[prms_t$model_idx == cur_seg] # potential evaporation or free-water surface evaporation; meters
    rh = prms_t$seg_humid[prms_t$model_idx == cur_seg] # relative humidity; decimal fraction
    gw_T = prms_t$seg_tave_gw[prms_t$model_idx == cur_seg] # groundwater temperature; C
    sat_vapor_press = 6.11 * exp((17.27 * air_T) / (273.16 + air_T)) # saturated vapor pressure; mb
    vapor_press = rh * sat_vapor_press # vapor pressure at current air temp and humidity; mb ; https://www.weather.gov/media/epz/wxcalc/vaporPressure.pdf
    emiss_air = 0.61 + 0.05 * sqrt(vapor_press) # emissivity of air

    H_a = (1 - r_l) * (1 - Sh) * (1 + 0.17 * C_l^2) * emiss_air * sigma * (air_T + 273.16)^4 # atmospheric longwave radiation; W/m^2

    H_v = emiss_veg * sigma * Sh * (air_T + 273.16)^4 # Riparian vegetation longwave radiation; W/m^2

    H_w = emiss_water * sigma * (water_T + 273.16)^4 # water-emitted longwave radiation; W/m^2  #### this might be a negative number? or subtracted?? currently added in Sanders et al. 2017 ####
    #if(H_w > 400){H_w = 400}

    H_s = (1 - albedo) * (1 - Sh) * sw_rad # Shortwave solar radiation; W/m^2

    gamma = 2495 * rho - 2360 * water_T
    H_e = gamma * (potet / 86400) * rho # Evaporation heat flux; W/m^2 ; converting potet to m / sec
    H_e = (6 + 3*4)*(sat_vapor_press/1.33322 - vapor_press/1.33322) # from Herbert et al. 2011 - seems to give more reasonable evaporative heat flux but this should be looked at again

    atm_press = press_from_elev(temp = air_T, elev = elev) # atmospheric pressure based on elevation and temp; mb
    B_c = (0.00061 * atm_press) / (sat_vapor_press - vapor_press) # Bowen coefficient; dimensionless
    B_o = B_c * (water_T - air_T) # Bowen Ratio
    H_c = B_o * H_e  # conductive and convective heat flux; W/m^2

    # from Hipsey et al 2019
    # rho_a = calc_air_density(air_T, rh, vapor_press) # air density (kg/m3)
    # c_a = 1005 # specific heat capacity of air in J/(kg*degC)
    # C_H = 0.0013 # bulk aerodynamic coefficient for sensible heat transfer (unitless)
    # U_10 = 4 # wind speed at 10m height (m/s)
    #
    # # (kg/m3) * J/(kg*degC) * 1 * (m/s) * degC = (J/s)/m2 = W/m2
    # H_c = rho_a * c_a * C_H * U_10 * (out$tw_tminus1[out$model_idx == cur_seg] - air_T)

    H_d = (K_g * (gw_T - water_T)) / delta_Z # streambed conduction ; W/m^2

    H_f = 9805 * (prms_t$seg_upstream_inflow[prms_t$model_idx == cur_seg] * slope) / width # stream friction ; W/m^2

    H_n = H_a + H_v - H_w + H_s - H_e - H_c + H_d + H_f # net heat flux (non-advective) ; W/m^2
    #H_n = H_a + H_v - H_w + H_s - H_e - H_c + H_f
    #H_n = H_a + H_v + H_s - H_e - H_c + H_f # trying to get some more stability
    #H_n = 0 # debugging

    # calculate advective heat flux
    #if(length(cur_upstream_segs) > 0){ # if there are upstream segments, calculate advective heat flux from those segments
    upstream_heat = 0
    upstream_flow = 0
    for(j in seq_along(cur_upstream_segs)){
      cur_up_seg = cur_upstream_segs[j]
      upstream_heat = upstream_heat + prms_tminus1$seg_tave_water[prms_tminus1$model_idx == cur_up_seg] * prms_tminus1$seg_outflow[prms_tminus1$model_idx == cur_up_seg]
      upstream_flow = upstream_flow +prms_tminus1$seg_outflow[prms_tminus1$model_idx == cur_up_seg]
    }
    # upstream_temp = upstream_heat / upstream_flow # average upstream temp
    heat_outflow = water_T * outflow
    upstream_temp = upstream_heat / upstream_flow

    qlat = prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
      prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
      prms_t$seginc_ssflow[prms_t$model_idx == cur_seg] # cms
    lat_temp = (prms_t$seg_tave_gw[prms_t$model_idx == cur_seg] * prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
                  prms_t$seg_tave_sroff[prms_t$model_idx == cur_seg] * prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
                  prms_t$seg_tave_ss[prms_t$model_idx == cur_seg] * prms_t$seginc_ssflow[prms_t$model_idx == cur_seg]) /
      (prms_t$seginc_gwflow[prms_t$model_idx == cur_seg] +
         prms_t$seginc_sroff[prms_t$model_idx == cur_seg] +
         prms_t$seginc_ssflow[prms_t$model_idx == cur_seg]) # C
    lat_heat = qlat * lat_temp

    # get t_o, the initial temperature at the beginning of the time step
    if(qlat > 0 & upstream_flow == 0){ # no upstream flow, only lateral flow
      t_o = lat_temp
    }else if(qlat == 0 & upstream_flow > 0){ # no lateral flow but there is upstream flow
      t_o = upstream_temp
    }else if(qlat > 0 & upstream_flow > 0){
      t_o = (upstream_heat + lat_heat) / (upstream_flow + qlat)
    }

    net_advect_heat = (upstream_heat + lat_heat) - heat_outflow
    H_u = net_advect_heat * rho * C_p


    dT_dlength = (((inflow / length) / outflow)*(t_o - water_T)) +
      ((width * H_n) / (outflow * rho * C_p))
    #dT_dlength = ((lat_flow / cur_outflow)*(lat_temp - cur_temp)) +
    #  ((cur_width * H_n) / (cur_outflow * rho * C_p))
    #dT_dlength = ((cur_width * H_n) / (cur_outflow * rho * C_p))

    dT = dT_dlength * length

    out$seg_tave_water[out$model_idx == cur_seg] = water_T


    out$H_n[out$model_idx == cur_seg] = H_n
    out$H_a[out$model_idx == cur_seg] = H_a
    out$H_v[out$model_idx == cur_seg] = H_v
    out$H_w[out$model_idx == cur_seg] = H_w
    out$H_s[out$model_idx == cur_seg] = H_s
    out$H_e[out$model_idx == cur_seg] = H_e
    out$H_c[out$model_idx == cur_seg] = H_c
    out$H_d[out$model_idx == cur_seg] = H_d
    out$H_f[out$model_idx == cur_seg] = H_f
    out$H_u[out$model_idx == cur_seg] = H_u
    out$dT[out$model_idx == cur_seg] = dT
  }

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
                                       parameter == 'seg_inflow' ~ parameter_value * cfs_to_m3sec,
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
press_from_elev = function(temp,
                           elev){

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

calc_air_density = function(air_temp,
                            rel_hum,
                            vapPressure){
  # Prepare components for the air density equation
  mwrw2a = 18.016 / 28.966 # ratio of the molecular (or molar) weight of water to dry air
  c_gas = 1.0e3 * 8.31436 / 28.966 # 100./c_gas = 0.348
  p = 1013.0 # atmospheric pressure (mb)

  r = mwrw2a * vapPressure/(p - vapPressure) # water vapor mixing ratio

  # The following is equivalent to 0.348*(1+r)/(1+1.61*r)*p/Ta
  # where 100./c_gas = 0.348, 1./mwrw2a=1.61, and
  # Ta is air_temp in Kelvin = (air_temp + 273.15)
  return(100 * (1.0/c_gas * (1 + r)/(1 + r/mwrw2a) * p/(air_temp + 273.15)))
}


compute_stream_tave = function(fs,
                               t_o,
                               qlat,
                               seg_tave_lat,
                               te,
                               ak1,
                               ak2,
                               seg_width,
                               seg_length){
  # based on SNTemp - https://github.com/nhm-usgs/prms/blob/1d727b47d671eef293f2c6ffd5ffa2896787c02d/prms/stream_temp.f90#L1077

  q1 = qlat
  # no lateral inflow
  if(q1 <= 0){
    tep = te
    b = (ak1 * seg_width) / (4182 * 1000)
    rexp = -1*(b * seg_length) / fs
    r = exp(rexp)
  }else if(q1 > 0 & fs <= 0){
    #  This is a headwaters (i.e. no streamflow from above, but lateral flow from HRUs.
    tep = te
    b = (ak1 * seg_width) / (4182 * 1000)
    rexp = -1*(b * seg_length) / q1
    r = exp(rexp)
  }else{
    # gaining stream (lateral inflows and upstream flow both > 0 )
    # b = (q1 / seg_length) + ((ak1 * seg_width) / (4182 * 1000))
    # tep = (((q1 / seg_length) * seg_tave_lat) + (((ak1 * width) / (4182 * 1000)) * te)) / b
    b = q1 + ((ak1 * seg_width) / (4182 * 1000))
    tep = ((q1 * seg_tave_lat) + (((ak1 * width) / (4182 * 1000)) * te)) / b
    if(q1 > 0 ){
      # rexp = -b / (q1 / seg_length)
      rexp = -b / (q1)
    }else{
      rexp = 0
    }

    if(fs <= 0){
      r = 2
    }else{
      r = 1 + (q1 / fs)
      #r = 1 + (q1 * seg_length / fs)
    }
    r = r^rexp
  }

  delt = tep - t_o
  denom = (1 + (ak2 / ak1) * delt * (1 - r))
  if(abs(denom)<=0){denom = 0.000001}
  tw = tep - (delt * r / denom)
  if(tw < 0){ tw = 0}

  return(tw)
}


equilb = function(ak1d,
                  ak2d,
                  air_T,
                  t_o,
                  inflow,
                  sw_rad,
                  rh,
                  B_c,
                  potet,
                  H_a,
                  H_f,
                  H_s,
                  H_v,
                  seg_tave_gw){
  # average daily equilibrium water temp parameters

  taabs = t_o + 273.16
  vp_sat = 6.108 * exp(17.26939 * t_o / (t_o + 273.16))

  q_init = inflow
  if(q_init <= 0){q_init = 0.000001}

  del_ht = 2.36e6
  ltnt_ht = 2495e6
  k_g = 1.65
  A = 5.4e-8

  if(rh > 0.99){rh = 0.99}

  # ha = ((3.354939e-8 + 2.74995e-9 * sqrt(rh * vp_sat))) * (1-sh) * (1 + (0.17)) #

  b = B_c * potet * (ltnt_ht + (del_ht * t_o)) + k_g - (del_ht * potet)
  #b = 10e6 * potet * (B_c * (2495 + 2.36 * air_T) - 2.36) + k_g
  c = B_c * del_ht * potet
  #c = 10e6 * potet * B_c * 2.36
  d = H_a + H_v + H_f + H_s + (ltnt_ht * potet * ((B_c * t_o) - 1) + (seg_tave_gw * k_g))
  #d = H_a + H_v + H_f + H_s + 2495 * potet * (B_c * air_T - 1) + seg_tave_gw * k_g

  ted = t_o

  # 1st order thermal exchange coef
  therm_coef_1 = therm_1(A, b, c, d, ted, ak1d) # outputs a list
  ak1d = therm_coef_1$ak1c
  ted = therm_coef_1$ted

  # 2nd order thermal exchange coef
  hnet = (A * ((t_o + 273.16)^4)) + (b * t_o) - (c * (t_o^2)) - d
  delt = t_o - ted
  if(abs(delt) <=0){
    ak2d = 0
  }else{
    ak2d = ((delt * ak1d) - hnet) / (delt^2)
  }

  return(list(ted = ted, ak1d = ak1d, ak2d = ak2d, hnet = hnet))

}



therm_1 = function(A,
                   b,
                   c,
                   d,
                   ted,
                   ak1d){
  # DETERMINE THE EQUILIBRIUM WATER TEMPERATURE FROM THE ENERGY BALANCE EQUATION BY ITERATING NEWTON'S METHOD
  tol = 1e-4
  max_iter = 1000
  fte = 99999
  delte = 99999

  for(i in 1:max_iter){
    if(abs(fte) < tol){break}
    if(abs(delte) < tol){break}
    teabs = ted + 273.16
    fte = (A * (teabs^4)) + (b * ted) - (c * (ted^2)) - d
    fpte = (4 * A * (teabs^3)) + b - (2 * c * ted)
    delte = fte / fpte
    ted = ted - delte
  }

  ak1c = (4 * A * ((ted + 273.16)^3)) + b - (2 * c * ted)

  return(list(ak1c = ak1c, ted = ted))

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
