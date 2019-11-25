

d = readRDS('4_model/out/model_out.rds')

obs = d$obs
Y = d$Y
R = d$R
n_en = 20
n_step = 31

obs[,1,1]
site = 455
plot(Y[site,,1], type = 'l',ylim =  range(c(Y[site,,], obs[site,1,]), na.rm = T))
for(i in 1:n_en){
  lines(Y[site,,i])
}
points(obs[site,1,], col = 'red')
arrows(1:n_step, obs[site,1,]+R[site,site,], 1:n_step, obs[site,1,]-R[site,site,],
       angle = 90, length = .1, col = 'red', code = 3)

params = 456 + site
windows()
plot(Y[params,,1], type = 'l', ylim = range(Y[params,,]))
for(i in 1:n_en){
  lines(Y[params,,i])
}



uncert = c()
n_obs = c()
for(site in 1:456){
  # uncertainty at end of run
  uncert = rbind(uncert, sd(Y[site,31,]))
  n_obs = rbind(n_obs, sum(!is.na(obs[site,1,])))
}
/mean(Y[site,31,])

plot(uncert ~ n_obs)

loc = cbind(d$model_locations, uncert, n_obs)

model_fabric = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') %>%
  mutate(seg_id_nat = as.character(seg_id_nat),
         model_idx = as.character(model_idx))

loc = dplyr::left_join(model_fabric, loc, by = c('seg_id_nat', 'model_idx'))

windows()
ggplot() +
  geom_sf(data = loc, aes(color = log10(uncert)))+
  scale_color_viridis_c() +
  theme_minimal()

windows()
ggplot() +
  geom_sf(data = dplyr::filter(loc, n_obs >0), color = 'red')+
  geom_sf(data = dplyr::filter(loc, n_obs ==0), color = 'grey90')+
  theme_minimal()


# w/o assimilating obs
for(n in 1:n_en){
  # set parameters / states for model config
  t=2
  updated_params = get_updated_params(Y = Y,
                                      param_names = param_names,
                                      n_states_est = n_states_est,
                                      n_params_est = n_params_est,
                                      cur_step = t-1,
                                      en = n)

  update_sntemp_params(param_names = param_names,
                       updated_params = updated_params)

  run_sntemp(start = dates[1], stop = dates[length(dates)], spinup = T,
             model_run_loc = model_run_loc,
             spinup_days = 730,
             restart = T,
             precip_file = sprintf('./input/prcp_%s.cbh', n),
             tmax_file = sprintf('./input/tmax_%s.cbh', n),
             tmin_file = sprintf('./input/tmin_%s.cbh', n),
             var_init_file = sprintf('prms_ic_%s.out', n),
             var_save_file = sprintf('prms_ic_%s.out', n))

  model_output = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                        model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp')) %>%
    dplyr::filter(date %in% dates)

  for(t in 1:length(dates)){
    cur_out = dplyr::filter(model_output, date == dates[t])
    Y[ , t, n] = c(cur_out$water_temp, updated_params) # store in Y vector
  }
}



windows()
ggplot() +
  geom_sf(data = loc, aes(color = log(loc_no_DA$uncert - loc$uncert)))+
  scale_color_viridis_c(direction = -1) +
  theme_minimal()

uncert_diff = loc_no_DA$uncert - loc$uncert

plot(uncert_diff~loc$n_obs)







