

d = readRDS('4_model/out/model_out.rds')
dd = readRDS('4_model/out/model_out_no_assim.rds')

obs = d$obs
Y = d$Y
R = d$R
n_en = 20
n_step = length(d$dates)
Y_no_assim = dd$Y

obs[,1,1]
site = 377
plot(Y[site,,1], type = 'l',ylim =  range(c(Y[site,,], obs[site,1,]), na.rm = T), ylab = 'Stream Temp (C)', xlab = 'Time Step')
for(i in 1:n_en){
  lines(Y_no_assim[site,,i], col = 'grey')
  lines(Y[site,,i])
}
points(obs[site,1,], col = 'red', pch = 16, cex = 1.2)
arrows(1:n_step, obs[site,1,]+R[site,site,], 1:n_step, obs[site,1,]-R[site,site,],
       angle = 90, length = .05, col = 'red', code = 3)

params = 456*4 + site
windows()
plot(Y[params,,1], type = 'l', ylim = range(Y[params,,]))
for(i in 1:n_en){
  lines(Y[params,,i])
}



uncert = c()
uncert_no_assim = c()
n_obs = c()
ss_tau = c()
gw_tau = c()
for(site in 1:456){
  # uncertainty at end of run
  uncert = rbind(uncert, sd(Y[site,n_step,]))
  uncert_no_assim = rbind(uncert_no_assim, sd(Y_no_assim[site, n_step, ]))
  n_obs = rbind(n_obs, sum(!is.na(obs[site,1,])))
  ss_tau = rbind(ss_tau, mean(Y[(456*1 + site), n_step, ]))
  gw_tau = rbind(gw_tau, mean(Y[(456*2 + site), n_step, ]))
}

plot(uncert ~ n_obs, ylab = 'Temperature Standard Deviation', xlab= '# of Observations Assimilated', pch =16, cex = 1.5)

loc = cbind(d$model_locations, uncert,uncert_no_assim, n_obs, ss_tau, gw_tau)
# loc = cbind(d$model_locations, uncert, n_obs)

model_fabric = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') %>%
  mutate(seg_id_nat = as.character(seg_id_nat),
         model_idx = as.character(model_idx))

loc = dplyr::left_join(model_fabric, loc, by = c('seg_id_nat', 'model_idx'))

library(ggplot2)
windows()
ggplot() +
  geom_sf(data = loc, aes(color = log10(uncert)))+
  scale_color_viridis_c() +
  theme_minimal()+
  theme(legend.title = element_text('Temp SD'))

# reduction in uncertainty
windows()
ggplot() +
  geom_sf(data = loc, aes(color = (uncert_no_assim - uncert), size = .1*(uncert_no_assim - uncert)))+
  scale_color_viridis_c(direction = -1) +
  theme_minimal()

windows()
ggplot() +
  geom_sf(data = dplyr::filter(loc, n_obs >0), color = 'red')+
  geom_sf(data = dplyr::filter(loc, n_obs ==0), color = 'grey90')+
  theme_minimal()

loc$red_uncert = loc$uncert_no_assim - loc$uncert
windows()
plot(loc$red_uncert ~ loc$n_obs)
abline(lm(loc$red_uncert~loc$n_obs))

windows()
loc$ss_tau = loc$ss_tau + 40
ggplot() +
  geom_sf(data = loc, aes(color = ss_tau ))+
  scale_color_viridis_c() +
  theme_minimal()+
  theme(legend.title = element_text('ss_tau'))


windows()
ggplot() +
  geom_sf(data = loc, aes(color = log10(uncert)))+
  scale_color_viridis_c() +
  theme_minimal()+
  theme(legend.title = element_text('Temp SD'))

# RMSE
time_period = 1:n_step
mean_Y = rowMeans(Y, dims = 2) # mean of ensembles for each time step
mean_Y_no_assim = rowMeans(Y_no_assim, dims = 2)
mean_temp = mean_Y[1:456, time_period]
mean_temp_no_assim = mean_Y_no_assim[1:456, time_period]
temp_rmse = sqrt(rowMeans((mean_temp - obs[,1,time_period])^2, na.rm = T))
temp_rmse_no_assim = sqrt(rowMeans((mean_temp_no_assim - obs[,1,time_period])^2, na.rm = T))

hist(temp_rmse)
hist(temp_rmse_no_assim)

loc = cbind(d$model_locations, uncert, uncert_no_assim, n_obs, temp_rmse, temp_rmse_no_assim)

model_fabric = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') %>%
  mutate(seg_id_nat = as.character(seg_id_nat),
         model_idx = as.character(model_idx))

loc = dplyr::left_join(model_fabric, loc, by = c('seg_id_nat', 'model_idx'))

loc$reduction_rmse = loc$temp_rmse_no_assim - loc$temp_rmse

library(ggplot2)
windows()
ggplot() +
  geom_sf(data = dplyr::filter(loc, !is.na(temp_rmse)), aes(color = temp_rmse))+
  scale_color_viridis_c() +
  theme_minimal()+
  geom_sf(data = dplyr::filter(loc, is.na(temp_rmse)), color = 'grey90')

windows()
ggplot() +
  geom_sf(data = dplyr::filter(loc, !is.na(reduction_rmse)), aes(color = reduction_rmse,size = abs(reduction_rmse)))+
  # scale_color_distiller(palette = 'RdBu', limits = c(-1,1)*max(abs(loc$reduction_rmse))) +
  scale_color_gradient2() +
  theme_minimal()+
  geom_sf(data = dplyr::filter(loc, is.na(reduction_rmse)), color = 'grey90')

plot(loc$reduction_rmse ~ loc$n_obs, ylab ='Reduction in RMSE', xlab = '# of Obs Assimilated', pch =16, cex = 1.5)

sum(loc$reduction_rmse/ length(!is.na(loc$n_obs)), na.rm = T)

hist(loc$reduction_rmse)

loc$model_idx[which(loc$reduction_rmse == max(loc$reduction_rmse, na.rm = T))]

