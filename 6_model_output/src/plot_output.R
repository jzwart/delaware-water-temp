
library(dplyr)

d = readRDS('4_model/out/model_out2.rds')
dd = readRDS('4_model/out/model_out_no_assim.rds')

obs = d$obs
Y = d$Y
R = d$R
n_en = 20
n_step = length(d$dates)
Y_no_assim = dd$Y

#lordsville site is seg_id_nat == 1573; model_idx = 224
obs[,1,1]
site = 79
windows()
plot(Y[site,,1] ~ d$dates, type = 'l',ylim =  range(c(Y[site,,], obs[site,1,], Y_no_assim[site,,]), na.rm = T), ylab = 'Stream Temp (C)', xlab = '', lty=0)
for(i in 1:n_en){
  lines(Y_no_assim[site,1:71,i] ~ d$dates, col = 'grey')
  lines(Y[site,,i] ~ d$dates)
}
points(obs[site,1,] ~ d$dates, col = 'red', pch = 16, cex = 1.2)
arrows(d$dates, obs[site,1,]+R[site,site,], d$dates, obs[site,1,]-R[site,site,],
       angle = 90, length = .05, col = 'red', code = 3)

params = 456*2 + site
windows()
plot(Y[params,,1], type = 'l', ylim = range(Y[params,,]))
for(i in 1:n_en){
  lines(Y[params,,i])
}

gw_sum = Y[456*2 + site,,]
gw_tau = Y[456*4 + site,,]
seg_tave_gw = gw_sum / gw_tau
ss_sum = Y[456*1 + site,,]
ss_tau = Y[456*3 + site,,]
seg_tave_ss = ss_sum / ss_tau

windows()
plot(seg_tave_gw[,1]~d$dates)
windows()
plot(seg_tave_ss[,1]~d$dates)


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
  ss_tau = rbind(ss_tau, mean(Y[(456*2 + site), n_step, ]))
  gw_tau = rbind(gw_tau, mean(Y[(456*3 + site), n_step, ]))
}

windows()


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
  geom_sf(data = loc, aes(color = (uncert_no_assim - uncert), size = .01*(uncert_no_assim - uncert)))+
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
# loc$ss_tau = loc$ss_tau + 40
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
rmse_diff = temp_rmse_no_assim - temp_rmse
rmse_reduced = ifelse(rmse_diff>0, 'decrease', 'increase')

rmse_df = tibble(rmse = c(temp_rmse, temp_rmse_no_assim),
                 da = c(rep('DA', length(temp_rmse)), rep('no_DA', length(temp_rmse_no_assim))),
                 diff = rep(abs(rmse_diff), 2),
                 reduced = rep(rmse_reduced, 2),
                 id = rep(1:length(rmse_diff), 2))

b <- runif(nrow(rmse_df), -0.1, 0.1)

rmse_df$da = factor(x = rmse_df$da, levels = c('no_DA', 'DA'))

windows()
ggplot(rmse_df, aes(x = da, y = rmse)) +
  geom_boxplot() +
  geom_point(aes(color = reduced, size = diff)) +
  geom_line(aes(group = id, color = reduced), linetype = '11')+
  theme_classic() +
  scale_color_manual(name = 'reduced',
                     values = c('blue', 'red'))+
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab('') +
  ylab(expression(RMSE~(degrees~C)))


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

