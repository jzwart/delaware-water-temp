
library(dplyr)
library(ggplot2)

d = readRDS('4_model/out/model_out_gwsum_sssum_subbasin_4182_inf_factor.rds')
dd = readRDS('4_model/out/model_out_no_assim_subbasin_4182_inf_factor.rds')
d = readRDS('4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[TRUE]_driver[TRUE]_init[TRUE].rds')

obs = d$obs
Y = d$Y
R = d$R
n_en = 20
n_step = length(d$dates)
dates = d$dates
cur_model_idxs = d$model_locations$model_idx
Y_no_assim = dd$Y

#lordsville site is seg_id_nat == 1573; model_idx = 224
cur_model_idxs = '416'
for(j in cur_model_idxs){
  obs[,1,1]
  matrix_loc = which(d$model_locations$model_idx == j)

  windows(width = 14, height = 8)
  par(mar = c(3,6,4,3))
  plot(Y[matrix_loc,,1] ~ d$dates, type = 'l',
       ylab = 'Stream Temp (C)', xlab = '', lty=0,
       ylim =range(c(Y[matrix_loc,,], obs[matrix_loc,1,]), na.rm = T), #, Y_no_assim[matrix_loc,,])
       cex.axis = 2, cex.lab =2)
  for(i in 1:n_en){
    # lines(Y_no_assim[matrix_loc,,i] ~ d$dates, col = 'grey')
    lines(Y[matrix_loc,,i] ~ d$dates)
  }
  points(obs[matrix_loc,1,] ~ d$dates, col = 'red', pch = 16, cex = 1.2)
  arrows(d$dates, obs[matrix_loc,1,]+R[matrix_loc,matrix_loc,], d$dates, obs[matrix_loc,1,]-R[matrix_loc,matrix_loc,],
  angle = 90, length = .05, col = 'red', code = 3)
}






#
site = 6
params = 42*3 + site
windows()
plot(Y[params,,1], type = 'l', ylim = range(Y[params,,]))
for(i in 1:n_en){
  lines(Y[params,,i])
}
#
# gw_sum = Y[456*1 + site,,]
# gw_tau = Y[456*4 + site,,]
# seg_tave_gw = gw_sum / gw_tau
# ss_sum = Y[456*1 + site,,]
# ss_tau = Y[456*3 + site,,]
# seg_tave_ss = ss_sum / ss_tau
#
# windows()
# plot(seg_tave_gw[,1]~d$dates)
# windows()
# plot(seg_tave_ss[,1]~d$dates)
# windows()
# plot(gw_sum[,9]~d$dates)
#
# windows()
# plot(Y[params,,1], type = 'l', ylim = range(Y[params,,]))
# for(i in 1:n_en){
#   lines(Y[params,,i])
# }

#
# uncert = c()
# uncert_no_assim = c()
# n_obs = c()
# ss_tau = c()
# gw_tau = c()
# for(site in 1:456){
#   # uncertainty at end of run
#   uncert = rbind(uncert, sd(Y[site,n_step,]))
#   uncert_no_assim = rbind(uncert_no_assim, sd(Y_no_assim[site, n_step, ]))
#   n_obs = rbind(n_obs, sum(!is.na(obs[site,1,])))
#   ss_tau = rbind(ss_tau, mean(Y[(456*2 + site), n_step, ]))
#   gw_tau = rbind(gw_tau, mean(Y[(456*3 + site), n_step, ]))
# }
#
# windows()
#
#
# plot(uncert ~ n_obs, ylab = 'Temperature Standard Deviation', xlab= '# of Observations Assimilated', pch =16, cex = 1.5)
#
# loc = cbind(d$model_locations, uncert,uncert_no_assim, n_obs, ss_tau, gw_tau)
# # loc = cbind(d$model_locations, uncert, n_obs)
#
# model_fabric = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') %>%
#   mutate(seg_id_nat = as.character(seg_id_nat),
#          model_idx = as.character(model_idx))
#
# loc = dplyr::left_join(model_fabric, loc, by = c('seg_id_nat', 'model_idx'))
#
# library(ggplot2)
# windows()
# ggplot() +
#   geom_sf(data = loc, aes(color = log10(uncert)))+
#   scale_color_viridis_c() +
#   theme_minimal()+
#   theme(legend.title = element_text('Temp SD'))
#
# # reduction in uncertainty
# windows()
# ggplot() +
#   geom_sf(data = loc, aes(color = (uncert_no_assim - uncert), size = .01*(uncert_no_assim - uncert)))+
#   scale_color_viridis_c(direction = -1) +
#   theme_minimal()
#
# windows()
# ggplot() +
#   geom_sf(data = dplyr::filter(loc, n_obs >0), color = 'red')+
#   geom_sf(data = dplyr::filter(loc, n_obs ==0), color = 'grey90')+
#   theme_minimal()
#
# loc$red_uncert = loc$uncert_no_assim - loc$uncert
# windows()
# plot(loc$red_uncert ~ loc$n_obs)
# abline(lm(loc$red_uncert~loc$n_obs))
#
# windows()
# # loc$ss_tau = loc$ss_tau + 40
# ggplot() +
#   geom_sf(data = loc, aes(color = ss_tau ))+
#   scale_color_viridis_c() +
#   theme_minimal()+
#   theme(legend.title = element_text('ss_tau'))
#
#
# windows()
# ggplot() +
#   geom_sf(data = loc, aes(color = log10(uncert)))+
#   scale_color_viridis_c() +
#   theme_minimal()+
#   theme(legend.title = element_text('Temp SD'))

# RMSE
time_period = 1:n_step
mean_Y = rowMeans(Y, dims = 2) # mean of ensembles for each time step
mean_Y_no_assim = rowMeans(Y_no_assim, dims = 2)
mean_temp = mean_Y[1:length(cur_model_idxs), time_period]
mean_temp_no_assim = mean_Y_no_assim[1:length(cur_model_idxs), time_period]
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
  geom_boxplot(size=2) +
  # geom_point(aes(color = reduced, size = diff)) +
  # geom_line(aes(group = id, color = reduced), linetype = '11')+
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


loc = cbind(d$model_locations, temp_rmse, temp_rmse_no_assim)

model_fabric = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') %>%
  mutate(seg_id_nat = as.character(seg_id_nat),
         model_idx = as.character(model_idx))

temp = as_tibble(mean_temp) %>%
  magrittr::set_colnames(dates) %>%
  mutate(model_idx = cur_model_idxs) %>%
  pivot_longer(names_to = 'date', values_to = 'temp_est', cols = -model_idx) %>%
  mutate(date = as.Date(date))
temp_no_assim = as_tibble(mean_temp_no_assim) %>%
  magrittr::set_colnames(dates) %>%
  mutate(model_idx = cur_model_idxs) %>%
  pivot_longer(names_to = 'date', values_to = 'temp_est_no_assim', cols = -model_idx) %>%
  mutate(date = as.Date(date))

spatial_temp = dplyr::left_join(model_fabric, temp, by = c('model_idx'))%>% dplyr::filter(model_idx %in% cur_model_idxs) %>%
  dplyr::left_join(temp_no_assim, by = c('model_idx', 'date'))

loc = dplyr::left_join(model_fabric, loc, by = c('seg_id_nat', 'model_idx')) %>% dplyr::filter(model_idx %in% cur_model_idxs)

loc$reduction_rmse = loc$temp_rmse_no_assim - loc$temp_rmse

library(ggplot2)
windows()
ggplot() +
  geom_sf(data = dplyr::filter(loc, !is.na(temp_rmse)), aes(color = temp_rmse), size =2)+
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

cur_date = as.Date('2014-07-01')
windows()
ggplot() +
  geom_sf(data = dplyr::filter(spatial_temp, date == cur_date), aes(color = temp_est_no_assim))+
  scale_color_viridis_c() +
  theme_minimal()

# library(gganimate)
# library(transformr)
# windows()
# plot = ggplot() +
#   geom_sf(data = spatial_temp, aes(color = temp_est))+
#   scale_color_viridis_c() +
#   theme_minimal() +
#   transition_time(time = date)
#
# plot


plot(loc$reduction_rmse ~ loc$n_obs, ylab ='Reduction in RMSE', xlab = '# of Obs Assimilated', pch =16, cex = 1.5)

sum(loc$reduction_rmse/ length(!is.na(loc$n_obs)), na.rm = T)

hist(loc$reduction_rmse)

loc$model_idx[which(loc$reduction_rmse == max(loc$reduction_rmse, na.rm = T))]

