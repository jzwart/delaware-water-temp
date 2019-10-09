


library(ggplot2)
library(tidyr)
library(dplyr)

output = readRDS('4_model/tmp/model_out.rds')

site_index = 4

site = output$model_locations[site_index]
site_est_mean = apply(output$Y[site_index, , ], MARGIN = 1, FUN = mean)
site_est_sd = apply(output$Y[site_index, , ], MARGIN = 1, FUN = sd)
site_obs = output$obs[site_index, , ]
site_obs_var = output$R[site_index, site_index, ] # site observation variance

site_data = tibble(site_id = rep(site, length(output$dates)),
                   date = output$dates,
                   temp_est = site_est_mean,
                   temp_est_sd = site_est_sd,
                   temp_obs = site_obs,
                   temp_obs_var = site_obs_var)

axes_text_size = 18


t_col <- function(color, percent = 50, name = NULL) {
  #	  color = color name
  #	percent = % transparency
  #	   name = an optional name for the color
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  ## Save the color
  invisible(t.col)

}

add_uncertainty_ribbons = function(data){
  uncert = ggplot(data = data) +
    geom_ribbon(aes(x = date, ymin = temp_est - temp_est_sd*2, ymax = temp_est + temp_est_sd*2),
                fill = t_col('lightblue',percent = 90), col = t_col('lightblue',percent = 90)) +
    geom_ribbon(aes(x = date, ymin = temp_est - temp_est_sd*1.5, ymax = temp_est + temp_est_sd*1.5),
                fill = t_col('lightblue',percent = 80), col = t_col('lightblue',percent = 80)) +
    geom_ribbon(aes(x = date, ymin = temp_est - temp_est_sd*1.25, ymax = temp_est + temp_est_sd*1.25),
                fill = t_col('lightblue',percent = 80), col = t_col('lightblue',percent = 80)) +
    geom_ribbon(aes(x = date, ymin = temp_est - temp_est_sd, ymax = temp_est + temp_est_sd),
                fill = t_col('lightblue',percent = 80), col = t_col('lightblue',percent = 80)) +
    geom_ribbon(aes(x = date, ymin = temp_est - temp_est_sd*.75, ymax = temp_est + temp_est_sd*.75),
                fill = t_col('lightblue',percent = 80), col = t_col('lightblue',percent = 80)) +
    geom_ribbon(aes(x = date, ymin = temp_est - temp_est_sd*.5, ymax = temp_est + temp_est_sd*.5),
                fill = t_col('lightblue',percent = 80), col = t_col('lightblue',percent = 80))  +
    geom_line(aes(x = date, y = temp_est), col = t_col('blue',percent = 50)) +
    geom_point(aes(x = date, y = temp_obs), col = 'grey30', alpha = .3, size = 2) +
    theme_classic() +
    theme(axis.title = element_text(size = axes_text_size),
          axis.text = element_text(size = axes_text_size)) +
    ylab('Temperature (C)') +
    xlab('')

  return(uncert)
}

uncert = add_uncertainty_ribbons(data = dplyr::filter(site_data, date < as.Date('2011-01-01')))

uncert

ggplot(data = site_data) +
  geom_line(aes(x = date, y = temp_est), col = 'blue') +
  geom_point(aes(x = date, y = temp_obs), col = 'grey30', alpha = .3, size = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = axes_text_size),
        axis.text = element_text(size = axes_text_size)) +
  ylab('Temperature (C)') +
  xlab('')



ggplot(data = site_data) +
  geom_ribbon(aes(x = date, ymin = temp_est - temp_est_sd, ymax = temp_est + temp_est_sd),
              fill = 'lightblue', col = 'lightblue') +
  geom_line(aes(x = date, y = temp_est), col = 'blue') +
  geom_point(aes(x = date, y = temp_obs), col = 'grey30', alpha = .3, size = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = axes_text_size),
        axis.text = element_text(size = axes_text_size)) +
  ylab('Temperature (C)') +
  xlab('')

ggplot(data = site_data) +
  geom_point(aes(x = temp_obs, y = temp_est), col = 'black', alpha = .3, size = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = axes_text_size),
        axis.text = element_text(size = axes_text_size)) +
  geom_abline(slope = 1, intercept = 0) +
  ylab('Temperature (C)') +
  xlab('')

#
# windows()
# plot(Y[1, ,1] ~ dates, type = 'l', col = 'grey', ylab= 'Temp (C)')
# for(n in 1:n_en){
#   lines(Y[1,,n] ~ dates, col ='grey')
# }
# lines(apply(Y[1,,], MARGIN = 1, FUN = mean) ~ dates, col = 'black')
# points(obs[1,1,] ~ dates)
# # arrows(dates,
# #        obs[1,1,]-state_sd[1],
# #        dates,
# #        obs[1,1,]+state_sd[1],
# #        code=3, length=0.1, angle=90, col='black')
#
#
# windows()
# plot(Y[4, ,1] ~ dates, type = 'l', col = 'grey', ylab= 'Temp (C)')
# for(n in 1:n_en){
#   lines(Y[4,,n] ~ dates, col ='grey')
# }
# lines(apply(Y[4,,], MARGIN = 1, FUN = mean) ~ dates, col = 'black')
# points(obs[4,1,] ~ dates)
#
# windows()
# plot(Y[3, ,1] ~ dates, type = 'l', col = 'grey', ylab= 'Temp (C)')
# for(n in 1:n_en){
#   lines(Y[3,,n] ~ dates, col ='grey')
# }
# lines(apply(Y[3,,], MARGIN = 1, FUN = mean) ~ dates, col = 'black')
# points(obs[3,1,] ~ dates)
#
#
# windows()
# plot(apply(Y[1,,], MARGIN = 1, FUN = mean) ~ obs[1,1,])
# abline(0,1, lty =2 ,col ='red', lwd = 2)
#
# windows()
# plot(apply(Y[4,,], MARGIN = 1, FUN = mean) ~ obs[4,1,])
# abline(0,1, lty =2 ,col ='red', lwd = 2)








