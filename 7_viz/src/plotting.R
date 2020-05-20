
#lordsville site is seg_id_nat == 1573; model_idx = 224

library(ggplot2)
library(tidyverse)

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


output = readRDS('4_model/out/model_out2.rds')
output_no_assim = readRDS('4_model/out/model_out_no_assim.rds')
compare_to_no_assim = T

dates = output$dates
start = '2014-05-10'
stop = '2014-07-09'
start_idx = which(dates == start)
stop_idx = which(dates == stop)

n_obs = function(x){sum(!is.na(x))}

most_obs = tibble(model_idx = output$model_locations$model_idx,
                  n_obs = apply(output$obs[,1,], MARGIN = 1, FUN = n_obs)) %>%
  arrange(desc(n_obs))
most_obs

# 1573; 224 == lordsville
# 1450; 122 == Downstream of Pepacton Res
# 1566; 217 == Downstream of Cannonsville Res

site_index = 224

site = output$model_locations$seg_id_nat[site_index]
site_est_mean = apply(output$Y[site_index, start_idx:stop_idx, ], MARGIN = 1, FUN = mean)
site_est_sd = apply(output$Y[site_index, start_idx:stop_idx, ], MARGIN = 1, FUN = sd)
site_obs = output$obs[site_index, , start_idx:stop_idx]
site_obs_var = output$R[site_index, site_index, start_idx:stop_idx] # site observation variance
if(compare_to_no_assim){
  site_no_assim_mean = apply(output_no_assim$Y[site_index, which(output_no_assim$dates == start):which(output_no_assim$dates == stop) , ], MARGIN = 1, FUN = mean)

  site_data = tibble(site_id = rep(site, length(output$dates[start_idx:stop_idx])),
                     date = output$dates[start_idx:stop_idx],
                     temp_est = site_est_mean,
                     temp_est_sd = site_est_sd,
                     temp_obs = site_obs,
                     temp_obs_var = site_obs_var,
                     temp_est_no_assim = site_no_assim_mean)
}else{
  site_data = tibble(site_id = rep(site, length(output$dates[start_idx:stop_idx])),
                     date = output$dates[start_idx:stop_idx],
                     temp_est = site_est_mean,
                     temp_est_sd = site_est_sd,
                     temp_obs = site_obs,
                     temp_obs_var = site_obs_var)
}




windows()
ggplot(data = site_data) +
  geom_line(aes(x = date, y = temp_est_no_assim), col = t_col('black', 70), size = 1)+
  geom_line(aes(x = date, y = temp_est), col = 'blue', size = 1) +
  geom_point(aes(x = date, y = temp_obs), col = 'grey30', alpha = .3, size = 4) +
  theme_classic() +
  theme(axis.title = element_text(size = axes_text_size),
        axis.text = element_text(size = axes_text_size)) +
  ylab('Temperature (C)') +
  xlab('')

Metrics::rmse(site_data$temp_obs, site_data$temp_est_no_assim)
Metrics::rmse(site_data$temp_obs, site_data$temp_est)


uncert = add_uncertainty_ribbons(data = site_data)

if(compare_to_no_assim){
  windows()
  uncert + geom_line(data = site_data, aes(x = date, y= temp_est_no_assim), col = t_col('grey', percent = 30))
}else{
  windows()
  uncert
}





for(sites in most_obs$model_idx[1:10]){
  site_index = as.numeric(sites)

  site = output$model_locations$seg_id_nat[site_index]
  site_est_mean = apply(output$Y[site_index, start_idx:stop_idx, ], MARGIN = 1, FUN = mean)
  site_est_sd = apply(output$Y[site_index, start_idx:stop_idx, ], MARGIN = 1, FUN = sd)
  site_obs = output$obs[site_index, , start_idx:stop_idx]
  site_obs_var = output$R[site_index, site_index, start_idx:stop_idx] # site observation variance
  if(compare_to_no_assim){
    site_no_assim_mean = apply(output_no_assim$Y[site_index, which(output_no_assim$dates == start):which(output_no_assim$dates == stop), ], MARGIN = 1, FUN = mean)

    site_data = tibble(site_id = rep(site, length(output$dates[start_idx:stop_idx])),
                       date = output$dates[start_idx:stop_idx],
                       temp_est = site_est_mean,
                       temp_est_sd = site_est_sd,
                       temp_obs = site_obs,
                       temp_obs_var = site_obs_var,
                       temp_est_no_assim = site_no_assim_mean)
  }else{
    site_data = tibble(site_id = rep(site, length(output$dates[start_idx:stop_idx])),
                       date = output$dates[start_idx:stop_idx],
                       temp_est = site_est_mean,
                       temp_est_sd = site_est_sd,
                       temp_obs = site_obs,
                       temp_obs_var = site_obs_var)
  }


  uncert = add_uncertainty_ribbons(data = site_data)

  if(compare_to_no_assim){
    windows(width = 12, height = 8)
    print(uncert + geom_line(data = site_data, aes(x = date, y= temp_est_no_assim), col = t_col('grey', percent = 30)) +
      ggtitle(paste('seg_id_nat', site)))
  }else{
    windows()
    print(uncert +
      ggtitle(paste('seg_id_nat', site)))
  }
}



# doing facet wrap
all_sites = tibble()
for(sites in most_obs$model_idx[1:9]){
  site_index = as.numeric(sites)

  site = output$model_locations$seg_id_nat[site_index]
  site_est_mean = apply(output$Y[site_index, start_idx:stop_idx, ], MARGIN = 1, FUN = mean)
  site_est_sd = apply(output$Y[site_index, start_idx:stop_idx, ], MARGIN = 1, FUN = sd)
  site_obs = output$obs[site_index, , start_idx:stop_idx]
  site_obs_var = output$R[site_index, site_index, start_idx:stop_idx] # site observation variance
  if(compare_to_no_assim){
    site_no_assim_mean = apply(output_no_assim$Y[site_index, which(output_no_assim$dates == start):which(output_no_assim$dates == stop), ], MARGIN = 1, FUN = mean)

    site_data = tibble(site_id = rep(site, length(output$dates[start_idx:stop_idx])),
                       date = output$dates[start_idx:stop_idx],
                       temp_est = site_est_mean,
                       temp_est_sd = site_est_sd,
                       temp_obs = site_obs,
                       temp_obs_var = site_obs_var,
                       temp_est_no_assim = site_no_assim_mean)
  }else{
    site_data = tibble(site_id = rep(site, length(output$dates[start_idx:stop_idx])),
                       date = output$dates[start_idx:stop_idx],
                       temp_est = site_est_mean,
                       temp_est_sd = site_est_sd,
                       temp_obs = site_obs,
                       temp_obs_var = site_obs_var)
  }

  all_sites = rbind(all_sites, site_data)
}



# plot all sites
windows()
ggplot(data = all_sites) +
  geom_line(aes(x = date, y = temp_est_no_assim), col = t_col('grey', 10))+
  geom_line(aes(x = date, y = temp_est), col = 'blue') +
  geom_point(aes(x = date, y = temp_obs), col = 'grey30', alpha = .3, size = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = axes_text_size),
        axis.text = element_text(size = 12)) +
  facet_wrap(~site_id, scales = 'free_y') +
  ylab('Temperature (C)') +
  xlab('')

Metrics::rmse(all_sites$temp_est, all_sites$temp_obs)


dim(output$Y)




