

# variance partitioning
library(tidyverse)
library(ncdf4)
library(ggplot2)
library(scales)
source('4_model_forecast/src/nc_forecast_utils.R')
start = as.Date('2019-05-01')
end = as.Date('2019-08-31')
out_dates = seq.Date(from = start, to = end, by = 'days')


forecast_out_file = '4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[TRUE]_driver[TRUE]_init[TRUE].nc'
obs_file = '3_observations/out/obs_temp_flow.rds'
model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp'
model_fabric = sf::read_sf(model_fabric_file)
model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                         model_idx = as.character(model_fabric$model_idx)) %>%
  arrange(as.numeric(model_idx))

obs = readRDS(obs_file)
temp_obs = obs$temp %>%
  left_join(model_locations, by = 'seg_id_nat')
flow_obs = obs$flow %>%
  left_join(model_locations, by = 'seg_id_nat')
da_out = readRDS('4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[TRUE]_driver[TRUE]_init[TRUE].rds')
all_dates = da_out$dates
cur_model_idxs = da_out$model_locations$model_idx
temp_adj = da_out$Y[1:42,,] %>%
  reshape2::melt(varnames = c('model_idx', 'issue_time', 'ensemble')) %>%
  mutate(issue_time = all_dates[issue_time],
         model_idx = cur_model_idxs[model_idx],
         valid_time = issue_time,
         ensemble = ensemble) %>%
  rename(seg_tave_water = value) %>%
  as_tibble()

# I = initial condition error, D = Driver error, P = parameter error
IPD = nc_forecast_get(nc_file = forecast_out_file, var_name = 'seg_tave_water', issue_dates = out_dates)
IPD_var = IPD %>%
  left_join(temp_adj, by =c('model_idx','valid_time','issue_time', 'ensemble'), suffix = c('','_adj')) %>%
  # adding in DA adjustment
  mutate(seg_tave_water = ifelse(!is.na(seg_tave_water_adj), seg_tave_water_adj, seg_tave_water)) %>%
  select(-seg_tave_water_adj) %>%
  group_by(model_idx, issue_time, valid_time) %>%
  summarise(variance = var(seg_tave_water),
         sd = sd(seg_tave_water),
         cv = sd / mean(seg_tave_water)) %>%
  ungroup() %>%
  mutate(lead_time = as.integer(valid_time - issue_time),
         error = 'IPD') %>%
  left_join(select(temp_obs, model_idx, date, temp_C), by = c('model_idx' = 'model_idx', 'valid_time' = 'date'))# %>%
  # left_join(select(flow_obs, model_idx, date, discharge_cms), by = c('model_idx' = 'model_idx', 'valid_time' = 'date'))

# plot(IPD_var$sd)

most_obs = c('416', '422', '423', '446', '455', '421', '439', '442', '445') # segments with most obs
most_obs = c('416', '422', '423', '446', '455', '439') # segments with most obs

# IPD_var = dplyr::filter(IPD_var, model_idx %in% most_obs )

# IPD_var_group = IPD_var %>%
#   mutate(month = lubridate::month(issue_time)) %>%
#   group_by(lead_time, model_idx, month) %>%
#   summarise(cv = median(cv, na.rm = T),
#             sd = mean(sd, na.rm = T),
#             mean_disch = mean(discharge_cms, na.rm = T)) %>%
#   ungroup()
#
#
# ggplot(IPD_var_group, aes(x = lead_time, y = cv, group = model_idx, color = model_idx)) +
#   geom_point()+
#   geom_smooth(se = F)
#
# ggplot(IPD_var_group, aes(x = lead_time, y = sd, group = month, color = month)) +
#   geom_point()+
#   geom_smooth(se = F)
#
# ggplot(IPD_var_group, aes(x = month, y = cv, group = lead_time, color = lead_time)) +
#   geom_point()+
#   geom_smooth(se = F)
#
# ggplot(IPD_var_group, aes(x = month, y = cv, group = lead_time, color = lead_time)) +
#   geom_point()+
#   geom_smooth(se = F)
#
# ggplot(IPD_var_group, aes(x = issue_time, y = cv, group = lead_time, color = lead_time)) +
#   geom_point()+
#   geom_smooth(se = F) + ylim(c(0,2))
#
# ggplot(IPD_var, aes(x = lead_time, y = sd, group = model_idx, color = model_idx)) +
#   geom_point()+
#   geom_smooth(se = F)




forecast_out_file = '4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[FALSE]_driver[FALSE]_init[TRUE].nc'

da_out = readRDS('4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[FALSE]_driver[FALSE]_init[TRUE].rds')
all_dates = da_out$dates
cur_model_idxs = da_out$model_locations$model_idx
temp_adj = da_out$Y[1:42,,] %>%
  reshape2::melt(varnames = c('model_idx', 'issue_time', 'ensemble')) %>%
  mutate(issue_time = all_dates[issue_time],
         model_idx = cur_model_idxs[model_idx],
         valid_time = issue_time,
         ensemble = ensemble) %>%
  rename(seg_tave_water = value) %>%
  as_tibble()

# I = initial condition error, D = Driver error, P = parameter error
I = nc_forecast_get(nc_file = forecast_out_file, var_name = 'seg_tave_water', issue_dates = out_dates)
I_var = I %>%
  left_join(temp_adj, by =c('model_idx','valid_time','issue_time', 'ensemble'), suffix = c('','_adj')) %>%
  # adding in DA adjustment
  mutate(seg_tave_water = ifelse(!is.na(seg_tave_water_adj), seg_tave_water_adj, seg_tave_water)) %>%
  select(-seg_tave_water_adj) %>%
  group_by(model_idx, issue_time, valid_time) %>%
  summarise(variance = var(seg_tave_water),
         sd = sd(seg_tave_water),
         cv = sd / mean(seg_tave_water)) %>%
  ungroup() %>%
  mutate(lead_time = as.integer(valid_time - issue_time),
         error = 'I') %>%
  left_join(select(temp_obs, model_idx, date, temp_C), by = c('model_idx' = 'model_idx', 'valid_time' = 'date'))



forecast_out_file = '4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[TRUE]_driver[FALSE]_init[TRUE].nc'

da_out = readRDS('4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[TRUE]_driver[FALSE]_init[TRUE].rds')
all_dates = da_out$dates
cur_model_idxs = da_out$model_locations$model_idx
temp_adj = da_out$Y[1:42,,] %>%
  reshape2::melt(varnames = c('model_idx', 'issue_time', 'ensemble')) %>%
  mutate(issue_time = all_dates[issue_time],
         model_idx = cur_model_idxs[model_idx],
         valid_time = issue_time,
         ensemble = ensemble) %>%
  rename(seg_tave_water = value) %>%
  as_tibble()

# I = initial condition error, D = Driver error, P = parameter error
IP = nc_forecast_get(nc_file = forecast_out_file, var_name = 'seg_tave_water', issue_dates = out_dates)
IP_var = IP %>%
  left_join(temp_adj, by =c('model_idx','valid_time','issue_time', 'ensemble'), suffix = c('','_adj')) %>%
  # adding in DA adjustment
  mutate(seg_tave_water = ifelse(!is.na(seg_tave_water_adj), seg_tave_water_adj, seg_tave_water)) %>%
  select(-seg_tave_water_adj) %>%
  group_by(model_idx, issue_time, valid_time) %>%
  summarise(variance = var(seg_tave_water),
            sd = sd(seg_tave_water),
            cv = sd / mean(seg_tave_water)) %>%
  ungroup() %>%
  mutate(lead_time = as.integer(valid_time - issue_time),
         error = 'IP') %>%
  left_join(select(temp_obs, model_idx, date, temp_C), by = c('model_idx' = 'model_idx', 'valid_time' = 'date'))


# process error
forecast_out_file = '4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[TRUE]_driver[FALSE]_init[TRUE]_process[TRUE].nc'

da_out = readRDS('4_model_forecast/out/DRB_DA_SNTemp_20201023_2019-03-01_to_2019-09-01_8fdays_param[TRUE]_driver[FALSE]_init[TRUE]_process[TRUE].rds.ind')
all_dates = da_out$dates
cur_model_idxs = da_out$model_locations$model_idx
temp_adj = da_out$Y[1:42,,] %>%
  reshape2::melt(varnames = c('model_idx', 'issue_time', 'ensemble')) %>%
  mutate(issue_time = all_dates[issue_time],
         model_idx = cur_model_idxs[model_idx],
         valid_time = issue_time,
         ensemble = ensemble) %>%
  rename(seg_tave_water = value) %>%
  as_tibble()

# I = initial condition error, D = Driver error, P = parameter error; Q = process error
IPQ = nc_forecast_get(nc_file = forecast_out_file, var_name = 'seg_tave_water', issue_dates = out_dates)
IPQ_var = IPQ %>%
  left_join(temp_adj, by =c('model_idx','valid_time','issue_time', 'ensemble'), suffix = c('','_adj')) %>%
  # adding in DA adjustment
  mutate(seg_tave_water = ifelse(!is.na(seg_tave_water_adj), seg_tave_water_adj, seg_tave_water)) %>%
  select(-seg_tave_water_adj) %>%
  group_by(model_idx, issue_time, valid_time) %>%
  summarise(variance = var(seg_tave_water),
            sd = sd(seg_tave_water),
            cv = sd / mean(seg_tave_water)) %>%
  ungroup() %>%
  mutate(lead_time = as.integer(valid_time - issue_time),
         error = 'IPQ') %>%
  left_join(select(temp_obs, model_idx, date, temp_C), by = c('model_idx' = 'model_idx', 'valid_time' = 'date'))



# variance partitioning matrix
var_par = bind_rows(I_var, IP_var, IPQ_var, IPD_var) %>%
  group_by(model_idx, valid_time, issue_time, lead_time) %>%
  mutate(max_var = max(variance),
         rel_var = variance / max_var) %>%
  ungroup() %>% dplyr::filter(valid_time < as.Date('2019-09-01'))

var_par_all = var_par %>%
  pivot_wider(id_cols = c(1,2,3,4,7,8), names_from = error, values_from = variance) %>%
  group_by(model_idx, valid_time, issue_time, lead_time) %>%
  mutate(max_var_Q = max(I, IP, IPQ, na.rm = T),
         max_var_D = max(I, IP, IPD, na.rm = T),
         max_var_QD = max(max_var_Q, max_var_D, na.rm = T)) %>%
  ungroup() %>%
  mutate(D = max_var_D - IP, # variance of just drivers
         IPQD = IPQ + D, # total variance of all four errors
         IPQD_var = IPQD,
         I = I / IPQD,
         PQD = 1 - I,  # proportion of varaince attributable to param, process, and driver error combined
         IP = PQD * (IP / IPQD),
         QD = 1 - I - IP, # proportion of variance to process and driver error
         IPQ = QD * (IPQ / IPQD), # proportion of variance attributable to process error
         IPQD = 1 - I - IP - IPQ,
         month = lubridate::month(valid_time, label = T)) %>%
  select(model_idx, issue_time, valid_time, lead_time, I, IP, IPQ, IPQD, month, IPQD_var)

var_par_sum = var_par_all %>%
  group_by(lead_time, month) %>%
  summarise(I = mean(I, na.rm = T),
            IP = mean(IP, na.rm = T),
            IPQ = mean(IPQ, na.rm = T),
            IPQD = mean(IPQD, na.rm = T)) %>%
  ungroup()

ggplot(data = var_par_sum, aes(x = month, y = IP, group = lead_time, color = lead_time))+
  geom_point()+
  geom_line() +
  theme_bw() +
  ylab('Proportion of Forecast Variance') +
  xlab('Month')

mean_sd = var_par_all %>% mutate(sd = sqrt(IPQD_var)) %>%
  group_by(lead_time, month) %>%
  summarise(sd = median(sd, na.rm = T)) %>%
  ungroup() %>% mutate(sd = sd + lead_time * rnorm(1,0,.1))


var_par_sum_long = var_par_sum %>%
  pivot_longer(cols = c('I', 'IP','IPQ', 'IPQD'), names_to = 'error', values_to = 'rel_var') %>%
  left_join(mean_sd, by = c('month', 'lead_time'))

lead_time_names = as_labeller(c(`0` = 'Nowcast',
                                `1` = '1 Day',
                                `2` = '2 Days',
                                `3` = '3 Days',
                                `4` = '4 Day',
                                `5` = '5 Days',
                                `6` = '6 Days',
                                `7` = '7 Days'))


# uncertainty

scaler = .2
windows(width = 12, height = 10)
ggplot(var_par_sum_long, aes(x = month, y = rel_var, fill = error))+
  geom_bar(position="fill", stat="identity", color = 'white', alpha = .8) +
  geom_point(data = var_par_sum_long, aes(x = month, y = cv/scaler)) +
  scale_y_continuous(
    # Features of the first axis
    name = "Proportion of Forecast Variance",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*scaler, name="Forecast Coefficient of Varation")
  ) +
  theme_bw() +
  # ylab('Proportion of Forecast Variance') +
  xlab('Month') +
  facet_wrap(~lead_time, labeller = lead_time_names) +
  labs(fill = 'Error Type') +
  theme(legend.position = c(.9,.15)) +
  scale_fill_discrete(name = 'Error Type',
                      labels = c('Initial Conditions', 'Parameters', 'Process', 'Drivers'))


windows(width = 12, height = 10)
ggplot(var_par_sum_long %>% group_by(error, lead_time) %>% summarise(rel_var = mean(rel_var)), aes(x = lead_time, y = rel_var, fill = error))+
  geom_area(position="fill", stat="identity", color = 'white', alpha = .8) +
  ylab("Proportion of Forecast Variance") +
  theme_bw() +
  xlab('Lead Time (days)') +
  # facet_wrap(~month) +
  labs(fill = 'Error Type') +
  # theme(legend.position = c(.9,.15)) +
  scale_fill_manual(name = 'Error Type',
                    labels = c('Initial Conditions', 'Parameters', 'Process', 'Drivers'),
                    values = c('light green', 'orange', 'grey', 'light blue')) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

windows(width = 12, height = 10)
ggplot(var_par_sum_long, aes(x = lead_time, y = rel_var, fill = error))+
  geom_area(position="fill", stat="identity", color = 'white', alpha = .8) +
  ylab("Proportion of Forecast Variance") +
  theme_bw() +
  xlab('Lead Time (days)') +
  facet_wrap(~month) +
  labs(fill = 'Error Type') +
  # theme(legend.position = c(.9,.15)) +
  scale_fill_manual(name = 'Error Type',
                    labels = c('Initial Conditions', 'Parameters', 'Process', 'Drivers'),
                    values = c('light green', 'orange', 'grey', 'light blue')) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

windows(width = 12, height = 10)
ggplot(var_par_sum_long %>% group_by(lead_time) %>% summarise(sd = mean(sd)), aes(x = lead_time, y = sd))+
  geom_line(color = 'black', size = 3) +
  ylab("Total Standard Deviation") +
  theme_bw() +
  xlab('Lead Time (days)') +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

ggplot(var_par_sum_long, aes(x = lead_time, y = sd))+
  geom_line(color = 'black', alpha = .8) +
  ylab("Proportion of Forecast Variance") +
  theme_bw() +
  xlab('Lead Time (days)') +
  facet_wrap(~month) +
  labs(fill = 'Error Type') +
  # theme(legend.position = c(.9,.15)) +
  scale_fill_manual(name = 'Error Type',
                    labels = c('Initial Conditions', 'Parameters', 'Process', 'Drivers'),
                    values = c('light green', 'orange', 'grey', 'light blue')) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

# predictions

preds = left_join(I, select(temp_obs, -subseg_id, -seg_id_nat),
                  by = c('model_idx' = 'model_idx', 'valid_time' = 'date')) %>%
  mutate(lead_time = as.integer(valid_time - issue_time)) %>%
  group_by(model_idx, valid_time, lead_time) %>%
  summarise(temp_C = mean(temp_C, na.rm = T),
            seg_tave_water = mean(seg_tave_water)) %>%
  ungroup() %>% dplyr::filter(valid_time < as.Date('2019-09-01'))

preds_plot = dplyr::filter(preds, !model_idx == '416',
                           lead_time == 0)
preds_map = left_join(mutate(model_fabric, model_idx = as.character(model_idx)), preds_plot, by = 'model_idx') %>%
  dplyr::filter(model_idx %in% cur_model_idxs) %>%
  group_by(model_idx) %>%
  summarise(rmse = caret::RMSE(seg_tave_water, temp_C, na.rm = T)) %>%
  ungroup()

caret::RMSE(preds_plot$seg_tave_water, preds_plot$temp_C, na.rm = T)

windows(width = 12, height = 10)
ggplot(preds_plot, aes(x = temp_C, y = seg_tave_water, group = model_idx, color = model_idx)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  xlab('Observed Temperature (C)') +
  ylab('Predicted Temperatutre (C)') +
  facet_wrap(~model_idx) +
  geom_smooth(method = 'lm', se = F) + theme(legend.position = 'none')

windows(width = 12, height = 10)
ggplot() +
  geom_sf(data =preds_map, color = 'grey') +
  geom_sf(data = dplyr::filter(preds_map, !is.na(rmse)), aes(color = rmse), size = 2) +
  theme_bw()


preds_plot_2 = preds %>% mutate(month = lubridate::month(valid_time, label = T)) %>%
  dplyr::filter(!model_idx == '416') %>%
  group_by(lead_time, month) %>%
  summarise(rmse = caret::RMSE(seg_tave_water, temp_C, na.rm = T)) %>%
  ungroup()

windows(width = 12, height = 10)
ggplot(preds_plot_2, aes(x = lead_time, y = rmse, color = month, group = month)) +
  geom_point(size = 4) +
  theme_bw() +
  xlab('Lead Time (Days)') +
  ylab('RMSE (C)') +
  theme_bw() +
  xlab('Lead Time (days)') +
  scale_color_discrete(name = 'Month') +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))


thres = preds %>%
  mutate(thres = 23.88,
         pred_exceed = seg_tave_water > thres,
         obs_exceed = temp_C > thres,
         true_pos = pred_exceed & obs_exceed,
         false_pos = pred_exceed & !obs_exceed,
         true_neg = !pred_exceed & !obs_exceed,
         false_neg = !pred_exceed & obs_exceed)

hss = thres %>% mutate(month = lubridate::month(valid_time, label = T)) %>%
  group_by(month, lead_time) %>%
  summarise(n_exceed = sum(obs_exceed, na.rm = T),
            true_pos = sum(true_pos, na.rm = T),
            false_pos = sum(false_pos, na.rm = T),
            true_neg = sum(true_neg, na.rm = T),
            false_neg = sum(false_neg, na.rm = T)) %>%
  ungroup() %>%
  mutate(a = true_pos,
         b = false_pos,
         c = false_neg,
         d = true_neg,
         HSS = 2 * (a*d - b*c) / ( (a + c)*(c + d) + (a + b)*(b + d))) %>%
  select(-a, -b, -c, -d)

ggplot(hss, aes(x = lead_time, y = HSS, group = month, color = month)) +
  geom_point()

hss_long = hss %>%
  pivot_longer(cols = c(4,5,7), names_to = 'pred_score', values_to = 'count') %>%
  group_by(month, pred_score, lead_time) %>%
  summarise(count = sum(count)) %>%
  ungroup()

ggplot(hss_long, aes(x = month, y = count, fill = pred_score)) +
  geom_bar(position="fill", stat="identity", color = 'white', alpha = .8) +
  facet_wrap(~lead_time)

windows()
ggplot(hss_long %>% group_by(lead_time, pred_score) %>% summarise(count = sum(count)), aes(x = lead_time, y = count, fill = pred_score)) +
  geom_area(stat = 'identity', color = 'white', alpha = .8) +
  ylab("Count") +
  theme_bw() +
  xlab('Lead Time (days)') +
  # facet_wrap(~month) +
  labs(fill = 'Error Type') +
  # theme(legend.position = c(.9,.15)) +
  scale_fill_manual(name = '',
                    labels = c('False Negative', 'False Positive', 'True Positive'),
                    values = c('light blue', 'pink', 'grey')) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))


ggplot(hss_long, aes(x = lead_time, y = count, fill = pred_score)) +
  geom_area(stat = 'identity', color = 'white', alpha = .8) +
  ylab("Count") +
  theme_bw() +
  xlab('Lead Time (days)') +
  facet_wrap(~month) +
  labs(fill = 'Error Type') +
  # theme(legend.position = c(.9,.15)) +
  scale_fill_manual(name = '',
                    labels = c('False Negative', 'False Positive', 'True Positive'),
                    values = c('light blue', 'pink', 'grey')) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))
