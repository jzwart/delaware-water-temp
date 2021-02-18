



d_100_6 = feather::read_feather('4_model_for_PGDL/out/uncal_sntemp_input_output_subset_100[gw_tau]_6[ss_tau].feather')
d_45_6 = feather::read_feather('4_model_for_PGDL/out/uncal_sntemp_input_output_subset_45[gw_tau]_6[ss_tau].feather')
d_10_6 = feather::read_feather('4_model_for_PGDL/out/uncal_sntemp_input_output_subset_10[gw_tau]_6[ss_tau].feather')
d_45_1 = feather::read_feather('4_model_for_PGDL/out/uncal_sntemp_input_output_subset_45[gw_tau]_1[ss_tau].feather')
d_45_20 = feather::read_feather('4_model_for_PGDL/out/uncal_sntemp_input_output_subset_45[gw_tau]_20[ss_tau].feather')



hist(d_10_6$seg_tave_water)
hist(d_100_6$seg_tave_water, add= T, col = 'red')
hist(d_45_6$seg_tave_water, add = T, col = 'blue')
hist(d_45_1$seg_tave_water)
hist(d_45_20$seg_tave_water, add = T, col = 'red')

summary(d_10_6$seg_tave_water)


gw_flow = d_100_6 %>%
  mutate(month = lubridate::month(date)) %>%
  group_by(month) %>%
  summarise(mean_gw_flow = mean(seginc_gwflow)) %>% ungroup()

plot(gw_flow, pch = 16, cex =2)



source('/Users/jzwart/Documents/Jake/Database/R Code/dbTable.R')
source('/Users/jzwart/Documents/Jake/Database/R Code/dbTableList.R')
source('/Users/jzwart/Documents/Jake/Database/R Code/sensordbTable.R')

color = dbTable('COLOR', depthClass = c('PML','Hypo'), lakeID = c('CR')) %>% as_tibble() %>%
  dplyr::filter(flag == 0) %>%
  pivot_wider(id_cols = c(lakeID,siteName,dateTimeSample, depthClass,g440) , names_from = depthClass, values_from = g440) %>%
  mutate(diff = abs(PML - Hypo))


plot(color$diff,ylim = c(-10,10))






