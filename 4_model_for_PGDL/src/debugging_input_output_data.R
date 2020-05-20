

library(ggplot2)
library(tidyverse)
library(igraph)
source('4_model/src/get_upstream_downstream_segs.R')
# debugging the input data

sub_input = feather::read_feather('4_model_for_PGDL/out/sntemp_input_subset.feather')
sub_output = feather::read_feather('4_model_for_PGDL/out/sntemp_output_subset.feather')
diff_prcp = left_join(sub_input, sub_output, by = c('seg_id_nat', 'model_idx', 'date')) %>%
  dplyr::filter(seg_rain != seg_prcp, seg_rain < 0.05, (seg_prcp - seg_rain) > 0.001)


tmp = diff_prcp %>%
  group_by(seg_id_nat) %>%
  mutate(mean_diff = mean(seg_prcp - seg_rain)) %>%
  ungroup()


hist(tmp$mean_diff, xlab= 'mean difference (m)', main ='')

high_diff = tmp[which(tmp$mean_diff > 0.003),]
high_diff_segs = unique(high_diff$model_idx)
low_diff_segs = unique(tmp$model_idx)[!unique(tmp$model_idx) %in% high_diff_segs]

params = readLines('4_model_for_PGDL/tmp/input/myparam.param')
n_hru  = 765
# hru_segment is the parameter that tells which hru's map to which segments
param_loc_start = grep('hru_segment', params) + 5
param_loc_end = param_loc_start + n_hru - 1

hru_to_seg_vec = params[param_loc_start:param_loc_end]

# weight by HRU area (units are acres)
param_loc_start = grep('hru_area', params) + 5
param_loc_end = param_loc_start + n_hru - 1

hru_area_vec = params[param_loc_start:param_loc_end]

out = tibble()
for(seg in unique(tmp$model_idx)){
  hrus = which(hru_to_seg_vec %in% seg)
  cur = tibble(model_idx = seg, n_hrus = length(hrus))
  out = bind_rows(out, cur)
}

tmp = left_join(tmp, out)

tmp2 = tmp %>%
  group_by(model_idx) %>%
  summarise(mean_diff = mean(mean_diff),
            n_hrus = mean(n_hrus)) %>% ungroup()

ggplot(tmp2, aes(y = mean_diff * 1000, x= n_hrus, group = n_hrus)) +
  geom_boxplot() + ylab('mean diff (mm)') + xlab('contributing HRUs (n)')


#only have one HRU
for(seg in high_diff_segs){
  print(which(hru_to_seg_vec %in% seg))
}

# have 2 or more HRUs
for(seg in low_diff_segs){
  print(which(hru_to_seg_vec %in% seg))
}

windows()
ggplot(dplyr::filter(diff_prcp, model_idx %in%low_diff_segs), aes(x = seg_rain, y = seg_prcp)) +
  geom_point() +
  geom_point(data = dplyr::filter(diff_prcp, model_idx %in%high_diff_segs), aes(x = seg_rain, y = seg_prcp), color = 'red') +
  geom_abline(slope = 1, color = 'grey')

plot(diff_prcp$seg_rain, diff_prcp$seg_prcp, xlim = c(0,0.05), ylim = c(0,0.05))
plot(diff_prcp$seg_rain, diff_prcp$seg_prcp)
abline(0,1)


ggplot(diff_prcp, aes(x = (seg_prcp - seg_rain) * 1000))+
  geom_histogram(bins = 100)+
  xlim(c(0.001*1000, 0.05*1000)) +
  xlab('jake_calc - sntemp_calc (mm)')

sum((diff_prcp$seg_prcp - diff_prcp$seg_rain) > 0.001)
nrow(sub_input)
nrow(diff_prcp)


#





