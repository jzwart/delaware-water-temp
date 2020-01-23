


library(ggplot2)

d100 = feather::read_feather('4_model_for_PGDL/out/drb_full_real_train_100obs.feather') %>% group_by(date) %>%
  summarise(sum = sum(exp1 == 'train')) %>% mutate(obs = rep('100', nrow(.)))
d50 = feather::read_feather('4_model_for_PGDL/out/drb_full_real_train_50obs.feather') %>% group_by(date) %>%
  summarise(sum = sum(exp1 == 'train')) %>% mutate(obs = rep('50', nrow(.)))
d10 = feather::read_feather('4_model_for_PGDL/out/drb_full_real_train_10obs.feather') %>% group_by(date) %>%
  summarise(sum = sum(exp1 == 'train')) %>% mutate(obs = rep('10', nrow(.)))
d2 = feather::read_feather('4_model_for_PGDL/out/drb_full_real_train_2obs.feather') %>% group_by(date) %>%
  summarise(sum = sum(exp1 == 'train')) %>% mutate(obs = rep('2', nrow(.)))


all = bind_rows(d100, d50, d10, d2)


ggplot(all, aes(x = date, y = sum, group = obs, color = obs)) +
  geom_point() +
  theme_classic() +
  xlab('Date') + ylab('n_obs Training / day') + theme(axis.text = element_text(size = 14))


d100 = feather::read_feather('4_model_for_PGDL/out/drb_subset_real_train_100obs.feather')
d50 = feather::read_feather('4_model_for_PGDL/out/drb_subset_real_train_50obs.feather')
d10 = feather::read_feather('4_model_for_PGDL/out/drb_subset_real_train_10obs.feather')
d2 = feather::read_feather('4_model_for_PGDL/out/drb_subset_real_train_2obs.feather')

unique(d100$seg_id_nat[d100$exp1=='train'])
unique(d50$seg_id_nat[d50$exp1=='train'])
unique(d10$seg_id_nat[d10$exp1=='train'])
unique(d2$seg_id_nat[d2$exp1=='train'])

length(d100$seg_id_nat[d100$exp1=='train'])
length(d50$seg_id_nat[d50$exp1=='train'])
length(d10$seg_id_nat[d10$exp1=='train'])
length(d2$seg_id_nat[d2$exp1=='train'])


d100 = feather::read_feather('4_model_for_PGDL/out/drb_full_real_train_100.feather') %>% group_by(date) %>%
  summarise(sum = sum(exp1 == 'train')) %>% mutate(perc = rep('100', nrow(.)))
d50 = feather::read_feather('4_model_for_PGDL/out/drb_full_real_train_50.feather') %>% group_by(date) %>%
  summarise(sum = sum(exp1 == 'train')) %>% mutate(perc = rep('50', nrow(.)))
d20 = feather::read_feather('4_model_for_PGDL/out/drb_full_real_train_20.feather') %>% group_by(date) %>%
  summarise(sum = sum(exp1 == 'train')) %>% mutate(perc = rep('20', nrow(.)))
d10 = feather::read_feather('4_model_for_PGDL/out/drb_full_real_train_10.feather') %>% group_by(date) %>%
  summarise(sum = sum(exp1 == 'train')) %>% mutate(perc = rep('10', nrow(.)))
d02 = feather::read_feather('4_model_for_PGDL/out/drb_full_real_train_02.feather') %>% group_by(date) %>%
  summarise(sum = sum(exp1 == 'train')) %>% mutate(perc = rep('02', nrow(.)))

all = bind_rows(d100, d50, d20, d10, d02)


ggplot(all, aes(x = date, y = sum, group = perc, color = perc)) +
  geom_line() +
  theme_classic() +
  xlab('Date') + ylab('n_obs Training / day') + theme(axis.text = element_text(size = 14))


out = d100 %>% group_by(seg_id_nat) %>%
  summarise(sum = as.numeric(n())) %>% ungroup()

out = out[order(out$sum), ]
out$cumsum = cumsum(out$sum)
out$fracObs = out$cumsum / out$cumsum[nrow(out)]




sntemp = feather::read_feather('4_model_for_PGDL/out/sntemp_input_output.feather')
obs = readRDS('3_observations/in/obs_temp_full.rds')

all = left_join(sntemp, obs, by = c('seg_id_nat', 'date'))

sqrt(mean((all$seg_tave_water - all$temp_C)^2, na.rm = T))
