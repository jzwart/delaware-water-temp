

compile_rgcn_temp_output = function(ind_file,
                                    output_dir,
                                    gd_config = 'lib/cfg/gd_config.yml'){

  files = list.files(output_dir)

  # notes from Xiaowei #
  # I have created the outputs of several methods across 42 segments in the subbasin I previously worked on. I am wondering if you could help visualize the prediction error across different segments so that we can get the error distribution from a spatial perspective. This may help get a better sense of which segments tend to have larger/smaller errors and how our methods improve from traditional RNN methods.
  #
  # I have attached six output files. Each of them is a 42-4383 matrix, which represents the output temperature/streamflow in the testing period. The files ended with '_flow' is for streamflow and otherwise for temperature predictions. The testing period is from 2004-10-01 to 2016-09-30 (4383 dates). 'RNN' represents the standard RNN, 'RGCN_woptr' is the RGCN model without pretraining, 'RGCN_wctr' represents the RGCN model with pretraining and contrastive loss. The segment ids of the 42 segments are saved in 'seg_ids.npy'.

  np = reticulate::import('numpy')

  date = seq.Date(from = as.Date('2004-10-01'), to = as.Date('2016-09-30'), by = 'day')

  seg_ids = np$load(file.path(output_dir, 'seg_ids.npy'))

  out_files = files[-grep('seg_ids.npy', files)]
  temp_files = out_files[-grep('flow', out_files)]

  compiled_rgcn_temp = tibble()
  for(i in 1:length(temp_files)){
    tmp = as_tibble(t(np$load(file.path(output_dir, temp_files[i]))))

    colnames(tmp) = seg_ids

    tmp$date = date

    tmp = tmp %>%
      pivot_longer(cols = -date,
                   names_to = 'seg_id_nat',
                   values_to = 'stream_temp_C') %>%
      mutate(model = strsplit(temp_files[i], '.npy')[[1]]) %>%
      arrange(seg_id_nat, date) %>% select(seg_id_nat, date, stream_temp_C, model)

    compiled_rgcn_temp = bind_rows(compiled_rgcn_temp, tmp)
  }

  out_file = as_data_file(ind_file)
  feather::write_feather(x = compiled_rgcn_temp, path = out_file)
  gd_put(remote_ind = ind_file, local_source = out_file, config_file = gd_config)
}






compile_rgcn_flow_output = function(ind_file,
                                    output_dir,
                                    gd_config = 'lib/cfg/gd_config.yml'){

  files = list.files(output_dir)

  # notes from Xiaowei #
  # I have created the outputs of several methods across 42 segments in the subbasin I previously worked on. I am wondering if you could help visualize the prediction error across different segments so that we can get the error distribution from a spatial perspective. This may help get a better sense of which segments tend to have larger/smaller errors and how our methods improve from traditional RNN methods.
  #
  # I have attached six output files. Each of them is a 42-4383 matrix, which represents the output temperature/streamflow in the testing period. The files ended with '_flow' is for streamflow and otherwise for temperature predictions. The testing period is from 2004-10-01 to 2016-09-30 (4383 dates). 'RNN' represents the standard RNN, 'RGCN_woptr' is the RGCN model without pretraining, 'RGCN_wctr' represents the RGCN model with pretraining and contrastive loss. The segment ids of the 42 segments are saved in 'seg_ids.npy'.

  np = reticulate::import('numpy')

  date = seq.Date(from = as.Date('2004-10-01'), to = as.Date('2016-09-30'), by = 'day')

  seg_ids = np$load(file.path(output_dir, 'seg_ids.npy'))

  out_files = files[-grep('seg_ids.npy', files)]
  flow_files = out_files[grep('flow', out_files)]

  compiled_rgcn_flow = tibble()
  for(i in 1:length(flow_files)){
    tmp = as_tibble(t(np$load(file.path(output_dir, flow_files[i]))))

    colnames(tmp) = seg_ids

    tmp$date = date

    tmp = tmp %>%
      pivot_longer(cols = -date,
                   names_to = 'seg_id_nat',
                   values_to = 'stream_flow_m3_s') %>%
      mutate(model = strsplit(flow_files[i], '.npy')[[1]]) %>%
      arrange(seg_id_nat, date) %>% select(seg_id_nat, date, stream_flow_m3_s, model)

    compiled_rgcn_flow = bind_rows(compiled_rgcn_flow, tmp)
  }

  out_file = as_data_file(ind_file)
  feather::write_feather(x = compiled_rgcn_flow, path = out_file)
  gd_put(remote_ind = ind_file, local_source = out_file, config_file = gd_config)
}


