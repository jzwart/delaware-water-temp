
get_sntemp_discharge = function(model_output_file, model_fabric_file){

  model_output = read.csv(model_output_file, header = T, stringsAsFactors = F)

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx))

  model_output = model_output %>% as_tibble() %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key = 'model_idx', value = 'discharge', starts_with('X')) %>%
    mutate(model_idx = gsub('X', '', model_idx)) %>%
    rename(date = Date) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    select(seg_id_nat, model_idx, date, discharge) %>%
    arrange(as.numeric(model_idx))

  return(model_output)
}

get_sntemp_temperature = function(model_output_file, model_fabric_file){

  model_output = read.csv(model_output_file, header = T, stringsAsFactors = F)

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx))

  model_output = model_output %>% as_tibble() %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key = 'model_idx', value = 'water_temp', starts_with('X')) %>%
    mutate(model_idx = gsub('X', '', model_idx)) %>%
    rename(date = Date) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    select(seg_id_nat, model_idx, date, water_temp) %>%
    arrange(as.numeric(model_idx))

  return(model_output)
}

get_sntemp_intermediates = function(model_output_file, model_fabric_file){

  model_output = read.table(model_output_file, header = T, stringsAsFactors = F) %>%
    dplyr::slice(-1) # first row indicates column type

  # model_otuput = read.csv(model_output_file,sep = ' ', skip = 20, header = T, stringsAsFactors = F)
  #
  # fc = file(file.path(model_output_file))
  # ic = strsplit(readLines(fc, skipNul = T), ' +') # reading in text with irregular white space seperators
  # close(fc)

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx))

  model_output = model_output %>% as_tibble() %>%
    mutate(timestamp = as.Date(timestamp),
           nsegment = as.character(nsegment)) %>%
    rename(date = timestamp,
           model_idx = nsegment) %>%
    gather(key = 'parameter', value = 'parameter_value', starts_with('seg')) %>%
    mutate(parameter_value = as.numeric(parameter_value)) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    select(seg_id_nat, model_idx, date, parameter, parameter_value) %>%
    arrange(as.numeric(model_idx))

  return(model_output)
}


get_sntemp_initial_states = function(state_names,
                                     by_seg = T,
                                     model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp',
                                     state_order_file = '4_model/cfg/state_order.rds',
                                     model_run_loc = '4_model/tmp',
                                     ic_file = 'prms_ic.txt'){
  # order of the states in the ic file - PRMS-SNTemp ic file isn't documented so these are the order of the states AS LONG AS
  #  we use the same modules every time
  state_order = readRDS(state_order_file)

  # open the ic file
  fc = file(file.path(model_run_loc, ic_file))
  ic = strsplit(readLines(fc, skipNul = T), ' +') # reading in text with irregular white space seperators
  close(fc)

  if(by_seg){ #states are per segment
    model_fabric = sf::read_sf(model_fabric_file)

    seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx)) %>%
      arrange(as.numeric(model_idx))

    out = seg_ids

    for(i in 1:length(state_names)){

      cur_state = state_names[i]
      cur_state_row = state_order$row_idx[state_order$state_name == cur_state]

      cur_state_vals = na.omit(as.numeric(ic[[cur_state_row]]))

      out = out %>%
        mutate(temp_name = cur_state_vals) %>%
        rename(!!noquote(cur_state) := temp_name)
    }
  }else{
    for(i in 1:length(state_names)){

      cur_state = state_names[i]
      cur_state_row = state_order$row_idx[state_order$state_name == cur_state]

      cur_state_vals = na.omit(as.numeric(ic[[cur_state_row]]))

      out = tibble(!!noquote(cur_state) := cur_state_vals) # temporary fix for sesnativity testing

      # out = out %>%
      #   mutate(temp_name = cur_state_vals) %>%
      #   rename(!!noquote(cur_state) := temp_name)
    }
  }

  return(out)
}



