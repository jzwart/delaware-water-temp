
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

get_sntemp_intermediates = function(model_output_file,
                                    model_fabric_file,
                                    sntemp_vars){

  ### This was given error with updated model output and reading in the data
  # error was:
  #  Error in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
  #       line 1489316 did not have 20 elements
  # even though all rows had 20 elements from manual inspection of R-indicated problem rows
  # model_output = read.table(model_output_file, header = T, stringsAsFactors = F) %>%
  #    dplyr::slice(-1) # first row indicates column type

  # using fread to solve error documented above ^
  to_skip = length(sntemp_vars) + 6 # how many lines to skip when reading in (based on how many vars are output)
  model_output = data.table::fread(file = model_output_file,
                                   skip = to_skip, header = F)
  # head(model_output)

  cols = readr::read_delim(file = model_output_file, delim = '\t', n_max = 2, skip = to_skip)

  colnames(model_output) = colnames(cols)
  rm(cols)
  # model_otuput = read.csv(model_output_file,sep = ' ', skip = 20, header = T, stringsAsFactors = F)
  #
  # fc = file(file.path(model_output_file))
  # ic = strsplit(readLines(fc, skipNul = T), ' +') # reading in text with irregular white space seperators
  # close(fc)

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                   model_idx = as.character(model_fabric$model_idx))

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


# function for retrieving parameters from param file
get_sntemp_params = function(param_names,
                             model_run_loc,
                             model_fabric_file = 'GIS/Segments_subset.shp',
                             param_file = 'input/myparam.param',
                             param_default_file = 'control/delaware.control.par_name',
                             n_segments = 456){

  params = readLines(file.path(model_run_loc, param_file))

  model_fabric = sf::read_sf(file.path(model_run_loc, model_fabric_file))

  # order by model_idx
  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  out = vector(mode = 'list', length = length(param_names))

  if(length(param_names) == 0){
    out = out
  }else{
    for(i in seq_along(param_names)){

      defaults = get_default_param_vals(param_name = param_names[i],
                                        model_run_loc = model_run_loc,
                                        param_default_file = param_default_file)

      param_loc_start = which(params == param_names[i]) + 4 + as.numeric(defaults$ndim)
      param_loc_end = param_loc_start + as.numeric(defaults$size) - 1

      cur_param_vals = params[param_loc_start:param_loc_end]

      out[[i]] = cur_param_vals
      names(out)[i] = param_names[i]
    }
  }

  return(out)
}


# function for retrieving jh_coef from param file
get_jh_coef = function(model_run_loc,
                       model_fabric_file = 'GIS/Segments_subset.shp',
                       model_hrus_file = 'GIS/HRU_subset.shp',
                       param_file = 'input/myparam.param',
                       n_hrus = 765,
                       n_months = 12){

  params = readLines(file.path(model_run_loc, param_file))

  model_fabric = sf::read_sf(file.path(model_run_loc, model_fabric_file))

  model_hrus = sf::read_sf(file.path(model_run_loc, model_hrus_file))

  # order by model_idx
  hru_ids = tibble(hru_id_nat = as.character(model_hrus$hru_id_nat), model_idx = as.character(model_hrus$model_idx)) %>%
    arrange(as.numeric(model_idx))

  months = as.character(seq(1, n_months))

  out = expand_grid(model_idx = hru_ids$model_idx, month = months) %>%
    left_join(hru_ids, by = 'model_idx') %>%
    arrange(as.numeric(month), as.numeric(model_idx)) %>%
    select(hru_id_nat, model_idx, month)

  param_loc_start = which(params == 'jh_coef') + 6
  param_loc_end = param_loc_start + n_hrus * n_months - 1

  # jh_coef is a per HRU x month basis. In the parameter file (myparam.param), jh_coef
  #  is organized in order of model_idx HRU and then month - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 764_Dec, 765_Dec
  cur_param_vals = params[param_loc_start:param_loc_end]

  out = mutate(out, jh_coef = cur_param_vals)

  return(out)
}



# function for retrieving jh_coef from param file
get_lat_temp_adj = function(model_run_loc,
                            model_fabric_file = 'GIS/Segments_subset.shp',
                            param_file = 'input/myparam.param',
                            n_segments = 456,
                            n_months = 12){

  params = readLines(file.path(model_run_loc, param_file))

  model_fabric = sf::read_sf(file.path(model_run_loc, model_fabric_file))

  # order by model_idx
  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  months = as.character(seq(1, n_months))

  out = expand_grid(model_idx = seg_ids$model_idx, month = months) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    arrange(as.numeric(month), as.numeric(model_idx)) %>%
    select(seg_id_nat, model_idx, month)

  param_loc_start = which(params == 'lat_temp_adj') + 6
  param_loc_end = param_loc_start + n_segments * n_months - 1

  # lat_temp_adj is a per segment x month basis. In the parameter file (myparam.param), lat_temp_adj
  #  is organized in order of segment model_idx and then month - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 455_Dec, 456_Dec
  cur_param_vals = params[param_loc_start:param_loc_end]

  out = mutate(out, lat_temp_adj = cur_param_vals)

  return(out)
}
