library(tidyverse)


get_segment_hrus(seg_id_nat = c('1573', '1571', '1450', '1565', '1641'),
                 model_run_loc = '4_model/tmp')



# returns hru model_idxs associated with the segment model_idxs supplied
get_segment_hrus = function(seg_id_nat, # seg_id_nat
                            model_run_loc,
                            param_file = 'input/myparam.param',
                            segment_shape_file = 'GIS/Segments_subset.shp',
                            hru_shape_file = 'GIS/HRU_subset.shp'){

  params = readLines(file.path(model_run_loc, param_file))

  hru_to_seg = get_sntemp_params(param_names = 'hru_segment',
                                 model_run_loc = model_run_loc,
                                 param_file = param_file)$hru_segment

  seg_fabric = sf::read_sf(file.path(model_run_loc, segment_shape_file))
  hru_fabric = sf::read_sf(file.path(model_run_loc, hru_shape_file))

  seg_ids = tibble(seg_id_nat = as.character(seg_fabric$seg_id_nat), model_idx = as.character(seg_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  cur_seg_id_nat = seg_id_nat

  cur_seg_model_idx = filter(seg_ids, seg_id_nat %in% cur_seg_id_nat) %>% pull(model_idx)

  hru_ids = tibble(hru_id_nat = as.character(hru_fabric$hru_id_nat), model_idx = as.character(hru_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  hru_id_nat_out = filter(hru_ids, model_idx %in% as.character(which(hru_to_seg %in% cur_seg_model_idx))) %>% pull(hru_id_nat)

  seg_id_nat_out = seg_ids$seg_id_nat[match(as.character(hru_to_seg[which(hru_to_seg %in% cur_seg_model_idx)]), table = seg_ids$model_idx)]

  out = tibble(hru_id_nat = hru_id_nat_out,
               seg_id_nat = seg_id_nat_out)
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


#### example of default parameter details in param_default_file; all params should be same structure ####
# Name      : K_coef
# Module    : routing
# Descr     : Muskingum storage coefficient
# Help      : Travel time of flood wave from one segment to the next downstream segment, called the Muskingum storage coefficient; enter 1.0 for reservoirs, diversions, and segment(s) flowing out of the basin
# Ndimen    : 1
# Dimensions: nsegment - 456
# Size      : 456
# Type      : float
# Units     : hours
# Width     : 0
# Max       : 24.000000
# Min       : 0.010000
# Default   : 1.000000

get_default_param_vals = function(param_name,
                                  model_run_loc,
                                  param_default_file){

  param_default = readLines(file.path(model_run_loc, param_default_file))

  param_default_loc = grep(paste('Name.*:.*', param_name,'?$'), param_default)

  ndim = param_default[param_default_loc + 4] %>% sub('.*: ', '', .)
  dim = param_default[param_default_loc + 5] %>% sub('.*: ', '', .)
  if(as.numeric(ndim) > 1){
    # I think there are only 2 dims max in PRMS-SNTemp, let's assume that for now
    first_dim = sub(' -.*', '', dim)
    second_dim = sub('.*, ', '', dim) %>% sub(' -.*', '', .)
    dim = paste(first_dim, second_dim, sep = '\n')
  }else{ # if only one dim, then extract everything to the left of '-'
    dim = sub(' -.*', '', dim)
  }
  size = param_default[param_default_loc + 6] %>% sub('.*: ', '', .)
  type = param_default[param_default_loc + 7] %>% sub('.*: ', '', .)
  if(type == 'float'){
    type = '2'
  }else if(type == 'long'){
    type = '1'
  }
  max = param_default[param_default_loc + 10] %>% sub('.*: ', '', .)
  min = param_default[param_default_loc + 11] %>% sub('.*: ', '', .)
  default = param_default[param_default_loc + 12] %>% sub('.*: ', '', .)

  vals = rep(default, as.numeric(size))

  out = list(param_name = param_name,
             ndim = ndim,
             dim = dim,
             size = size,
             type = type,
             max = max,
             min = min,
             vals = vals)
  return(out)
}
