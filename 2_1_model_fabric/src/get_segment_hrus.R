


# returns hru model_idxs associated with the segment model_idxs supplied
get_segment_hrus = function(seg_model_idxs,
                            model_run_loc,
                            param_file = 'input/myparam.param'){

  params = readLines(file.path(model_run_loc, param_file))

  hru_to_seg = get_sntemp_params(param_names = 'hru_segment',
                                 model_run_loc = model_run_loc,
                                 param_file = param_file)$hru_segment

  out = tibble(hru_model_idxs = as.character(which(hru_to_seg %in% seg_model_idxs)),
               seg_model_idxs = as.character(hru_to_seg[which(hru_to_seg %in% seg_model_idxs)]))
  return(out)
}
