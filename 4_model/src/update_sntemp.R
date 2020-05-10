
#' update parameters
#'
update_sntemp_params = function(param_names, updated_params,
                                model_run_loc = '4_model/tmp',
                                param_file = 'input/myparam.param',
                                n_segments = 456){

  params = readLines(file.path(model_run_loc, param_file))

  for(i in 1:length(param_names)){
    param_loc_start = grep(param_names[i], params) + 5
    param_loc_end = param_loc_start + n_segments - 1

    params[param_loc_start:param_loc_end] = as.character(round(as.numeric(updated_params[((i-1)*n_segments+1):(i*n_segments)]), digits = 0))
  }

  writeLines(params, file.path(model_run_loc, param_file))
}


#' update states
#'
update_sntemp_states = function(state_names,
                                by_seg = T,
                                updated_states,
                                state_order_file = '4_model/cfg/state_order.rds',
                                model_run_loc = '4_model/tmp',
                                ic_file_in = 'prms_ic.txt',
                                ic_file_out = 'prms_ic.txt',
                                n_segments = 456){
  # order of the states in the ic file - PRMS-SNTemp ic file isn't documented so these are the order of the states AS LONG AS
  #  we use the same modules every time
  state_order = readRDS(state_order_file)

  # open the ic file
  fc = file(file.path(model_run_loc, ic_file_in))
  ic = strsplit(readLines(fc, skipNul = T), ' +') # reading in text with irregular white space seperators
  close(fc)

  # update the states
  if(by_seg){
    for(i in 1:length(state_names)){

      cur_state = state_names[i]
      cur_state_row = state_order$row_idx[state_order$state_name == cur_state]

      ic[[cur_state_row]] = as.character(updated_states[((i-1)*n_segments+1):(i*n_segments)])
    }
  }else{
    for(i in 1:length(state_names)){

      cur_state = state_names[i]
      cur_state_row = state_order$row_idx[state_order$state_name == cur_state]

      ic[[cur_state_row]] = as.character(updated_states) # quick fix for sensativity testing
    }
  }

  out = lapply(X = 1:length(ic), FUN = function(x){paste(ic[[x]], collapse = ' ')})

  # have to delete old ic file before writing new one because of appending thing that I do below
  file.remove(file.path(model_run_loc, ic_file_out))

  # write out new ic file
  lapply(out, write, file.path(model_run_loc, ic_file_out), append = T)
}

