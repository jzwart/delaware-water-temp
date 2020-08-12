
# checks if the parameter is in the parameter input file; if not, then it adds default values to parameter input file

add_default_sntemp_params = function(param_names,
                                     model_run_loc,
                                     param_file = 'input/myparam.param',
                                     param_default_file = 'control/delaware.control.par_name',
                                     n_segments = 456,
                                     n_hru = 756,
                                     n_months = 12){

  params = readLines(file.path(model_run_loc, param_file))

  param_default = readLines(file.path(model_run_loc, param_default_file))

  for(i in 1:length(param_names)){
    if(length(which(params == param_names[i])) == 0){ # checking if parameter not currently in param_file
      print(sprintf('%s was not in the parameter input file; adding default %s values to %s',
                    param_names[i], param_names[i], file.path(model_run_loc, param_file)))
      # if it isn't, then add append it to the the param_file with default values
      param_default_loc = grep(param_names[i], param_default)

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
      default = param_default[param_default_loc + 12] %>% sub('.*: ', '', .)

      vals = rep(default, as.numeric(size))

      cur_default = paste('####',
                          param_names[i],
                          ndim,
                          dim,
                          size,
                          type,
                          paste(vals, collapse = '\n'),
                          sep = '\n')
    }else{
      print(sprintf('%s is already in the parameter input file; leaving %s parameters as is',
                    param_names[i], param_names[i]))
    }
  }

  if(exists('cur_default')){
    params_out = c(params,
                   cur_default)
  }else{
    # all parameters in param_names
    params_out = params
  }

  writeLines(params_out, file.path(model_run_loc, param_file))
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

