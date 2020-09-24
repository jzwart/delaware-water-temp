

# function for writing control files for pestpp
write_pestpp_pst_files = function(params,
                                  seg_model_idxs,
                                  model_run_loc,
                                  temp_model_output_file,
                                  flow_model_output_file,
                                  obs,
                                  file_out,
                                  param_groups,
                                  tpl_file_name,
                                  param_file_name,
                                  param_default_file = 'control/delaware.control.par_name',
                                  temp_ins_file_name,
                                  flow_ins_file_name,
                                  calibrate_flow,
                                  calibrate_temp,
                                  weight_by_magnitude = T,
                                  tie_by_group = F){

  output = read.csv(file.path(model_run_loc, temp_model_output_file), header = T)
  model_idxs = seq(1,ncol(output)-1)
  colnames(output)[2:ncol(output)] = model_idxs
  dates = strftime(strptime(output$Date, format = '%Y-%m-%d'), '%Y%m%d') # date vector for

  cur_model_idxs = as.character(sort(as.numeric(seg_model_idxs)))

  if(calibrate_temp){
    obs$temp$obs_name = paste('wtemp',
                              obs$temp$model_idx,
                              strftime(strptime(obs$temp$date, format = '%Y-%m-%d'), '%Y%m%d'),
                              sep = '_')
  }
  if(calibrate_flow){
    obs$flow$obs_name = paste('flow',
                              obs$flow$model_idx,
                              strftime(strptime(obs$flow$date, format = '%Y-%m-%d'), '%Y%m%d'),
                              sep = '_')
  }

  #######################################################
  # format for minimalist .pst file :
  #
  # pcf
  # * control data
  # RSTFLE PESTMODE
  # NPAR NOBS NPARGP NPRIOR NOBSGP
  # NTPLFLE NINSFLE PRECIS DPOINT [NUMCOM]
  # RLAMBDA1 RLAMFAC PHIRATSUF PHIREDLAM NUMLAM
  # RELPARMAX FACPARMAX FACORIG
  # PHIREDSWH
  # NOPTMAX PHIREDSTP NPHISTP NPHINORED RELPARSTP NRELPAR
  # ICOV ICOR IEIG
  # * singular value decomposition
  # SVDMODE
  # MAXSING EIGTHRESH
  # EIGWRITE
  # * parameter groups
  # PARGPNME INCTYP DERINC DERINCLB FORCEN DERINCMUL DERMTHD
  # (one such line for each parameter group)
  # * parameter data
  # PARNME PARTRANS PARCHGLIM PARVAL1 PARLBND PARUBND PARGP SCALE OFFSET DERCOM
  # (one such line for each parameter)
  # PARNME PARTIED
  # (one such line for each tied parameter)
  # * observation groups
  # OBGNME
  # (one such line for each observation group)
  # * observation data
  # OBSNME OBSVAL WEIGHT OBGNME
  # (one such line for each observation)
  # * model command line
  # COMLINE
  # (one such line for each model command line)
  # * model input
  # TEMPFLE INFLE
  # (one such line for each template file)
  # * model output
  # INSFLE OUTFLE
  # (one such line for each instruction file)
  # * prior information
  # PILBL PIFAC * PARNME + PIFAC * log(PARNME) ... = PIVAL WEIGHT OBGNME
  # (one such line for each article of prior information)
  # * regularization
  # PHIMLIM  PHIMACCEPT [FRACPHIM]
  # WFINIT  WFMIN  WFMAX
  # WFFAC  WFTOL [IREGADJ]
  ####################################################

  # Jake went through the PEST++ manual on section 4.4 and decided which variables to set in the
  #  control data. We could make these variables an input to provide flexibility to function

  first_line = 'pcf' # pretty sure this will always stay the same

  param_names = names(params)
  npargp = length(param_names) # number of parameter groups
  npar = 0  # for counting the number of parameters to be calibrated
  for(i in seq_along(param_names)){
    npar = npar + nrow(params[[param_names[i]]])
  }

  control_data = paste('* control data',
                       'restart estimation',
                       sprintf('%s %s %s %s %s',
                               npar,
                               length(dates) * length(cur_model_idxs),
                               npargp,
                               '0',
                               '1'),
                       '1 1 single point',
                       '10.0 -2.0 0.3 0.01 10',
                       '10.0 10.0 0.001',
                       '0.1',
                       '50 0.005 4 4 0.005 4',
                       '0 0 0',
                       sep = '\n')

  # single value decomposition is not needed in control file unless you want to override default
  #single_val_decomp = paste('* single value decomposition',
  #                          sep = '\n')

  # change this to be input from yaml file (similar to range of values )
  if(!is.null(params)){
    param_groups_out = paste('* parameter groups',
                             sapply(seq_along(param_names), function(i){
                               out = sprintf('%s %s %s %s %s %s %s',
                                             param_names[i],
                                             param_groups$inctyp[param_groups$param == param_names[i]],
                                             param_groups$derinc[param_groups$param == param_names[i]],
                                             param_groups$derinclb[param_groups$param == param_names[i]],
                                             param_groups$forcen[param_groups$param == param_names[i]],
                                             param_groups$derincmul[param_groups$param == param_names[i]],
                                             param_groups$dermthd[param_groups$param == param_names[i]])
                             }) %>% paste(., collapse = '\n'),
                             sep = '\n')

    param_data = paste('* parameter data',
                       sapply(seq_along(param_names), function(i){
                         cur_defaults = get_default_param_vals(param_name = param_names[i],
                                                               model_run_loc = model_run_loc,
                                                               param_default_file = param_default_file)
                         cur_params = params[[param_names[i]]]

                         if(cur_defaults$dim == 'one'){
                           param_name_out = param_names[i]
                           out = sprintf('%s %s %s %s %s %s %s 1.0 0.0 1',
                                         param_name_out, # parameter name
                                         param_groups$partrans[param_groups$param == param_names[i]], # transformation of parameter
                                         param_groups$parchglim[param_groups$param == param_names[i]], # limitation of parameter adjustment
                                         cur_params$vals,  # initial parameter value
                                         param_groups$min[param_groups$param == param_names[i]], # lower bound of parameter
                                         param_groups$max[param_groups$param == param_names[i]], # upper bound of parameter
                                         param_names[i]) # parameter group
                         }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nsegment'){ # a stream segment-based parameter
                           out = sapply(seq_len(nrow(cur_params)), function(j){
                             param_name_out = paste(param_names[i], cur_params$seg_model_idx[j], sep = '_')
                             cur_out = sprintf('%s %s %s %s %s %s %s 1.0 0.0 1',
                                               param_name_out, # parameter name
                                               param_groups$partrans[param_groups$param == param_names[i]], # transformation of parameter
                                               param_groups$parchglim[param_groups$param == param_names[i]], # limitation of parameter adjustment
                                               cur_params$vals[j],  # initial parameter value
                                               param_groups$min[param_groups$param == param_names[i]], # lower bound of parameter
                                               param_groups$max[param_groups$param == param_names[i]], # upper bound of parameter
                                               param_names[i]) # parameter group
                           }) %>% paste(., collapse = '\n')
                         }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nhru'){
                           out = sapply(seq_len(nrow(cur_params)), function(j){
                             param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], sep = '_')
                             cur_out = sprintf('%s %s %s %s %s %s %s 1.0 0.0 1',
                                               param_name_out, # parameter name
                                               param_groups$partrans[param_groups$param == param_names[i]], # transformation of parameter
                                               param_groups$parchglim[param_groups$param == param_names[i]], # limitation of parameter adjustment
                                               cur_params$vals[j],  # initial parameter value
                                               param_groups$min[param_groups$param == param_names[i]], # lower bound of parameter
                                               param_groups$max[param_groups$param == param_names[i]], # upper bound of parameter
                                               param_names[i]) # parameter group
                           }) %>% paste(., collapse = '\n')
                         }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'ngw'){
                           out = sapply(seq_len(nrow(cur_params)), function(j){
                             param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], sep = '_')
                             cur_out = sprintf('%s %s %s %s %s %s %s 1.0 0.0 1',
                                               param_name_out, # parameter name
                                               param_groups$partrans[param_groups$param == param_names[i]], # transformation of parameter
                                               param_groups$parchglim[param_groups$param == param_names[i]], # limitation of parameter adjustment
                                               cur_params$vals[j],  # initial parameter value
                                               param_groups$min[param_groups$param == param_names[i]], # lower bound of parameter
                                               param_groups$max[param_groups$param == param_names[i]], # upper bound of parameter
                                               param_names[i]) # parameter group
                           }) %>% paste(., collapse = '\n')
                         }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nssr'){
                           out = sapply(seq_len(nrow(cur_params)), function(j){
                             param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], sep = '_')
                             cur_out = sprintf('%s %s %s %s %s %s %s 1.0 0.0 1',
                                               param_name_out, # parameter name
                                               param_groups$partrans[param_groups$param == param_names[i]], # transformation of parameter
                                               param_groups$parchglim[param_groups$param == param_names[i]], # limitation of parameter adjustment
                                               cur_params$vals[j],  # initial parameter value
                                               param_groups$min[param_groups$param == param_names[i]], # lower bound of parameter
                                               param_groups$max[param_groups$param == param_names[i]], # upper bound of parameter
                                               param_names[i]) # parameter group
                           }) %>% paste(., collapse = '\n')
                         }else if(cur_defaults$ndim == '2'){
                           if(grepl('nsegment', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
                             # per segment x month basis is organized in order of segment model_idx and then month
                             #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 455_Dec, 456_Dec
                             out = sapply(seq_len(nrow(cur_params)), function(j){
                               param_name_out = paste(param_names[i], cur_params$seg_model_idx[j], cur_params$month[j], sep = '_')
                               cur_out = sprintf('%s %s %s %s %s %s %s 1.0 0.0 1',
                                                 param_name_out, # parameter name
                                                 param_groups$partrans[param_groups$param == param_names[i]], # transformation of parameter
                                                 param_groups$parchglim[param_groups$param == param_names[i]], # limitation of parameter adjustment
                                                 cur_params$vals[j],  # initial parameter value
                                                 param_groups$min[param_groups$param == param_names[i]], # lower bound of parameter
                                                 param_groups$max[param_groups$param == param_names[i]], # upper bound of parameter
                                                 param_names[i]) # parameter group
                             }) %>% paste(., collapse = '\n')
                           }else if(grepl('nhru', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
                             # per hru x month basis is organized in order of hru model_idx and then month
                             #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 755_Dec, 756_Dec
                             out = sapply(seq_len(nrow(cur_params)), function(j){
                               param_name_out = paste(param_names[i], cur_params$hru_model_idx[j], cur_params$month[j], sep = '_')
                               cur_out = sprintf('%s %s %s %s %s %s %s 1.0 0.0 1',
                                                 param_name_out, # parameter name
                                                 param_groups$partrans[param_groups$param == param_names[i]], # transformation of parameter
                                                 param_groups$parchglim[param_groups$param == param_names[i]], # limitation of parameter adjustment
                                                 cur_params$vals[j],  # initial parameter value
                                                 param_groups$min[param_groups$param == param_names[i]], # lower bound of parameter
                                                 param_groups$max[param_groups$param == param_names[i]], # upper bound of parameter
                                                 param_names[i]) # parameter group
                             }) %>% paste(., collapse = '\n')
                           }
                         }
                       }) %>% paste(., collapse = '\n'),
                       sep = '\n')
  }

  if(calibrate_temp & calibrate_flow){
    obs_groups = paste('* observation groups',
                       'wtemp',
                       'flow',
                       sep = '\n')
  }else if(calibrate_temp & !calibrate_flow){
    obs_groups = paste('* observation groups',
                       'wtemp',
                       sep = '\n')
  }else if(!calibrate_temp & calibrate_flow){
    obs_groups = paste('* observation groups',
                       'flow',
                       sep = '\n')
  }


  if(calibrate_temp){
    temp_obs_data = sapply(seq_along(dates), function(j){
      cur_date = dates[j]
      sapply(seq_along(cur_model_idxs), function(i){
        cur = paste('wtemp',
                    as.character(cur_model_idxs[i]),
                    cur_date,
                    sep = '_')
        if(cur %in% obs$temp$obs_name){ # if obs matches predictions, use weight of 1.0
          if(weight_by_magnitude){
            if(calibrate_flow){
              cur_weight = calc_weight(all_obs = c(obs$temp$temp_C, obs$flow$discharge_cfs),
                                       cur_obs = obs$temp$temp_C[obs$temp$obs_name == cur]) # assigning weight based on magnitude of value
            }else{
              cur_weight = calc_weight(all_obs = c(obs$temp$temp_C),
                                       cur_obs = obs$temp$temp_C[obs$temp$obs_name == cur]) # assigning weight based on magnitude of value
            }
          }else{cur_weight = '1.0'}
          out = sprintf('%s %s %s %s', cur, obs$temp$temp_C[obs$temp$obs_name == cur], cur_weight, 'wtemp')
        }else{ # no observation available so giving 0 weight
          out = sprintf('%s %s %s %s', cur, '0.0', '0.0', 'wtemp')
        }
        return(out)
      }) %>% paste(., collapse = '\n')
    }) %>% paste(., collapse = '\n') %>% paste('* observation data', ., sep = '\n')
  }

  if(calibrate_flow){
    flow_obs_data = sapply(seq_along(dates), function(j){
      cur_date = dates[j]
      sapply(seq_along(cur_model_idxs), function(i){
        cur = paste('flow',
                    as.character(cur_model_idxs[i]),
                    cur_date,
                    sep = '_')
        if(cur %in% obs$flow$obs_name){ # if obs matches predictions, use weight of 1.0
          if(weight_by_magnitude){
            if(calibrate_temp){
              cur_weight = calc_weight(all_obs = c(obs$temp$temp_C, obs$flow$discharge_cfs),
                                       cur_obs = obs$flow$discharge_cfs[obs$flow$obs_name == cur]) # assigning weight based on magnitude of value
            }else{
              cur_weight = calc_weight(all_obs = c(obs$flow$discharge_cfs),
                                       cur_obs = obs$flow$discharge_cfs[obs$flow$obs_name == cur]) # assigning weight based on magnitude of value
            }
          }else{cur_weight = '1.0'}
          out = sprintf('%s %s %s %s', cur, obs$flow$discharge_cfs[obs$flow$obs_name == cur], cur_weight, 'flow')
        }else{ # no observation available so giving 0 weight
          out = sprintf('%s %s %s %s', cur, '0.0', '0.0', 'flow')
        }
        return(out)
      }) %>% paste(., collapse = '\n')
    }) %>% paste(., collapse = '\n')
    if(!calibrate_temp){
      flow_obs_data = paste('* observation data', flow_obs_data, sep = '\n')
    }
  }

  model_cmd_line = paste('* model command line',
                         '"C:/Program Files/R/R-4.0.0/bin/Rscript.exe" ../src/pestpp_model_call.R',
                         sep = '\n')

  if(calibrate_temp & calibrate_flow){
    model_inout = paste('* model input/output',
                        sprintf('%s %s', tpl_file_name, param_file_name),
                        sprintf('%s %s', temp_ins_file_name, temp_model_output_file),
                        sprintf('%s %s', flow_ins_file_name, flow_model_output_file),
                        sep = '\n')
  }else if(calibrate_temp & !calibrate_flow){
    model_inout = paste('* model input/output',
                        sprintf('%s %s', tpl_file_name, param_file_name),
                        sprintf('%s %s', temp_ins_file_name, temp_model_output_file),
                        sep = '\n')
  }else if(!calibrate_temp & calibrate_flow){
    model_inout = paste('* model input/output',
                        sprintf('%s %s', tpl_file_name, param_file_name),
                        sprintf('%s %s', flow_ins_file_name, flow_model_output_file),
                        sep = '\n')
  }


  # model_out = paste('* model output',
  #                   sprintf('%s %s', ins_file_name, model_output_file),
  #                   sep = '\n')

  prior_inf = paste('* prior information',
                    sep = '\n')

  reg = paste('* regularization',
              sep = '\n')

  control_vars = paste(sprintf('++ tie_by_group(%s)', tolower(tie_by_group)),
                       sep = '\n')


  if(calibrate_temp & calibrate_flow){
    pst_out = paste(first_line,
                    control_data,
                    param_groups_out,
                    param_data,
                    obs_groups,
                    temp_obs_data,
                    flow_obs_data,
                    model_cmd_line,
                    model_inout,
                    # model_out,
                    prior_inf,
                    reg,
                    control_vars,
                    sep = '\n')
  }else if(calibrate_temp & !calibrate_flow){
    pst_out = paste(first_line,
                    control_data,
                    param_groups_out,
                    param_data,
                    obs_groups,
                    temp_obs_data,
                    model_cmd_line,
                    model_inout,
                    # model_out,
                    prior_inf,
                    reg,
                    control_vars,
                    sep = '\n')
  }else if(!calibrate_temp & calibrate_flow){
    pst_out = paste(first_line,
                    control_data,
                    param_groups_out,
                    param_data,
                    obs_groups,
                    flow_obs_data,
                    model_cmd_line,
                    model_inout,
                    # model_out,
                    prior_inf,
                    reg,
                    control_vars,
                    sep = '\n')
  }


  writeLines(pst_out, file.path(model_run_loc, file_out))
}



calc_weight = function(all_obs, cur_obs){
  out = 1 - log10(cur_obs - min(all_obs) + 1) / log10(diff(range(all_obs)))# lower vals get higher weights

  return(round(out, digits = 4))
}

