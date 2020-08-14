

# function for writing control files for pestpp
write_pestpp_pst_files = function(params,
                                  seg_model_idxs,
                                  model_run_loc,
                                  model_output_file,
                                  obs,
                                  file_out,
                                  param_groups,
                                  tpl_file_name,
                                  param_file_name,
                                  ins_file_name,
                                  tie_by_group = F){

  # if(!is.null(params$seg_params)){
  #   seg_params = params$seg_params
  #   seg_param_names = unique(seg_params$param_name)
  # }else{seg_params = NULL}
  # if(!is.null(params$seg_month_params)){
  #   seg_month_params = params$seg_month_params
  #   seg_month_param_names = unique(seg_month_param_names$param_name)
  # }else{seg_month_params = NULL}

  output = read.csv(file.path(model_run_loc, model_output_file), header = T)
  model_idxs = seq(1,ncol(output)-1)
  colnames(output)[2:ncol(output)] = model_idxs
  dates = strftime(strptime(output$Date, format = '%Y-%m-%d'), '%Y%m%d') # date vector for

  cur_model_idxs = as.character(sort(as.numeric(seg_model_idxs)))

  obs$obs_name = paste('wtemp',
                       obs$model_idx,
                       strftime(strptime(obs$date, format = '%Y-%m-%d'), '%Y%m%d'),
                       sep = '_')

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

  first_line = 'pcf'

  param_names = names(params)
  npargp = length(param_names) # number of parameter groups
  npar = 0  # number of parameters to be calibrated
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

  #######################I'm HERE ##################################
  # change this to be input from yaml file (similar to range of values )
  if(!is.null(seg_params) & !is.null(seg_month_params)){
    param_groups_out = paste('* parameter groups',
                             sapply(seq_along(seg_param_names), function(i){
                               out = sprintf('%s %s %s %s %s %s %s',
                                             seg_param_names[i],
                                             param_groups$inctyp[param_groups$param == seg_param_names[i]],
                                             param_groups$derinc[param_groups$param == seg_param_names[i]],
                                             param_groups$derinclb[param_groups$param == seg_param_names[i]],
                                             param_groups$forcen[param_groups$param == seg_param_names[i]],
                                             param_groups$derincmul[param_groups$param == seg_param_names[i]],
                                             param_groups$dermthd[param_groups$param == seg_param_names[i]])
                             }) %>% paste(., collapse = '\n'),
                             sapply(seq_along(seg_month_param_names), function(i){
                               out = sprintf('%s %s %s %s %s %s %s',
                                             seg_month_param_names[i],
                                             param_groups$inctyp[param_groups$param == seg_month_param_names[i]],
                                             param_groups$derinc[param_groups$param == seg_month_param_names[i]],
                                             param_groups$derinclb[param_groups$param == seg_month_param_names[i]],
                                             param_groups$forcen[param_groups$param == seg_month_param_names[i]],
                                             param_groups$derincmul[param_groups$param == seg_month_param_names[i]],
                                             param_groups$dermthd[param_groups$param == seg_month_param_names[i]])
                             }) %>% paste(., collapse = '\n'),
                             sep = '\n')

    param_data = paste('* parameter data',
                       sapply(seq_along(seg_params$model_idx), function(i){
                         out = sprintf('%s_%s %s factor %s %s %s %s 1.0 0.0 1',
                                       seg_params$param_name[i], # parameter name
                                       seg_params$model_idx[i], # parameter name
                                       param_groups$partrans[param_groups$param == seg_params$param_name[i]], # transformation of parameter
                                       seg_params$param_value[i],  # initial parameter value
                                       param_groups$min[param_groups$param == seg_params$param_name[i]], # lower bound of parameter
                                       param_groups$max[param_groups$param == seg_params$param_name[i]], # upper bound of parameter
                                       seg_params$param_name[i]) # parameter group
                       }) %>% paste(., collapse = '\n'),
                       sapply(seq_along(seg_month_params$model_idx), function(i){
                         out = sprintf('%s_%s_%s %s relative %s %s %s %s 1.0 0.0 1',
                                       seg_month_params$param_name[i], # parameter name
                                       seg_month_params$model_idx[i], # model idx
                                       seg_month_params$month[i], # month
                                       param_groups$partrans[param_groups$param == seg_month_params$param_name[i]], # transformation of parameter
                                       seg_month_params$param_value[i],  # initial parameter value
                                       param_groups$min[param_groups$param == seg_month_params$param_name[i]], # lower bound of parameter
                                       param_groups$max[param_groups$param == seg_month_params$param_name[i]], # upper bound of parameter
                                       seg_month_params$param_name[i]) # parameter group
                       }) %>% paste(., collapse = '\n'),
                       sep = '\n')
  }else{
    param_groups_out = paste('* parameter groups',
                         sapply(seq_along(seg_param_names), function(i){
                           out = sprintf('%s %s %s %s %s %s %s',
                                         seg_param_names[i],
                                         param_groups$inctyp[param_groups$param == seg_param_names[i]],
                                         param_groups$derinc[param_groups$param == seg_param_names[i]],
                                         param_groups$derinclb[param_groups$param == seg_param_names[i]],
                                         param_groups$forcen[param_groups$param == seg_param_names[i]],
                                         param_groups$derincmul[param_groups$param == seg_param_names[i]],
                                         param_groups$dermthd[param_groups$param == seg_param_names[i]])
                         }) %>% paste(., collapse = '\n'),
                         sep = '\n')

    param_data = paste('* parameter data',
                       sapply(seq_along(seg_params$model_idx), function(i){
                         out = sprintf('%s_%s %s factor %s %s %s %s 1.0 0.0 1',
                                       seg_params$param_name[i], # parameter name
                                       seg_params$model_idx[i], # parameter name
                                       param_groups$partrans[param_groups$param == seg_params$param_name[i]], # transformation of parameter
                                       seg_params$param_value[i],  # initial parameter value
                                       param_groups$min[param_groups$param == seg_params$param_name[i]], # lower bound of parameter
                                       param_groups$max[param_groups$param == seg_params$param_name[i]], # upper bound of parameter
                                       seg_params$param_name[i]) # parameter group
                       }) %>% paste(., collapse = '\n'),
                       sep = '\n')
  }


  obs_groups = paste('* observation groups',
                     'wtemp',
                     sep = '\n')

  obs_data = sapply(seq_along(dates), function(j){
    cur_date = dates[j]
    sapply(seq_along(cur_model_idxs), function(i){
      cur = paste('wtemp',
                  as.character(cur_model_idxs[i]),
                  cur_date,
                  sep = '_')
      if(cur %in% obs$obs_name){ # if obs matches predictions, use weight of 1.0
        out = sprintf('%s %s %s %s', cur, obs$temp_C[obs$obs_name == cur], '1.0', 'wtemp')
      }else{ # no observation available so giving 0 weight
        out = sprintf('%s %s %s %s', cur, '0.0', '0.0', 'wtemp')
      }
      return(out)
    }) %>% paste(., collapse = '\n')
  }) %>% paste(., collapse = '\n') %>% paste('* observation data', ., sep = '\n')

  model_cmd_line = paste('* model command line',
                         '"C:/Program Files/R/R-4.0.0/bin/Rscript.exe" ../src/pestpp_model_call.R',
                         sep = '\n')

  model_inout = paste('* model input/output',
                      sprintf('%s %s', tpl_file_name, param_file_name),
                      sprintf('%s %s', ins_file_name, model_output_file),
                      sep = '\n')

  # model_out = paste('* model output',
  #                   sprintf('%s %s', ins_file_name, model_output_file),
  #                   sep = '\n')

  prior_inf = paste('* prior information',
                    sep = '\n')

  reg = paste('* regularization',
              sep = '\n')

  control_vars = paste(sprintf('++ tie_by_group(%s)', tolower(tie_by_group)),
                       sep = '\n')


  pst_out = paste(first_line,
                  control_data,
                  param_groups_out,
                  param_data,
                  obs_groups,
                  obs_data,
                  model_cmd_line,
                  model_inout,
                  # model_out,
                  prior_inf,
                  reg,
                  control_vars,
                  sep = '\n')

  writeLines(pst_out, file.path(model_run_loc, file_out))
}
