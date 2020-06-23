

# function for writing template files for pestpp
write_pestpp_pst_files = function(params,
                                  model_run_loc,
                                  model_output_file,
                                  obs,
                                  file_out,
                                  delim,
                                  secondary_delim){

  output = read.csv(file.path(model_run_loc, model_output_file), header = T)
  model_idxs = seq(1,ncol(output)-1)
  colnames(output)[2:ncol(output)] = model_idxs
  dates = strftime(strptime(output$Date, format = '%Y-%m-%d'), '%Y%m%d') # date vector for

  cur_model_idxs = unique(params$model_idx)

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

  control_data = paste('* control data',
                       'restart estimation',
                       sprintf('%s %s %s %s %s',
                               nrow(params),
                               length(dates) * length(cur_model_idxs),
                               length(unique(params$param_name)),
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


  param_groups = paste('* parameter groups',
                       'ss_tau relative 0.01 0.01 switch 2.0 parabolic',
                       'gw_tau relative 0.01 0.01 switch 2.0 parabolic',
                       sep = '\n')

  param_data = paste()

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
                         'delaware.bat',
                         sep = '\n')

  model_inout = paste('* model inputoutput',
                      sep = '\n')

  model_out = paste('* model output',
                    sep = '\n')

  prior_inf = paste('* prior information',
                    sep = '\n')

  reg = paste('* regularization',
              sep = '\n')


  pst_out = paste(first_line,
                  control_data,
                  param_groups,
                  param_data,
                  obs_groups,
                  obs_data,
                  model_cmd_line,
                  model_inout,
                  model_out,
                  prior_inf,
                  reg,
                  sep = '\n')

  writeLines(pif_out, file.path(model_run_loc, file_out))
}


