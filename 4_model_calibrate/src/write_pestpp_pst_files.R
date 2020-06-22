

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

  # output[1:5,1:5]
  cur_model_idxs = unique(params$model_idx)

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




  first_line = sprintf('pif %s', delim)
  second_line = sapply(seq_along(model_idxs), function(i) {
    cur = as.character(model_idxs[i])
    if(cur %in% cur_model_idxs){
      out = sprintf('%s%s%s ', delim, cur, delim)
    }else{
      out = ''
    }
    return(out)
  }) %>% paste(., collapse = '')
  data_lines = sapply(seq_along(dates), function(j){
    cur_date = dates[j]
    sapply(seq_along(model_idxs), function(i){
      cur = as.character(model_idxs[i])
      if(cur %in% cur_model_idxs){
        out = sprintf('%swtemp_%s_%s%s ', secondary_delim, cur, cur_date, secondary_delim)
      }else{
        out = sprintf('%s,%s ', delim, delim)
      }
      return(out)
    }) %>% paste(., collapse = '') %>% paste0(sprintf('l1 %s,%s ', delim, delim), .)
  }) %>% paste(., collapse = '\n')

  pif_out = paste(first_line, second_line, data_lines, sep = '\n')

  writeLines(pif_out, file.path(model_run_loc, file_out))
}


