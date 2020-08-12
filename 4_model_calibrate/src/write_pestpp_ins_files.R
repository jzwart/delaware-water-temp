

# function for writing template files for pestpp
write_pestpp_ins_files = function(params,
                                  model_run_loc,
                                  model_output_file,
                                  file_out,
                                  delim,
                                  secondary_delim){

  if(!is.null(params$seg_params)){
    seg_params = params$seg_params
  }else{seg_params = NULL}
  if(!is.null(params$seg_month_params)){
    seg_month_params = params$seg_month_params
  }else{seg_month_params = NULL}

  output = read.csv(file.path(model_run_loc, model_output_file), header = T)
  model_idxs = seq(1,ncol(output)-1)
  colnames(output)[2:ncol(output)] = model_idxs
  dates = strftime(strptime(output$Date, format = '%Y-%m-%d'), '%Y%m%d') # date vector for

  # output[1:5,1:5]
  cur_model_idxs = unique(seg_params$model_idx)

  # format for .ins file based on modeled temperature output:
  #
  # pif @
  # @1@ @2@ @414@ ...
  # l1 @,@ !wtemp_1_19801001! @,@ !wtemp_2_19801001! @,@ @,@....@,@ !wtemp_414_19801001! @,@ ...
  # l1 @,@ !wtemp_1_19801002! @,@ !wtemp_2_19801002! @,@ @,@....@,@ !wtemp_414_19801002! @,@ ...
  # ...
  # l1 @,@ !wtemp_1_20040930! @,@ !wtemp_2_20040930! @,@ @,@....@,@ !wtemp_414_20040930! @,@ ...

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


