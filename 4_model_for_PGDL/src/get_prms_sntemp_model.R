


get_prms_sntemp_model = function(gd_zip_ind_file,
                                 unzip_loc,
                                 gd_config = 'lib/cfg/gd_config.yml'){

  gd_get(gd_zip_ind_file)

  unzip(zipfile = scipiper::as_data_file(gd_zip_ind_file),
        exdir = unzip_loc)
}


copy_model_to_run_dir = function(model_run_loc,
                                 orig_model_loc){
  # copy over original run files to temporary file location
  dir.create(model_run_loc, showWarnings = F)
  print('Copying original model files to model working directory...')
  files_to_transfer = list.files(orig_model_loc)
  file.copy(from = file.path(orig_model_loc, files_to_transfer),
            to = model_run_loc,
            overwrite = T,
            recursive = T)

}

