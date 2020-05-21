
#' List files in directory on remote server (e.g. Yeti, Denali)
#'
#' @param server_dir file path on remote server directory from which you want a list of files
list_server_files <- function(server_dir,
                              server){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@%s.cr.usgs.gov', user, server))
  files = ssh::ssh_exec_internal(session = session, command = paste('ls', server_dir))
  files_out = dplyr::tibble(files = unlist(strsplit(rawToChar(files$stdout), split = '\n')))

  ssh::ssh_disconnect(session = session)

  return(files_out)
}

#' return rds of files on Yeti
#'
yeti_files <- function(ind_file, yeti_dir){
  files = yeti_list_files(yeti_dir = yeti_dir)

  if(sum(grepl('.ind', files$files)) >= 1){
    non_ind_files = dplyr::tibble(files = files$files[-grep('.ind', files$files)])
  }else{
    non_ind_files = dplyr::tibble(files = files$files)
  }

  data_file <- as_data_file(ind_file)
  saveRDS(non_ind_files, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file)
}



server_put <- function(local_dir,
                       server_dir,
                       files,
                       server){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@%s.cr.usgs.gov', user, server))
  on.exit(ssh::ssh_disconnect(session = session))

  file_paths = sprintf('%s/%s', local_dir, files)

  ssh::scp_upload(session = session, files = file_paths, to = server_dir)

}


server_get <- function(local_dir,
                       server_dir,
                       files,
                       server){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@%s.cr.usgs.gov', user, server))

  file_paths = sprintf('%s/%s', server_dir, files)

  ssh::scp_download(session = session, files = file_paths, to = local_dir)

  ssh::ssh_disconnect(session = session)
}
