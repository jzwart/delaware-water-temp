#' Read the site_id column of the crosswalk file
#' @param file should be lib/crosswalks/pipeline_3_lakes.csv
get_site_ids <- function(file) {
  sites <- readr::read_csv(file, col_types = cols('c', 'c'))
  return(sites$site_id)
}

#' List files in directory on Yeti (or any remote server)
#'
#' @param yeti_dir file path on yeti directory from which you want a list of files
yeti_list_files <- function(yeti_dir){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@yeti.cr.usgs.gov', user))
  files = ssh::ssh_exec_internal(session = session, command = paste('ls', yeti_dir))
  files_out = dplyr::tibble(files = unlist(strsplit(rawToChar(files$stdout), split = '\n')))

  ssh::ssh_disconnect(session = session)

  return(files_out)
}

yeti_put <- function(yeti_dir, dest_dir, files){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@yeti.cr.usgs.gov', user))

  file_paths = sprintf('%s/%s', yeti_dir, files)

  ssh::scp_upload(session = session, files = file_paths, to = dest_dir)

  ssh::ssh_disconnect(session = session)
}

yeti_get <- function(yeti_dir, dest_dir, files){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@yeti.cr.usgs.gov', user))

  file_paths = sprintf('%s/%s', yeti_dir, files)

  ssh::scp_download(session = session, files = file_paths, to = dest_dir)

  ssh::ssh_disconnect(session = session)
}
