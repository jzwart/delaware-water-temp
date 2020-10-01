
# This project uses rsync and ssh to pull/push data/predictions from Yeti in the 1_get_lake_attr and 2_model phase. For this to work, SSH keys must be set up for communication with Yeti:
#
#   If you don't yet have a local ssh key pair, use ssh-keygen -t rsa from within a local terminal.
# Copy the public key to Yeti with ssh-copy-id username@yeti.cr.usgs.gov (also from within your local terminal). You can then check that you're set up by running ssh username@yeti.cr.usgs.gov from a terminal - it should log you in without a password.
# On Windows with RStudio, there will be a problem in that SSH/rsync assume your .ssh folder is at ~/.ssh, but ~ means C:/Users/username within a terminal but C:/Users/username/Documents within RStudio. Therefore you should create a symlink for the .ssh folder by calling ln -s ~/.ssh ~/Documents/.ssh in a bash shell.

#' List files in directory on remote server (e.g. Yeti, Denali)
#'
#' @param server_dir file path on remote server directory from which you want a list of files
list_server_files <- function(server_dir,
                              server){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@%s.cr.usgs.gov', user, server))
  files = ssh::ssh_exec_internal(session = session, command = paste('ls', server_dir))
  files_out = dplyr::tibble(files = unlist(strsplit(rawToChar(files$stdout), split = '\n')))

  files_out = files_out$files

  ssh::ssh_disconnect(session = session)

  return(files_out)
}

#' return rds of files on server
#'
server_files <- function(ind_file,
                         server_dir,
                         server,
                         gd_config = 'lib/cfg/gd_config.yml'){
  files = list_server_files(server_dir = server_dir,
                            server = server)

  if(sum(grepl('.ind', files$files)) >= 1){
    non_ind_files = dplyr::tibble(files = files$files[-grep('.ind', files$files)])
  }else{
    non_ind_files = dplyr::tibble(files = files$files)
  }

  data_file <- as_data_file(ind_file)
  saveRDS(non_ind_files, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}

get_local_files <- function(local_dir){

  files_to_transfer = list.files(local_dir,
                                 # recursive = T,
                                 include.dirs = T)
  return(files_to_transfer)
}


server_put <- function(local_dir,
                       server_dir,
                       files = NULL,
                       server){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@%s.cr.usgs.gov', user, server))
  on.exit(ssh::ssh_disconnect(session = session))

  if(is.null(files)){
    files = get_local_files(local_dir = local_dir)
  }

  file_paths = sprintf('%s/%s', local_dir, files)

  ssh::scp_upload(session = session, files = file_paths, to = server_dir)
}


server_get <- function(local_dir,
                       server_dir,
                       files = NULL,
                       server){
  user <- Sys.info()[['user']]
  session <- ssh::ssh_connect(sprintf('%s@%s.cr.usgs.gov', user, server))
  on.exit(ssh::ssh_disconnect(session = session))

  if(is.null(files)){
    files = list_server_files(server_dir = server_dir,
                              server = server)
  }

  file_paths = sprintf('%s/%s', server_dir, files)
  # file_paths = server_dir

  ssh::scp_download(session = session, files = file_paths, to = local_dir)

}
