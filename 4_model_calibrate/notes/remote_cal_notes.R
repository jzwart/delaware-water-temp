




d = list_server_files(server_dir ='/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/driver-data',
                  server = 'yeti' )


d = list_server_files(server_dir ='/caldera/projects/usgs/water/iidd/datasci/delaware-water-temp',
                      server = 'denali' )


server_put(local_dir = 'C:/Users/jzwart/Documents',
           server_dir = '/caldera/projects/usgs/water/iidd/datasci/delaware-water-temp',
           files = '.Renviron',
           server = 'denali')

'C:/Users/jzwart/Documents/.Renviron'

# This project uses rsync and ssh to pull/push data/predictions from Yeti in the 1_get_lake_attr and 2_model phase. For this to work, SSH keys must be set up for communication with Yeti:
#
#   If you don't yet have a local ssh key pair, use ssh-keygen -t rsa from within a local terminal.
# Copy the public key to Yeti with ssh-copy-id username@yeti.cr.usgs.gov (also from within your local terminal). You can then check that you're set up by running ssh username@yeti.cr.usgs.gov from a terminal - it should log you in without a password.
# On Windows with RStudio, there will be a problem in that SSH/rsync assume your .ssh folder is at ~/.ssh, but ~ means C:/Users/username within a terminal but C:/Users/username/Documents within RStudio. Therefore you should create a symlink for the .ssh folder by calling ln -s ~/.ssh ~/Documents/.ssh in a bash shell.


# for Denali and R ###
#
# TO START R SESSION TYPE:
#
# module avail
# module load cray-R
# R
# quit('yes') # quits R session
#
# I added .Renviron to root of project dir with R_LIBS=...
# I created Rlib folder in root dir
# I installed packages by opening up R session and using install.packages
