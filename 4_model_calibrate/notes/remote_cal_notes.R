




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


######## for installing R packages, follow this: https://hpcportal.cr.usgs.gov/hpc-user-docs/Guides_and_Tutorials/Denali_Guides_and_Tutorials/how-to/Running_R_and_Installing_R_packages/Installing_rgdal_R_package.html

# steps
# login to denali
#
# module switch PrgEnv-cray/6.0.5 PrgEnv-gnu
#
# module load cray-R/3.6.1 gdal/3.0.4 proj/6.2.1
#
# module show proj/6.2.1
#
# srun -A iidd -t 3:00:00 -N 1 -n 1 -c 40 --pty bash -i
#
# wget https://cran.r-project.org/src/contrib/rgdal_1.4-8.tar.gz # or any other tar ball from CRAN
#
# R CMD INSTALL rgdal_1.4-8.tar.gz --configure-args="-with-proj-include=/home/software/denali/arc/apps/proj/6.2.1/GNU/8.3.0/include --with-proj-lib=/home/software/denali/arc/apps/proj/6.2.1/GNU/8.3.0/lib"
#
# rm rgdal_1.4-8.tar.gz
#


######################### for running PRMS on Denali #######################
# I compiled prms on denali using:

# > module load gcc/8.3.0
### moving all PRMS files to Denali
### navigating to prms root dir and making sure there is a bin directory at ../bin
# > make
### navigate to bin directory where prms was put
# > chmod +x prms
### now we should be able to execute prms with:
# > ./prms

### If copying files from Windows machine to Denali, you need to convert delaware.control, myparams.param, and seg_tave_water.csv files to unix using the dos2unix command
# > dos2unix input/myparams.param

### From Steve Markstrom on 2020-07-10 re: PRMS-SNTemp model on Denali:
# "the best version of PRMS on denali that runs stream temp
# is located at /caldera/projects/usgs/water/wbeep/NHM/gf_v11/gridmet/prms_5.1.0/prms/prms/prms
# This is PRMS version 5.1.0 and has been compiled on denali using GCC 8.3.0"







