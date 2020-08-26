
# pre-processor for changing the floats to integers for ss_tau and gw_tau

# change_floats_to_int()
model_run_loc = '.'
param_file = 'input/myparam.param'
n_segments = 456
params = readLines(file.path(model_run_loc, param_file))

param_names = c('gw_tau', 'ss_tau')

for(i in 1:length(param_names)){
  param_loc_start = grep(param_names[i], params) + 5
  param_loc_end = param_loc_start + n_segments - 1

  params[param_loc_start:param_loc_end] = as.character(round(as.numeric(params[param_loc_start:param_loc_end]), digits = 0))
}

writeLines(params, file.path(model_run_loc, param_file))

shell('delaware.bat') # run batch file

# command line argument for pestpp to call this script: "C:/Program Files/R/R-4.0.0/bin/Rscript.exe" r_file.R


# function for changing floats to integers
# change_floats_to_int = function(model_run_loc = '.',
#                                 param_file = 'input/myparam.param',
#                                 n_segments = 456){
#
#   params = readLines(file.path(model_run_loc, param_file))
#
#   param_names = c('gw_tau', 'ss_tau')
#
#   for(i in 1:length(param_names)){
#     param_loc_start = grep(param_names[i], params) + 5
#     param_loc_end = param_loc_start + n_segments - 1
#
#     params[param_loc_start:param_loc_end] = as.character(round(as.numeric(params[param_loc_start:param_loc_end]), digits = 0))
#   }
#
#   writeLines(params, file.path(model_run_loc, param_file))
# }
