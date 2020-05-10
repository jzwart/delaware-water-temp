

###Loading required libraries
library(hydroPSO)
library(hydroTSM)
library(hydroGOF)

###Definition of working directory: input, output and model files paths
model.drty <- "~/MF2005"
setwd(model.drty )

###Customised I/O functions (R scripts) to interface MF2005 with hydroPSO
# source("read.hobs.R")
# source("read.hsim.R")
# source("read.wbal.R")
# source("read.lik.R")

###Goodness-of-fit, either customised or pre-defined from hydroGOF
gof.FUN <- "sntemp_lik"
gof.FUN.args <- list(model_run_loc = model_run_loc,
                     obs = cur_obs)

###MAIN model function
model.FUN <- "hydromod"
model.FUN.args <- list(
  model.drty = model_run_loc,
  param.files = file.path(model_run_loc,"PSO.in/ParamFiles.txt"),
  exe.fname = "delaware.bat",
  ###Function for reading the simulated equivalents
  out.FUN = "read.hsim",
  out.FUN.args = list(
    fname="M2.LST",
    nobs=42),
  ###Function assessing the simulated equivalents against the observations
  gof.FUN=gof.FUN,
  gof.FUN.args=gof.FUN.args,
  obs=obs
) ###END model.FUN.args




###MAIN PSO ALGORITHM
###For hydroPSO fine-tuning parameters, see Zambrano-Bigiarini and Rojas,2012
#set.seed(1111)
hydroPSO(
  fn="hydromod",
  model.FUN.args=model.FUN.args,
  method="spso2011",
  control=list(
    MinMax="max",
    npart=70,
    maxit=3000,
    reltol=1e-30,
    c1=2.05,
    c2=2.05,
    use.IW=FALSE,
    use.CF=TRUE,
    use.TVlambda=TRUE,TVlambda.type="linear",TVlambda.rng=c(1.0,0.5),TVlambda.exp=1,
    topology="random", k=11,
    drty.out="SPSO2011_i31.out",
    boundary.wall="reflecting",
    normalise=TRUE,
    REPORT=50
  ) ###END control options
) ###END MAIN hydroPSO ALGORITHM




# returns nll of predictions / observations for sntemp
sntemp_lik = function(model_run_loc, obs){

  preds = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                 model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

  compare = left_join(preds, select(obs, model_idx, date, temp_C),
                      by = c('model_idx', 'date'))

  # plot(compare$water_temp ~ compare$temp_C,
  #      ylim = c(0, max(compare$temp_C, na.rm = T)), ylab = 'pred', xlab = 'obs')

  return(nll(compare$temp_C, compare$water_temp))
}



# returns negative log likelihood
nll = function(obs, pred){
  if (any(is.na(obs))){
    NA_obs <- which(is.na(obs))
    res <- obs[-NA_obs] - pred[-NA_obs]
  } else{
    res <- obs - pred
  }

  nRes <- length(res)
  SSE <- sum(res^2)
  sigma2 <- SSE/nRes
  NLL <- 0.5*((SSE/sigma2) + nRes*log(2*pi*sigma2))

  return(NLL)
}





sntemp_out = function(param.values,
                      start,
                      stop,
                      model_run_loc,
                      spinup = F,
                      restart = F,
                      var_init_file,
                      var_save_file,
                      obs,
                      model_idxs_to_cal,
                      all_params,
                      param_names){
  # debugging
  print(param.values)
  print(model_idxs_to_cal)
  #

  updated_params = combine_cal_uncal_params(cal_params = pararm.values,
                                            all_params = all_params,
                                            param_names = param_names)

  update_sntemp_params(param_names = param_names,
                       updated_params = updated_params,
                       model_run_loc = model_run_loc)

  run_sntemp(start = start,
             stop = stop,
             spinup = spinup,
             restart = restart,
             model_run_loc = model_run_loc,
             var_init_file = var_init_file,
             var_save_file = var_save_file)

  preds = get_sntemp_temperature(model_output_file = file.path(model_run_loc, 'output/seg_tave_water.csv'),
                                 model_fabric_file = file.path(model_run_loc, 'GIS/Segments_subset.shp'))

  compare = left_join(preds, select(obs, model_idx, date, temp_C),
                      by = c('model_idx', 'date'))

  # plot(compare$water_temp ~ compare$temp_C,
  #      ylim = c(0, max(compare$temp_C, na.rm = T)), ylab = 'pred', xlab = 'obs')

  return(nll(compare$temp_C, compare$water_temp)) #optimize on NLL for water temp
  # return(rmse(compare$temp_C, compare$water_temp, na.rm = T)) #optimize on water temp RMSE
}



