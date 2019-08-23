

n_en = 100 # number of ensembles

n_params_est = 10 # number of parameters we're calibrating

n_params_obs = 0 # number of parameters for which we have observations

n_states = 10 # number of states we're updating; will be dependent on obs

start = as.Date('2010-01-01')

stop = as.Date('2015-01-01')

time_step = 'days'

model_dates = seq.Date(from = start, to = stop, by = time_step)

n_step = length(model_dates)

Y = array(dim = c(n_states + n_params_est, n_step, n_en)) # vector for holding states and parameters for updating

R = array(0, dim = c(n_states + n_params_obs, n_states + n_params_obs, n_step)) # observation error matrix, should be a square matrix where col & row = the number of states and params for which you have observations

temp_sd = .53 # estimating temperature observation accruacy based on https://www.onsetcomp.com/products/data-loggers/ua-002-64
temp_var = temp_sd^2 #variance of temperature observations

param_sd = 0
param_var = param_sd^2

for(i in 1:n_states){
  R[i, i, ] = temp_var # variance is the same for each depth and time step; could make dynamic or varying by time step if we have good reason to do so
}

# measurement operator matrix saying 1 if there is observation data available, 0 otherwise
H = array(0, dim=c(n_states + n_params_obs, n_states + n_params_est, n_step)) # dimensions will be n_depths x Y vector length (or n_depths + n_params)

obs = data.frame() # create a dataframe that contains observations of temperature for each model timestep and each depth you're estimating (dimensions should be n_depth rows by nStep columns)

for(t in 1:n_step){
  H[1, 1, t] = ifelse(!is.na(obs[1, t]), 1, 0) # checks if there is an observation at given depth and timestep and puts in 1 if there is obs, 0 otherwise
  H[2, 2, t] = ifelse(!is.na(obs[2, t]), 1, 0)
  H[3, 3, t] = ifelse()....# fill in rest for all depths
  .....
}

##' @param Y vector for holding states and parameters you're estimating
##' @param R observation error matrix
##' @param obs observations at current timestep
##' @param H observation identity matrix
##' @param n_en number of ensembles
##' @param cur_step current model timestep
kalman_filter = function(Y, R, obs, H, n_en, cur_step){

  ###### estimate the spread of your ensembles #####
  Y_mean = matrix(apply(Y[,cur_step,], MARGIN = 1, FUN = mean), nrow = length(Y[,1,1])) # calculating the mean of each temp and parameter estimate
  delta_Y = Y[,cur_step,] - matrix(rep(Y_mean, n_en), nrow = length(Y[,1,1])) # difference in ensemble state/parameter and mean of all ensemble states/parameters

  ###### estimate Kalman gain #########
  K = ((1 / (nEn - 1)) * delta_Y %*% t(delta_Y) %*% t(H[, , cur_step])) %*%
    qr.solve(((1 / (nEn - 1)) * H[, , cur_step] %*% delta_Y %*% t(delta_Y) %*% t(H[, , cur_step]) + R[, , cur_step]))

  ###### update Y vector ######
  for(q in 1:n_en){
    Y[, cur_step, q] = Y[, cur_step, q] + K %*% (obs - H[, , cur_step] %*% Y[, cur_step, q]) # adjusting each ensemble using kalman gain and observations
  }
  return(Y)
}

for(t in 1:n_step){
  for(n in 1:n_en){
    # set parameters / states for model config
    # run model
    Y = model_output # store in Y vector
  }
  if(any(obs)){
    Y = kalman_filter(Y = Y,
                      R = R,
                      cur_obs = obs[t, ],
                      H = H,
                      n_en = n_en,
                      cur_step = t) # updating params / states if obs available
  }
  # update states / params for model config
}





