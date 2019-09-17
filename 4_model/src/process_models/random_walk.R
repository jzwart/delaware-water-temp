
#' random walk model
#'
#' @param states model states at the previous time step
#' @param sd state error as standard deviation

random_walk = function(states, sd){
  states_1 = rnorm(n = length(states), mean = states, sd = sd)

  return(states_1)
}
