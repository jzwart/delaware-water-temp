

##'  Kalman Filter
##' @param  M   = model matrix
##' @param  mu0 = initial condition mean vector
##' @param  P0  = initial condition covariance matrix
##' @param  Q   = process error covariance matrix
##' @param  R   = observation error covariance matrix
##' @param  Y   = observation matrix (with missing values as NAs), time as col's
##'
##' @return list
##'  mu.f, mu.a  = state mean vector for (a)nalysis and (f)orecast steps
##'  P.f, P.a    = state covariance matrix for a and f
##'








KalmanFilter <- function(M,mu0,P0,Q,R,Y){

  ## storage
  nstates = nrow(Y)
  nt = ncol(Y)
  mu.f  = matrix(NA,nstates,nt+1)  ## forecast mean for time t
  mu.a  = matrix(NA,nstates,nt)  ## analysis mean for time t
  P.f  = array(NA,c(nstates,nstates,nt+1))  ## forecast variance for time t
  P.a  = array(NA,c(nstates,nstates,nt))  ## analysis variance for time t
  ## initialization
  mu.f[,1] = mu0
  P.f[,,1] = P0
  I = diag(1,nstates)
  ## run updates sequentially for each observation.
  for(t in 1:nt){
    ## Analysis step: combine previous forecast with observed data
    KA <- KalmanAnalysis(mu.f[,t],P.f[,,t],Y[,t],R,I)
    mu.a[,t] <- KA$mu.a
    P.a[,,t] <- KA$P.a

    ## Forecast step: predict to next step from current
    KF <- KalmanForecast(mu.a[,t],P.a[,,t],M,Q)
    mu.f[,t+1] <- KF$mu.f
    P.f[,,t+1] <- KF$P.f
  }

  return(list(mu.f=mu.f,mu.a=mu.a,P.f=P.f,P.a=P.a))
}
##' Kalman Filter: Analysis step
##' @param  mu.f = Forecast mean (vector)
##' @param  P.f  = Forecast covariance (matrix)
##' @param  Y    = observations, with missing values as NAs) (vector)
##' @param  R    = observation error covariance (matrix)
##' @param  H    = observation matrix (maps observations to states)
KalmanAnalysis <- function(mu.f,P.f,Y,R,H){
  obs = !is.na(Y) ## which Y's were observed?
  if(any(obs)){
    H <- H[obs,]                                              ## observation matrix
    K <- P.f %*% t(H) %*% solve(H%*%P.f%*%t(H) + R[obs,obs])  ## Kalman gain
    mu.a <- mu.f + K%*%(Y[obs] - H %*% mu.f)                  ## update mean
    P.a <- (1-K %*% H)*P.f                                    ## update covariance
  } else {
    ##if there's no data, the posterior is the prior
    mu.a = mu.f
    P.a = P.f
  }
  return(list(mu.a=mu.a,P.a=P.a))
}
##' Kalman Filter: Forecast Step
##' @param mu.a = analysis posterior mean (vector)
##' @param P.a  = analysis posterior covariance (matrix)
##' @param M    = model (matrix)
##' @param  Q   = process error covariance (matrix)
KalmanForecast <- function(mu.a,P.a,M,Q){
  mu.f = M%*%mu.a
  P.f  = Q + M*P.a*t(M)
  return(list(mu.f=mu.f,P.f=P.f))
}
