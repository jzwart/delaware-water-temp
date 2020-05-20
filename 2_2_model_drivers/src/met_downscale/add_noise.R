# --------------------------------------
# purpose: create ensemble members of downscaling noise
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: creates ensemble members with noise addition (random sample from normal distribution with standard deviation equal to saved standard deviation of residuals from downscaling process.) For all variables except Shortwave, this is the standard deviaiton of the residuals after downscaling to the hourly resolution. For Shortwave, is is the value after downscaling to the daily resolution to artificually high values that result from noise in observational data from hour to hour.
# --------------------------------------

add_noise <- function(debiased, cov, n_ds_members, n_met_members, VarNames){

    with.noise <- debiased %>%
      group_by_all() %>%
      expand(dscale.member = 1:n_ds_members) %>%
      dplyr::mutate(ShortWaveOld = ShortWave,
                    RainOld = Rain) %>%
      ungroup() 

  # add option for covariance vs non covariance
  
  for(NOAA.ens in 1:n_met_members){
    for(dscale.ens in 1:n_ds_members){
      noise <- rmvnorm(1, mean = rep(0, length(VarNames)), sigma = cov)
      colnames(noise) <- colnames(cov)
      for(VarNum in 1:length(VarNames)){
        VarName <- VarNames[VarNum]
        with.noise[which(with.noise$NOAA.member == NOAA.ens & with.noise$dscale.member == dscale.ens),VarName] =
          with.noise[which(with.noise$NOAA.member == NOAA.ens & with.noise$dscale.member == dscale.ens),VarName] + noise[,VarName]
      }
    }
  }
  return(with.noise)
}