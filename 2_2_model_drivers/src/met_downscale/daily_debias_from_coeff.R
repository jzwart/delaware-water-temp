daily_debias_from_coeff <- function(daily.forecast, 
                                    coeff.df, 
                                    VarInfo){
  # --------------------------------------
  # purpose: does linear debiasing from previously calculated coefficients
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  # @param: daily.forecast, dataframe of past forecasts at daily resolution
  # @param: coeff.df, the save coefficients for linear debaiasing for each meterological variable at daily resolution
if("fday.group" %in% colnames(daily.forecast)){
  grouping <- c("fday.group","NOAA.member")
}else{
  grouping <- c("date","NOAA.member")
}
  lin_mod <- function(col.for, coeff){
    intercept = coeff[1]
    slope = coeff[2]
    modeled = col.for*slope + intercept
    return(modeled)
  }

  debiased <- daily.forecast %>% select(all_of(grouping))
  for(Var in 1:length(VarInfo$VarNames)){
    VarName <- VarInfo$VarNames[Var]
    assign(VarName, value = as_tibble(lin_mod(daily.forecast[,VarName],
                                          coeff.df[,VarName])))
    debiased <- cbind(debiased, get(VarName))
  }
  
  return(debiased)
}

