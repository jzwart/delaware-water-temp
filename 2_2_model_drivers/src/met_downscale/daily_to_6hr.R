daily_to_6hr <- function(forecasts, daily.forecast, debiased, VarNames){
  grouping = c("NOAA.member")
  if("fday.group" %in% colnames(forecasts)){
    grouping = append(grouping, "fday.group")
  }else{
    grouping = append(grouping, "date")
    forecasts <- forecasts %>% mutate(date = as_date(timestamp))
  }
  
  
  
  deviations <- full_join(daily.forecast, forecasts, by = grouping, suffix = c(".daily",".6hr"))
  devNames = NULL
  for(Var in 1:length(VarNames)){
    deviations[,paste0(VarNames[Var],".prop")] = deviations[,paste0(VarNames[Var], ".6hr")] / deviations[,paste0(VarNames[Var], ".daily")]
    devNames = append(devNames, paste0(VarNames[Var],".prop"))
    deviations[which(is.nan(unlist(deviations[,paste0(VarNames[Var],".prop")]))),paste0(VarNames[Var],".prop")] <- 0.0
  }
  deviations <- deviations %>% select(all_of(grouping), timestamp, all_of(devNames))
  
  redistributed <- inner_join(debiased, deviations, by = grouping)
  for(Var in 1:length(VarNames)){
    redistributed[,VarNames[Var]] = redistributed[,VarNames[Var]] * redistributed[,paste0(VarNames[Var], ".prop")]
  }
  
  redistributed <- redistributed %>% select(NOAA.member, timestamp, all_of(VarNames), dscale.member)
  
  return(redistributed)
}

