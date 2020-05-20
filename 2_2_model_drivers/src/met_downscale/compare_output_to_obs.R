compare_output_to_obs <- function(output, hrly.observations){
  if("dscale.member" %in% colnames(output) == FALSE){
    output = output %>% mutate(dscale.member = 0)
  }
  observations = hrly.observations %>%
    filter(timestamp >= min(output$timestamp) & timestamp <= max(output$timestamp)) %>% mutate(AirTemp = AirTemp - 273.15)
  
  joined = full_join(observations, output, by = "timestamp", suffix = c(".obs",".for"))
  
  mean.joined = joined %>%
    group_by(timestamp) %>%
    summarize_all("mean")
  
  time0 = min(output$timestamp)
  
  mean.joined.day.1 = mean.joined %>%
    filter(timestamp <= time0 + 24*60*60)
  
  ## make a summary table of comparison between output and observations
  summary.table = tibble(metric = c("AirTemp","RelHum","WindSpeed","ShortWave","LongWave", "Rain"),
                             r2.AllDays = rep(NA,6),
                             r2.FirstDay = rep(NA,6),
                             mean.residual = rep(NA,6),
                             CI.90 = rep(NA,6),
                             CI.95 = rep(NA,6),
                             CI.100 = rep(NA,6))
  
  formula = lm(mean.joined$AirTemp.obs ~ mean.joined$AirTemp.for)
  summary.table[1,2] = summary(lm(formula))$r.squared
  formula = lm(mean.joined.day.1$AirTemp.obs ~ mean.joined.day.1$AirTemp.for)
  summary.table[1,3] = summary(lm(formula))$r.squared
  summary.table[1,4] = mean(mean.joined$AirTemp.obs - mean.joined$AirTemp.for, na.rm = TRUE)
  summary.table[1,5] = check_CI(df = joined, obs.col.name = "AirTemp.obs", for.col.name = "AirTemp.for")$check.90.pcnt
  summary.table[1,6] = check_CI(df = joined, obs.col.name = "AirTemp.obs", for.col.name = "AirTemp.for")$check.95.pcnt
  summary.table[1,7] = check_CI(df = joined, obs.col.name = "AirTemp.obs", for.col.name = "AirTemp.for")$check.100.pcnt
  
  formula = lm(mean.joined$RelHum.obs ~ mean.joined$RelHum.for)
  summary.table[2,2] = summary(lm(formula))$r.squared
  formula = lm(mean.joined.day.1$RelHum.obs ~ mean.joined.day.1$RelHum.for)
  summary.table[2,3] = summary(lm(formula))$r.squared
  summary.table[2,4] = mean(mean.joined$RelHum.obs - mean.joined$RelHum.for, na.rm = TRUE)
  summary.table[2,5] = check_CI(df = joined, obs.col.name = "RelHum.obs", for.col.name = "RelHum.for")$check.90.pcnt
  summary.table[2,6] = check_CI(df = joined, obs.col.name = "RelHum.obs", for.col.name = "RelHum.for")$check.95.pcnt
  summary.table[2,7] = check_CI(df = joined, obs.col.name = "RelHum.obs", for.col.name = "RelHum.for")$check.100.pcnt
  
  formula = lm(mean.joined$WindSpeed.obs  ~ mean.joined$WindSpeed.for)
  summary.table[3,2] = summary(lm(formula))$r.squared
  formula = lm(mean.joined.day.1$WindSpeed.obs  ~ mean.joined.day.1$WindSpeed.for)
  summary.table[3,3] = summary(lm(formula))$r.squared
  summary.table[3,4] = mean(mean.joined$WindSpeed.obs -  mean.joined$WindSpeed.for, na.rm = TRUE)
  summary.table[3,5] = check_CI(df = joined, obs.col.name = "WindSpeed.obs", for.col.name = "WindSpeed.for")$check.90.pcnt
  summary.table[3,6] = check_CI(df = joined, obs.col.name = "WindSpeed.obs", for.col.name = "WindSpeed.for")$check.95.pcnt
  summary.table[3,7] = check_CI(df = joined, obs.col.name = "WindSpeed.obs", for.col.name = "WindSpeed.for")$check.100.pcnt
  
  formula = lm(mean.joined$ShortWave.obs ~ mean.joined$ShortWave.for)
  summary.table[4,2] = summary(lm(formula))$r.squared
  formula = lm(mean.joined.day.1$ShortWave.obs ~ mean.joined.day.1$ShortWave.for)
  summary.table[4,3] = summary(lm(formula))$r.squared
  summary.table[4,4] = mean(mean.joined$ShortWave.obs -  mean.joined$ShortWave.for, na.rm = TRUE)
  summary.table[4,5] = check_CI(df = joined, obs.col.name = "ShortWave.obs", for.col.name = "ShortWave.for")$check.90.pcnt
  summary.table[4,6] = check_CI(df = joined, obs.col.name = "ShortWave.obs", for.col.name = "ShortWave.for")$check.95.pcnt
  summary.table[4,7] = check_CI(df = joined, obs.col.name = "ShortWave.obs", for.col.name = "ShortWave.for")$check.100.pcnt
  
  formula = lm(mean.joined$LongWave.obs ~ mean.joined$LongWave.for)
  summary.table[5,2] = summary(lm(formula))$r.squared
  formula = lm(mean.joined.day.1$LongWave.obs ~ mean.joined.day.1$LongWave.for)
  summary.table[5,3] = summary(lm(formula))$r.squared
  summary.table[5,4] = mean(mean.joined$LongWave.obs - mean.joined$LongWave.for, na.rm = TRUE)
  summary.table[5,5] = check_CI(df = joined, obs.col.name = "LongWave.obs", for.col.name = "LongWave.for")$check.90.pcnt
  summary.table[5,6] = check_CI(df = joined, obs.col.name = "LongWave.obs", for.col.name = "LongWave.for")$check.95.pcnt
  summary.table[5,7] = check_CI(df = joined, obs.col.name = "LongWave.obs", for.col.name = "LongWave.for")$check.100.pcnt

  formula = lm(mean.joined$Rain.obs ~ mean.joined$Rain.for)
  summary.table[6,2] = summary(lm(formula))$r.squared
  formula = lm(mean.joined.day.1$Rain.obs ~ mean.joined.day.1$Rain.for)
  summary.table[6,3] = summary(lm(formula))$r.squared
  summary.table[6,4] = mean(mean.joined$Rain.obs - mean.joined$Rain.for, na.rm = TRUE)
  summary.table[6,5] = check_CI(df = joined, obs.col.name = "Rain.obs", for.col.name = "Rain.for")$check.90.pcnt
  summary.table[6,6] = check_CI(df = joined, obs.col.name = "Rain.obs", for.col.name = "Rain.for")$check.95.pcnt
  summary.table[6,7] = check_CI(df = joined, obs.col.name = "Rain.obs", for.col.name = "Rain.for")$check.100.pcnt
  print(summary.table)
  
  
  print(ggplot(data = joined, aes(x = timestamp)) +
          geom_line(aes(y = AirTemp.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
          geom_point(aes(y = AirTemp.obs, color = "Site Observations")) + 
          geom_line(aes(y = AirTemp.obs, color = "Site Observations")) + 
          ylab("Air Temperature (Degrees Celsius)")+
          xlab("")+
          theme_bw()+
          theme(text = element_text(size = 14)) +
          scale_color_manual(values = c("firebrick2","black")))
  
  print(ggplot(data = joined, aes(x = timestamp)) +
          geom_line(aes(y = ShortWave.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
          geom_line(aes(y = ShortWave.obs, color = "Site Observations")) + 
          ylab("Shortwave Radiation (W/m2)")+
          xlab("")+
          theme_bw()+
          theme(text = element_text(size = 14)) +
          scale_color_manual(values = c("firebrick2","black")))
  
  print(ggplot(data = joined, aes(x = timestamp)) +
          geom_line(aes(y = LongWave.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.4) +
          geom_line(aes(y = LongWave.obs, color = "Site Observations")) + 
          ylab("Longwave Radiation (W/m2)")+
          xlab("")+
          theme_bw()+
          theme(text = element_text(size = 14)) +
          scale_color_manual(values = c("firebrick2","black")))
  
  print(ggplot(data = joined, aes(x = timestamp)) +
          geom_line(aes(y = RelHum.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
          geom_point(aes(y = RelHum.obs, color = "Site Observations")) + 
          geom_line(aes(y = RelHum.obs, color = "Site Observations")) + 
          ylab("Relative Humidity (%)")+
          xlab("")+
          theme_bw()+
          theme(text = element_text(size = 14)) +
          scale_color_manual(values = c("firebrick2","black")))
  
  print(ggplot(data = joined, aes(x = timestamp)) +
          geom_line(aes(y = WindSpeed.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
          geom_point(aes(y = WindSpeed.obs, color = "Site Observations")) + 
          geom_line(aes(y = WindSpeed.obs, color = "Site Observations")) + 
          ylab("Wind Speed (m/s)")+
          xlab("")+
          theme_bw()+
          theme(text = element_text(size = 14)) +
          scale_color_manual(values = c("firebrick2","black")))
  
  print(ggplot(data = joined, aes(x = timestamp)) +
          geom_line(aes(y = Rain.for, color = "Downscaled", group = interaction(NOAA.member, dscale.member)), alpha = 0.3) +
          geom_line(aes(y = Rain.obs, color = "Site Observations")) + 
          ylab("Rain (m/day)")+
          xlab("")+
          theme_bw()+
          theme(text = element_text(size = 14)) +
          scale_color_manual(values = c("firebrick2","black")))
  
  return(summary.table)
}
