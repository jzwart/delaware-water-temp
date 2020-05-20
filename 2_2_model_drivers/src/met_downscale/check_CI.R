check_CI <- function (df, obs.col.name, for.col.name){
  # df should be joined df after downscaling (with noise)
  # obs.col.name and for.col.name are the column names of the data to compare
  quant.df <- df %>% dplyr::group_by(timestamp) %>%
    dplyr::summarize(quant.0 = quantile(get(for.col.name), probs = 0.0, na.rm = TRUE),
                     quant.025 = quantile(get(for.col.name), probs = 0.025, na.rm = TRUE),
                     quant.05 = quantile(get(for.col.name), probs = 0.05, na.rm = TRUE),
                     quant.50 = quantile(get(for.col.name), probs = 0.50, na.rm = TRUE),
                     quant.95 = quantile(get(for.col.name), probs = 0.95, na.rm = TRUE),
                     quant.975 = quantile(get(for.col.name), probs = 0.975, na.rm = TRUE),
                     quant.100 = quantile(get(for.col.name), probs = 1, na.rm = TRUE)) %>%
    ungroup()
  
  pcnt.in.CI <- inner_join(quant.df, df, by = "timestamp") %>%
    mutate(check.100 = ifelse(get(obs.col.name) >= quant.0 & get(obs.col.name) <= quant.100,1,0),
           check.95 = ifelse(get(obs.col.name) >= quant.025 & get(obs.col.name) <= quant.975,1,0),
           check.90 = ifelse(get(obs.col.name) >= quant.05 & get(obs.col.name) <= quant.95,1,0)) %>%
    dplyr::summarise(check.100.pcnt = mean(check.100, na.rm = TRUE)*100,
                     check.95.pcnt = mean(check.95, na.rm = TRUE)*100,
                     check.90.pcnt = mean(check.90, na.rm = TRUE)*100)
  return(pcnt.in.CI )
}