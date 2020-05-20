repeat_6hr_to_hrly <- function(data.6hr){
  data.hrly <- data.6hr %>%
  group_by_all() %>%
  tidyr::expand(timestamp = c(timestamp  - 1*60*60,
                       timestamp  - 2*60*60,
                       timestamp  - 3*60*60,
                       timestamp  - 4*60*60,
                       timestamp  - 5*60*60,
                       timestamp  - 6*60*60)) %>%
  ungroup() %>%
  filter(timestamp >= min(data.6hr$timestamp)) %>%
  arrange(timestamp)
return(data.hrly)
}
