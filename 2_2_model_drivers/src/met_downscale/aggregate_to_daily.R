aggregate_to_daily <- function(data){
  grouping = NULL
  if("fday.group" %in% colnames(data)){
    grouping = append(grouping, "fday.group")
  }else{
    grouping = append(grouping, "date")
  }
  if("NOAA.member" %in% colnames(data)){
    grouping = append(grouping, "NOAA.member")
  }

  daily.data <- data %>%
    dplyr::mutate(date = date(timestamp)) %>%
    select(-timestamp) %>%
    group_by_at(grouping) %>%
    dplyr::summarize_all(funs(mean), na.rm = FALSE) %>%
    ungroup()
  if("fday" %in% colnames(daily.data)){
    daily.data <- daily.data %>% select(-fday)
  }
  return(daily.data)
}