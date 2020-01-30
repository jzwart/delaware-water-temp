


get_calibration_order = function(subbasin_outlet_file){

  # read in subbasin outlets from yaml file
  subbasin_outlets = yaml::read_yaml(subbasin_outlet_file)$subbasin_outlets

  # find out which basins are nested in other basins - basins most upstream will be calibrated first,
  #   before moving downstream
  col_upstream_rows = is_b_upstream_of_a(seg_id_nat_a = subbasin_outlets, seg_id_nat_b = subbasin_outlets)

  cal_order = tibble(subbasin_outlet = subbasin_outlets, upstream_basins = NA, calibration_order = NA)
  for(cur_basin_outlet in subbasin_outlets){
    upstream_basin_outlets = colnames(col_upstream_rows)[which(col_upstream_rows[rownames(col_upstream_rows) ==
                                                                                   cur_basin_outlet])]

    upstream_basin_outlets = upstream_basin_outlets[upstream_basin_outlets != cur_basin_outlet]

    cal_order$upstream_basins[cal_order$subbasin_outlet == cur_basin_outlet] = length(upstream_basin_outlets)
  }

  # doesn't matter the order of calibration for headwater basins.
  cal_order = cal_order %>%
    arrange(upstream_basins) %>%
    mutate(calibration_order = seq(1:nrow(cal_order)))

  return(cal_order)
}
