


get_subbasin_obs = function(subbasin,
                            obs_file = '3_observations/in/obs_temp_full.rds'){

  all_obs = readRDS(obs_file)

  subbasin_segs = unique(subbasin$seg_id_nat)

  subbasin_obs = dplyr::filter(all_obs, seg_id_nat %in% subbasin_segs)

  return(subbasin_obs)
}


