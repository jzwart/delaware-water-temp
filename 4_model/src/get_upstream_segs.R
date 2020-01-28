


#' @param seg_id_nat segments of the network for which you want upstream segments
#'

get_upstream_segs = function(seg_id_nat,
                             param_file = 'delaware.control.param',
                             model_run_loc = '4_model/tmp',
                             model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp',
                             n_segments = 456){

  seg_id_nat = as.character(seg_id_nat)

  # use this to organize connect to seg_id_nat
  model_fabric = sf::read_sf(model_fabric_file)

  model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                           model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  # tosegment is a parameter in the param file
  params = readLines(file.path(model_run_loc, 'control', param_file))

  param_loc_start = grep('tosegment_nhm', params) + 5
  param_loc_end = param_loc_start + n_segments - 1

  to_seg_id_nat_vec = params[param_loc_start:param_loc_end]

  # make data frame of network to use in igraph package
  network_map = model_locations %>%
    mutate(to_seg_id_nat = to_seg_id_nat_vec) %>%
    left_join(select(model_locations, seg_id_nat), by = c('to_seg_id_nat' = 'seg_id_nat')) %>%
    rename(from_seg_id_nat = seg_id_nat) %>%
    mutate(to_seg_id_nat = ifelse(to_seg_id_nat == 0 , NA, to_seg_id_nat)) %>%
    select(from_seg_id_nat, to_seg_id_nat)

  network_graph = igraph::graph_from_data_frame(network_map, directed = TRUE)

  # see https://github.com/robertness/lucy/blob/master/R/lucy.R for upstream /downstream functions
  # grab upstream segments of seg_id_nat supplied
  upstream <- igraph::shortest.paths(graph = network_graph,
                                     v = igraph::V(network_graph),
                                     to = seg_id_nat, mode = "out")

  # create named list of segments upstream
  out = sapply(colnames(upstream), function(seg){
    cur = names(upstream[!is.infinite(upstream[,seg]), seg])
  }, USE.NAMES = T)

  return(out)
}








