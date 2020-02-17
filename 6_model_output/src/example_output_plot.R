

require(sf)
require(ggplot2)
require(dplyr)
require(tidyr)

model_fabric = sf::read_sf('20191002_Delaware_streamtemp/GIS/Segments_subset.shp') %>%
  mutate(seg_id_nat = as.character(seg_id_nat),
         model_idx = as.character(model_idx))

model_output = read.csv('20191002_Delaware_streamtemp/output/seg_outflow.csv', stringsAsFactors = F) %>%
  as_tibble()

model_output_long = model_output %>%
  gather(key = 'model_idx', value = 'outflow', starts_with('X')) %>%
  mutate(model_idx = gsub('X', '', model_idx))

date_to_plot = '1983-06-05' # day of simulation to plot

model_output_sub = dplyr::filter(model_output_long, Date == date_to_plot) # filtering output to date

merged_data = dplyr::left_join(model_fabric, model_output_sub, by = 'model_idx')

windows()
ggplot() +
  geom_sf(data = merged_data, aes(color = log10(outflow)))+
  scale_color_viridis_c(direction = -1) +
  theme_minimal()



