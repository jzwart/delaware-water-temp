


library(sf)
library(ggplot2)

d = sf::read_sf('1_geo_boundary/in/drbbnd/drb_bnd_polygon.shp')

m = sf::st_as_sf(maps::map(database = 'state', region = c('new york', 'new jersey', 'penn', 'delaware'), fill = T, plot = F))

ggplot() +
  geom_sf(data = m) +
  geom_sf(data = dplyr::select(d, geometry) , col ='blue', alpha = 0) +
  theme_bw()

