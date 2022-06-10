#Make a cool map of the for this paper that iluustrates the variation in snowiness across the short topography

library(tidyverse)
library(sf)
library(ggplot2)
library(mapview)
library(tmap)
library(spData)
library(terra)
library(rasterVis)
library(rgl)
library(terrainr)
library(tmaptools)
library(rgdal)
library(rayshader)

dem_2m <- rast("data/nwt_dtm_02 copy.tif")
plot(dem_2m)

#location_of_interest <- tmaptools::geocode_OSM("Niwot Ridge")$coords

#location_of_interest <- data.frame(
#  x = location_of_interest[["x"]],
#  y = location_of_interest[["y"]]
#)

plot_points <- read_csv("data/nwt_sdl_locations copy.csv") %>%
    filter(Site_code %in% 1:900) %>%  # saddle plots only
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = F) %>% 
  st_transform(crs = st_crs(dem_2m)) %>% 
  sf::st_buffer(dist = 100)
plot_points


 output_files <- get_tiles(plot_points,
                            output_prefix = tempfile(),
                            services = c("elevation", "ortho"))
output_files

raster::plot(raster::raster(output_files[[1]][1]))
raster::plotRGB(raster::brick(output_files[[2]][1]), scale = 1)

plot_points <- read_csv("data/nwt_sdl_locations copy.csv") %>%
    filter(Site_code %in% 1:900) %>%  # saddle plots only
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = F) %>% 
  st_transform(crs = st_crs(dem_2m)) %>% 
plot_points

ggplot() + 
  geom_spatial_rgb(data = output_files[[2]][1],
                   aes(x = x, y = y, r = red, g = green, b = blue)) +
  #geom_sf(data = plot_points)+
  theme_classic()

