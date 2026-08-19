library(tidyverse)
library(sf)
library(rayshader)
library(units)

kontour <- st_read("../data/kontur_population_IN_20231101.gpkg/kontur_population_IN_20231101.gpkg")
selection <- st_read("./selection.kml")

kontour <- st_transform(kontour, crs = 4326)

selection <- st_transform(selection, st_crs(kontour))
selected_h3s <- kontour[st_intersects(kontour, selection, sparse = FALSE), ]

sum(selected_h3s$population, na.rm = TRUE)

ggplot() +
  geom_sf(data = selection, fill = '#a5b5ac', color = "firebrick3", size = 1) +     # KML polygon outline
  geom_sf(data = selected_h3s, aes(fill = population), color = NA) +  # H3 hexagons
  scale_fill_viridis_c(option = "magma",  limits = c(0, 8000)) +
  theme_minimal() +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "#a4dee9")) +
  labs(title = "h3 hexagons within kml selection",
       fill = "Population")

ggsave('selection.png', dpi = 800, device = 'png')

max_h3 <- selected_h3s[which.max(selected_h3s$population),]
st_write(max_h3, "max_population_h3.geojson", delete_dsn = TRUE)

#-------------------------------------------------------------------------------
# test single hexagon
library(tmap)
tmap_mode("view")

tm_shape(max_h3) +
  tm_polygons(col = "population", border.col = "red", fill_alpha=0.4) +
  tm_basemap("Esri.WorldImagery") 
  #tm_basemap("OpenStreetMap")

#-------------------------------------------------------------------------------

tmap_mode("view")

# centre the view on the ROI
#ctr <- st_coordinates(st_centroid(st_union(selection)))
#pal <- viridisLite::magma(10)

tm <- tm_basemap("Esri.WorldImagery") +
  # all selected H3s
  tm_shape(selected_h3s) +
  tm_polygons(
    col = "population",
    #palette = pal,
    alpha = 0.3,
    border.col = NA,
    id = "h3",
    popup.vars = c(population = "population")
  ) +
  # ROI polygon
  tm_shape(selection) +
  tm_borders(col = "firebrick3", lwd = 2) +
  # highlight the max one
  tm_shape(max_h3) +
  tm_borders(col = "gold", lwd = 3) +
  tm_layout(title = "",
            legend.outside = TRUE)
tm

tmap_save(tm, "selection_tmap.html")
#-------------------------------------------------------------------------------
# check if centroid lat long of hexagon
h3r::cellToLatLng(max_h3$h3)
# is same as polygon from sf object
st_coordinates(st_centroid(max_h3))
#-------------------------------------------------------------------------------
h3r::getResolution('8864e6cdc1fffff')
mean(h3r::cellAreaKm2(selected_h3s$h3))


selection <- st_read("./selection.kml")
roi_proj <- st_transform(selection, crs = 32644)
area_m2 <- st_area(roi_proj)
area_km2 <- set_units(area_m2, "km^2")
print(area_km2)

#-------------------------------------------------------------------------------
p <- ggplot(kontour) +
  geom_sf(aes(fill = population), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void() +
  theme(legend.position = "none")

plot_gg(
  p,
  multicore = TRUE,
  width = 6,        # in inches
  height = 6,       # in inches
  scale = 300,      # controls spike height
  windowsize = c(800, 800),
  zoom = 0.6,
  phi = 45,
  theta = 45,
  preview = FALSE
)

render_snapshot(filename = "kontour_spike_map.png")
