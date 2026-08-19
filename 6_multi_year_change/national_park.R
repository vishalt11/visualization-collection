library(osmdata)
library(sf)
library(tidyverse)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)

bandipur_query <- opq(bbox = "Bandipur National Park, India") %>%
  add_osm_feature(key = "boundary", value = "national_park") %>%
  add_osm_feature(key = "name", value = "Bandipur National Park")

bandipur_data <- osmdata_sf(bandipur_query)

bandipur_polygon <- bandipur_data$osm_polygons

# View attributes (like name, wikidata, etc.)
print(names(bandipur_polygon))

# Plot the park boundary
plot(st_geometry(bandipur_polygon), col = "darkgreen", main = "BNP")

tmap_mode("view")

bandipur_simple <- st_make_valid(bandipur_polygon)
bandipur_simple <- st_union(bandipur_simple)

tm_shape(bandipur_simple) +
  tm_polygons(col = "forestgreen", border.col = "darkgreen", fill_alpha=0.4) +
  tm_basemap("Esri.WorldImagery") 



#-------------------------------------------------------------------------------

# Get bounding box for India
india_bbox <- getbb("India")

# Query all national parks in India
india_np_query <- opq(bbox = india_bbox) %>%
  add_osm_feature(key = "boundary", value = "national_park")


india_np_data <- osmdata_sf(india_np_query)

national_parks <- bind_rows(
  india_np_data$osm_polygons,
  india_np_data$osm_multipolygons
)

unique(national_parks$name)

# Keep only rows with non-missing names
national_parks <- national_parks %>% filter(!is.na(name))

india <- ne_countries(country = "India", returnclass = "sf")
national_parks <- st_transform(national_parks, st_crs(india))
parks_within_india <- national_parks[st_within(national_parks, india, sparse = FALSE)[,1], ]

# Make valid and union if needed
national_parks_simple <- parks_within_india %>%
  st_make_valid() %>%
  group_by(name) %>%
  summarise(geometry = st_union(geometry))

tmap_mode("view")

tm_shape(national_parks_simple) +
  tm_polygons(col = "red", border.col = "black", alpha = 0.3) +
  tm_basemap("Esri.WorldImagery")

