library(tidyverse)
library(rgplates)
library(rphylopic)
library(sf)
library(terra)
library(ncdf4)
library(via)
library(png)



#coastlines <- reconstruct("coastlines", age=65, model="MERDITH2021")

# the edge of the map (for mollweide)
#edge <- mapedge()

# transform to Robinson 
#epsg <- "ESRI:54030"
#coastsRob <- sf::st_transform(coastlines, crs=epsg)
#edgeRob <- sf::st_transform(edge, crs=epsg)

# plot
#plot(edgeRob, col="#1A6BB0", border="gray30")
#plot(coastsRob, border=NA, col="gray90", add=TRUE)


df <- read_csv(
  "data/maas_worldwide_ornith.csv",
  col_select = c(
    accepted_name, accepted_rank, early_interval, late_interval, max_ma,
    min_ma, lng, lat, cc, state,
    county, paleolng, paleolat, occurrence_comments, geology_comments,
    environment
  )
)
colnames(df)

unique(df$early_interval)
unique(df$late_interval)

max(df$max_ma)
min(df$min_ma)
#-------------------------------------------------------------------------------

unique(df$early_interval)
unique(df$late_interval)


#df <- df %>% filter(early_interval == 'Kimmeridgian' | late_interval == 'Kimmeridgian')

max(df$max_ma)
min(df$min_ma)

df <- df[df$max_ma <= 73 & df$min_ma >= 66,]

sort(table(df$accepted_rank))

df <- df %>% filter(accepted_rank == 'family')

sort(table(df$accepted_name))

#mako_colors <- viridis::viridis_pal(option = "mako")(6)
mako_colors = c("#0C1F4b", "#B10DC9", "#85144b", "#FF4136", "#FF851B", "#F0E442")

scales::show_col(mako_colors)


# Global PaleoAtlas map at 70 Ma ------------------------------------------

age <- 70

target_families <- c(
  "Spheroolithidae", "Pachycephalosauridae", "Nodosauridae",
  "Ankylosauridae", "Ceratopsidae", "Hadrosauridae"
)

family_colors <- setNames(mako_colors, target_families)

fossil_records <- df %>%
  filter(accepted_name %in% target_families, !is.na(lng), !is.na(lat)) %>%
  transmute(family = factor(accepted_name, levels = target_families), lng, lat) %>%
  distinct(family, lng, lat)

missing_families <- setdiff(target_families, as.character(unique(fossil_records$family)))
if (length(missing_families) > 0) warning("No records found for: ", paste(missing_families, collapse = ", "))

reconstruct_one_family <- function(records, family_name, age) {
  reconstructed <- rgplates::reconstruct(
    as.data.frame(records[c("lng", "lat")]), age = age,
    model = "PALEOMAP"
  ) %>%
    as.data.frame()

  reconstructed$family <- family_name
  reconstructed
}

paleocoords <- fossil_records %>%
  split(.$family, drop = TRUE) %>%
  purrr::imap_dfr(~ reconstruct_one_family(.x, .y, age = age)) %>%
  mutate(family = factor(family, levels = target_families))

failed_reconstructions <- sum(!is.finite(paleocoords$paleolong) | !is.finite(paleocoords$paleolat))
if (failed_reconstructions > 0) warning(failed_reconstructions, " fossil localities could not be reconstructed and were omitted.")

paleocoords <- paleocoords %>%
  filter(is.finite(paleolong), is.finite(paleolat))


# Orthographic view settings ---------------------------------------------

view_lon <- -60
view_lat <- 30
image_size <- 1800

ortho_crs <- paste0(
  "+proj=ortho +lon_0=", view_lon,
  " +lat_0=", view_lat,
  " +datum=WGS84 +units=m +no_defs"
)


# Fetch and prepare the RGB PaleoAtlas image ------------------------------

atlas_cache <- "data/chronosphere_cache"
dir.create(atlas_cache, showWarnings = FALSE)

paleoatlas <- chronosphere::fetch(
  src = "paleomap", ser = "paleoatlas", ver = "v3",
  res = 0.1, datadir = atlas_cache
)

atlas_age <- paleoatlas[as.character(age), ]
if (inherits(atlas_age, "RasterArray")) atlas_age <- atlas_age@stack
# class(paleoatlas)
# dim(paleoatlas)
# dimnames(paleoatlas)
# 
# class(atlas_age)
# atlas_age
# terra::nlyr(atlas_age)
# names(atlas_age)
# terra::global(atlas_age, c("min", "max"), na.rm = TRUE)
# terra::has.RGB(atlas_age)

if (terra::nlyr(atlas_age) < 3) stop("The selected PaleoAtlas slice does not contain three RGB layers.")

atlas_age <- atlas_age[[1:3]]
atlas_age <- terra::flip(atlas_age, direction = "vertical")
terra::crs(atlas_age) <- "EPSG:4326"

earth_radius <- 6371000

ortho_template <- terra::rast(
  nrows = image_size, ncols = image_size,
  xmin = -earth_radius, xmax = earth_radius,
  ymin = -earth_radius, ymax = earth_radius,
  crs = ortho_crs
)

atlas_ortho <- terra::project(
  atlas_age, ortho_template,
  method = "bilinear", mask = FALSE
)

valid_cells <- terra::global(!is.na(atlas_ortho[[1]]), "sum", na.rm = TRUE)[1, 1]
if (valid_cells < 0.5 * terra::ncell(atlas_ortho)) {
  stop("The orthographic projection contains too few valid cells: ", valid_cells)
}

valid_globe <- !is.na(atlas_ortho[[1]])
atlas_rgb <- terra::ifel(is.na(atlas_ortho), 0, atlas_ortho)
atlas_rgba <- c(atlas_rgb, terra::ifel(valid_globe, 255, 0))
terra::RGB(atlas_rgba) <- 1:4
atlas_extent <- terra::ext(atlas_ortho)

# Export a transparent RGBA globe for efficient use in ggplot.
atlas_png <- file.path(
  atlas_cache,
  paste0("paleoatlas_ortho_rgba_", age, "Ma_lon", view_lon, "_lat", view_lat, ".png")
)

terra::writeRaster(
  atlas_rgba, atlas_png, filetype = "PNG",
  datatype = "INT1U", overwrite = TRUE
)

atlas_image <- png::readPNG(atlas_png, native = TRUE)


# Keep and project only points on the visible hemisphere -----------------

is_visible_ortho <- function(lon, lat, lon_0, lat_0) {
  lon <- lon * pi / 180
  lat <- lat * pi / 180
  lon_0 <- lon_0 * pi / 180
  lat_0 <- lat_0 * pi / 180

  sin(lat_0) * sin(lat) + cos(lat_0) * cos(lat) * cos(lon - lon_0) >= 0
}

paleocoords_visible <- paleocoords %>%
  filter(is_visible_ortho(paleolong, paleolat, view_lon, view_lat))

dino_points <- paleocoords_visible %>%
  sf::st_as_sf(coords = c("paleolong", "paleolat"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(crs = ortho_crs)

point_coordinates <- sf::st_coordinates(dino_points)

point_df <- bind_cols(
  sf::st_drop_geometry(dino_points),
  tibble(x = point_coordinates[, 1], y = point_coordinates[, 2])
)

# Fetch and color one PhyloPic silhouette per family ---------------------

get_family_uuid <- function(family_name) {
  uuid <- tryCatch(
    rphylopic::get_uuid(name = family_name, n = 1),
    error = function(e) NA_character_
  )

  if (length(uuid) == 0 || is.na(uuid[1])) {
    warning("No exact PhyloPic found for ", family_name, "; using Dinosauria.")
    uuid <- rphylopic::get_uuid(name = "Dinosauria", n = 1)
  }

  uuid[1]
}

family_uuid <- setNames(vapply(target_families, get_family_uuid, character(1)), target_families)

family_images <- purrr::map2(
  family_uuid, family_colors[names(family_uuid)],
  ~ rphylopic::recolor_phylopic(rphylopic::get_phylopic(.x), fill = .y)
)
names(family_images) <- target_families


# Arrange the silhouettes and names above the map panel -------------------

map_width <- atlas_extent$xmax - atlas_extent$xmin
map_height <- atlas_extent$ymax - atlas_extent$ymin
icon_height <- 0.05 * map_height

icon_df <- tibble(
  family = factor(target_families, levels = target_families),
  icon_x = seq(atlas_extent$xmin + 0.09 * map_width,
               atlas_extent$xmax - 0.09 * map_width,
               length.out = length(target_families)),
  icon_y = atlas_extent$ymax + 0.085 * map_height,
  label_x = icon_x,
  label_y = atlas_extent$ymax + 0.025 * map_height,
  img = unname(family_images[target_families])
)


# Plot --------------------------------------------------------------------

p <- ggplot() +
  annotation_raster(
    atlas_image, xmin = atlas_extent$xmin, xmax = atlas_extent$xmax,
    ymin = atlas_extent$ymin, ymax = atlas_extent$ymax, interpolate = TRUE
  ) +
  geom_point(
    data = point_df, aes(x = x, y = y, fill = family),
    shape = 21, 
    color = "firebrick2", 
    stroke = NA, 
    size = 1, 
    alpha = 0.75,
    position = position_jitter(width = 25000, height = 25000, seed = 42)
  ) +
  rphylopic::geom_phylopic(
    data = icon_df, aes(x = icon_x, y = icon_y, img = img),
    height = icon_height, color = "transparent", fill = "original",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = icon_df, aes(x = label_x, y = label_y, label = family, color = family),
    hjust = 0.5, fontface = "bold", size = 2.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = family_colors, guide = "none", drop = FALSE) +
  scale_fill_manual(values = family_colors, guide = "none", drop = FALSE) +
  coord_equal(
    xlim = c(atlas_extent$xmin, atlas_extent$xmax),
    ylim = c(atlas_extent$ymin, atlas_extent$ymax),
    expand = FALSE, clip = "off"
  ) +
  labs(
    title = "Maastrichtian Ornithischian Families",
    subtitle = paste0("PALEOMAP PaleoAtlas v3 at ", age, " Ma | ", nrow(point_df), " visible fossil sites"),
    #caption = "Local fossil data: maas_worldwide_ornith.csv | Basemap: PALEOMAP PaleoAtlas v3 | Silhouettes: PhyloPic"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 4, b = 72)),
    plot.caption = element_text(size = 8, color = "gray35", hjust = 1, margin = margin(t = 6)),
    plot.margin = margin(10, 12, 8, 12)
  )

p

ggsave(
  "paleoatlas_ornithischian_families_70Ma_orthographic_NA.png", p,
  width = 14, height = 7.5, units = "in", dpi = 600,
  bg = "grey80"
)


