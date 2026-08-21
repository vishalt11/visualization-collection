library(tidyverse)
library(rgplates)
library(rphylopic)
library(sf)
library(terra)
library(via)
library(png)


# Fossil data -------------------------------------------------------------

df <- read_csv(
  "maas_worldwide_ornith.csv",
  col_select = c(
    accepted_name, accepted_rank, early_interval, late_interval, max_ma,
    min_ma, lng, lat, cc, state,
    county, paleolng, paleolat, occurrence_comments, geology_comments,
    environment
  )
)

df <- df %>%
  filter(max_ma <= 73, min_ma >= 66, accepted_rank == "family")

age <- 70

target_families <- c(
  "Spheroolithidae", "Pachycephalosauridae", "Nodosauridae",
  "Ankylosauridae", "Ceratopsidae", "Hadrosauridae"
)

mako_colors <- c(
  "#0C1F4B", "#B10DC9", "#85144B",
  "#FF4136", "#FF851B", "#F0E442"
)

family_colors <- setNames(mako_colors, target_families)

fossil_records <- df %>%
  filter(accepted_name %in% target_families, !is.na(lng), !is.na(lat)) %>%
  transmute(family = factor(accepted_name, levels = target_families), lng, lat) %>%
  distinct(family, lng, lat)


# Reconstruct fossil coordinates at 70 Ma --------------------------------

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


# Fetch and project the PALEOMAP paleoDEM --------------------------------

dem_cache <- "chronosphere_dem_cache"
dir.create(dem_cache, showWarnings = FALSE)

paleodems <- chronosphere::fetch(
  src = "paleomap", ser = "dem", ver = "v24221",
  res = 0.1, datadir = dem_cache
)

dem_age <- paleodems[as.character(age)]
if (inherits(dem_age, "RasterArray")) dem_age <- dem_age@stack
if (terra::nlyr(dem_age) != 1) dem_age <- dem_age[[1]]

terra::crs(dem_age) <- "EPSG:4326"

moll_crs <- paste(
  "+proj=moll +lon_0=0 +datum=WGS84",
  "+units=m +no_defs"
)

# Terrain hillshade -------------------------------------------------------

terrain_derivatives <- terra::terrain(
  dem_age, v = c("slope", "aspect"),
  unit = "radians", neighbors = 8
)

hillshade_ll <- terra::shade(
  terrain_derivatives$slope, terrain_derivatives$aspect,
  angle = 40, direction = 315, normalize = TRUE
) / 255

dem_moll <- terra::project(
  dem_age, moll_crs, method = "bilinear",
  res = 10000, mask = TRUE
)

hillshade <- terra::project(
  hillshade_ll, dem_moll,
  method = "bilinear", mask = TRUE
)

# Retain seafloor relief but soften its contrast relative to land.
hillshade_soft_water <- 0.5 + (hillshade - 0.5) * 0.65
hillshade_display <- terra::ifel(dem_moll < 0, hillshade_soft_water, hillshade)

terrain_breaks <- c(
  -12000, -6000, -4000, -2500, -1000, -200, 0,
  200, 800, 1600, 3000, 6000, 12000
)

terrain_colors <- c(
  "#06162F", "#09284A", "#0E4168", "#176386", "#2D8EAA", "#86C8D2",
  "#2F7355", "#68A05B", "#A7B565", "#C5A66A", "#9A6847", "#F0E8D5"
)

shade_colors <- grDevices::colorRampPalette(
  c("#00000099", "#00000000", "#FFFFFF26"),
  alpha = TRUE
)(256)


# Render the projected relief once for efficient use in ggplot -----------

relief_png <- file.path(dem_cache, paste0("paleodem_hillshade_mollweide_", age, "Ma.png"))

render_relief <- function(filename, dem, shade) {
  grDevices::png(filename, width = 3600, height = 1800, bg = "transparent")
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")

  terra::plot(
    dem, col = terrain_colors, breaks = terrain_breaks,
    axes = FALSE, legend = FALSE, colNA = NA,
    maxcell = terra::ncell(dem), mar = c(0, 0, 0, 0)
  )

  terra::plot(
    shade, col = shade_colors, breaks = seq(0, 1, length.out = 257),
    axes = FALSE, legend = FALSE, colNA = NA, add = TRUE,
    maxcell = terra::ncell(shade)
  )
}

if (!file.exists(relief_png)) render_relief(relief_png, dem_moll, hillshade_display)

relief_image <- png::readPNG(relief_png, native = TRUE)
dem_extent <- terra::ext(dem_moll)


# Transform reconstructed fossils to Mollweide ---------------------------

dino_points <- paleocoords %>%
  sf::st_as_sf(coords = c("paleolong", "paleolat"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(crs = moll_crs)

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


# Place silhouettes above the Mollweide map ------------------------------

map_width <- dem_extent$xmax - dem_extent$xmin
map_height <- dem_extent$ymax - dem_extent$ymin
icon_height <- 0.05 * map_height

icon_df <- tibble(
  family = factor(target_families, levels = target_families),
  icon_x = seq(dem_extent$xmin + 0.09 * map_width,
               dem_extent$xmax - 0.09 * map_width,
               length.out = length(target_families)),
  icon_y = dem_extent$ymax + 0.085 * map_height,
  label_x = icon_x,
  label_y = dem_extent$ymax + 0.025 * map_height,
  img = unname(family_images[target_families])
)


# Plot --------------------------------------------------------------------

p <- ggplot() +
  annotation_raster(
    relief_image, xmin = dem_extent$xmin, xmax = dem_extent$xmax,
    ymin = dem_extent$ymin, ymax = dem_extent$ymax,
    interpolate = TRUE
  ) +
  geom_point(
    data = point_df, aes(x = x, y = y, fill = family),
    shape = 21, color = "firebrick2", size = 1.2,
    stroke = 0.3, alpha = 0.85,
    position = position_jitter(width = 30000, height = 30000, seed = 42)
  ) +
  rphylopic::geom_phylopic(
    data = icon_df, aes(x = icon_x, y = icon_y, img = img),
    height = icon_height, color = "transparent", fill = "original",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = icon_df, aes(x = label_x, y = label_y, label = family, color = family),
    hjust = 0.5, fontface = "bold", size = 3.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = family_colors, guide = "none", drop = FALSE) +
  scale_fill_manual(values = family_colors, guide = "none", drop = FALSE) +
  coord_sf(
    crs = moll_crs,
    xlim = c(dem_extent$xmin, dem_extent$xmax),
    ylim = c(dem_extent$ymin, dem_extent$ymax),
    expand = FALSE, datum = NA, clip = "off"
  ) +
  labs(
    title = "Maastrichtian Ornithischian Families",
    subtitle = paste0("PALEOMAP paleoDEM hillshade at ", age, " Ma | ", nrow(point_df), " fossil sites"),
    #caption = "Local fossils: maas_worldwide_ornith.csv | Terrain and bathymetry: PALEOMAP paleoDEM v24221 | Silhouettes: PhyloPic"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 4, b = 76)),
    plot.caption = element_text(size = 8, color = "gray35", hjust = 1, margin = margin(t = 6)),
    plot.margin = margin(10, 12, 8, 12)
  )

p

ggsave(
  "paleodem_ornithischian_families_70Ma.png", p,
  width = 14, height = 8, units = "in", dpi = 600,
  bg = "white"
)
