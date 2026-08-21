library(terra)
library(via)
library(ggplot2)
library(png)


# Settings ----------------------------------------------------------------

age <- 70
view_lon <- -60
view_lat <- 30
image_size <- 1800

cache_dir <- "data/chronosphere_cache"
output_file <- "paleoatlas_70Ma_orthographic_NorthAmerica.png"

dir.create(cache_dir, showWarnings = FALSE)


# Fetch and select the 70 Ma RGB raster ----------------------------------

paleoatlas <- chronosphere::fetch(
  src = "paleomap", ser = "paleoatlas", ver = "v3",
  res = 0.1, datadir = cache_dir
)

atlas_age <- paleoatlas[as.character(age), ]
if (inherits(atlas_age, "RasterArray")) atlas_age <- atlas_age@stack
if (terra::nlyr(atlas_age) < 3) stop("The selected map does not contain three RGB layers.")

atlas_age <- atlas_age[[1:3]]
atlas_age <- terra::flip(atlas_age, direction = "vertical")
terra::crs(atlas_age) <- "EPSG:4326"


# Project onto an explicit orthographic globe ----------------------------

ortho_crs <- paste0(
  "+proj=ortho +lon_0=", view_lon,
  " +lat_0=", view_lat,
  " +datum=WGS84 +units=m +no_defs"
)

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


# Convert the projected RGB raster to a transparent RGBA image -----------

valid_globe <- !is.na(atlas_ortho[[1]])
atlas_rgb <- terra::ifel(is.na(atlas_ortho), 0, atlas_ortho)
atlas_rgba <- c(atlas_rgb, terra::ifel(valid_globe, 255, 0))
terra::RGB(atlas_rgba) <- 1:4

rgba_file <- file.path(cache_dir, paste0("paleoatlas_ortho_rgba_", age, "Ma.png"))

terra::writeRaster(
  atlas_rgba, rgba_file, filetype = "PNG",
  datatype = "INT1U", overwrite = TRUE
)

atlas_image <- png::readPNG(rgba_file, native = TRUE)
atlas_extent <- terra::ext(atlas_ortho)


# Minimal ggplot of the orthographic raster ------------------------------

p <- ggplot() +
  annotation_raster(
    atlas_image, xmin = atlas_extent$xmin, xmax = atlas_extent$xmax,
    ymin = atlas_extent$ymin, ymax = atlas_extent$ymax,
    interpolate = TRUE
  ) +
  coord_equal(
    xlim = c(atlas_extent$xmin, atlas_extent$xmax),
    ylim = c(atlas_extent$ymin, atlas_extent$ymax),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

p

ggsave(
  output_file, p,
  width = 8, height = 8, units = "in",
  dpi = 300, bg = "white"
)
