library(terra)
library(via)


# Settings ----------------------------------------------------------------

start_age <- 270
end_age <- 0
frames_per_second <- 1
resolution_factor <- 2  # 1 = 3600 x 1800; 2 = 1800 x 900

cache_dir <- "chronosphere_cache"
frame_dir <- file.path(cache_dir, "paleoatlas_animation_frames")
output_file <- "paleoatlas_270Ma_to_present.mp4"

dir.create(cache_dir, showWarnings = FALSE)
dir.create(frame_dir, showWarnings = FALSE, recursive = TRUE)

if (!requireNamespace("av", quietly = TRUE)) {
  stop("Install the 'av' package first: install.packages('av')")
}


# Fetch the PALEOMAP PaleoAtlas ------------------------------------------

paleoatlas <- chronosphere::fetch(
  src = "paleomap", ser = "paleoatlas", ver = "v3",
  res = 0.1, datadir = cache_dir
)

available_ages <- as.numeric(dimnames(paleoatlas)$age)
animation_ages <- available_ages[
  available_ages <= start_age & available_ages >= end_age
]
animation_ages <- sort(animation_ages, decreasing = TRUE)

if (length(animation_ages) == 0) {
  stop("No PaleoAtlas maps were found between ", start_age, " and ", end_age, " Ma.")
}


# Export one clean RGB image for each available age ----------------------

make_frame <- function(frame_age, frame_number) {
  frame_file <- file.path(
    frame_dir,
    sprintf("frame_%03d_%03dMa.png", frame_number, frame_age)
  )

  if (file.exists(frame_file)) return(frame_file)

  atlas_frame <- paleoatlas[as.character(frame_age), ]
  if (inherits(atlas_frame, "RasterArray")) atlas_frame <- atlas_frame@stack
  if (terra::nlyr(atlas_frame) < 3) stop("The ", frame_age, " Ma map does not contain three RGB layers.")

  atlas_frame <- atlas_frame[[1:3]]
  atlas_frame <- terra::flip(atlas_frame, direction = "vertical")

  if (resolution_factor > 1) {
    atlas_frame <- terra::aggregate(
      atlas_frame, fact = resolution_factor,
      fun = "mean", na.rm = TRUE
    )
  }

  terra::RGB(atlas_frame) <- 1:3

  terra::writeRaster(
    atlas_frame, frame_file, filetype = "PNG",
    datatype = "INT1U", overwrite = TRUE
  )

  frame_file
}

frame_files <- purrr::map2_chr(
  animation_ages, seq_along(animation_ages),
  make_frame
)


# Encode the map-only animation ------------------------------------------

av::av_encode_video(
  input = frame_files,
  output = output_file,
  framerate = frames_per_second,
  codec = "libx264",
  vfilter = "format=yuv420p",
  verbose = TRUE
)

message("Saved: ", normalizePath(output_file, winslash = "/", mustWork = FALSE))
