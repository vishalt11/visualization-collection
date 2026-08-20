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
  "maas_worldwide_ornith.csv",
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

df <- df %>% filter(accepted_rank == 'species')

sort(table(df$accepted_name))
sort(table(df$accepted_rank))

#-------------------------------------------------------------------------------
# Global 150 Ma reconstruction of the five target dinosaur genera

age <- 150
proj <- "ESRI:54009" # Mollweide, as in example.R
reconstruction_model <- "PALEOMAP" # Character model name uses the remote GPlates Web Service
target_genera <- c("Camarasaurus", "Allosaurus", "Apatosaurus", "Diplodocus", "Stegosaurus")

# accepted_name is mostly a species name (for example, "Allosaurus fragilis").
# Extract its first word so all species assigned to a target genus are retained.
dino_records <- df %>%
  mutate(genus = stringr::word(accepted_name, 1)) %>%
  filter(genus %in% target_genera) %>%
  filter(!is.na(lng), !is.na(lat)) %>%
  distinct(genus, lng, lat, .keep_all = TRUE)

missing_genera <- setdiff(target_genera, unique(dino_records$genus))
if (length(missing_genera) > 0) {
  stop("No records survived the age filter for: ", paste(missing_genera, collapse = ", "))
}

message("Unique fossil localities supplied to GPlates:")
print(dino_records %>% count(genus, name = "n_localities"))


#gmst <- chronosphere::fetch(src = "paleomap", ser = "gmst", ver = "scotese02a_v21321")
#pc <- chronosphere::fetch(src = "paleomap", ser = "paleocoastlines", ver = "7")

saveRDS(gmst, "paleomap_gmst_scotese02a_v21321.rds")
saveRDS(pc, "paleomap_paleocoastlines_v7.rds")

gmst <- readRDS("paleomap_gmst_scotese02a_v21321.rds")
pc <- readRDS("paleomap_paleocoastlines_v7.rds")

# Reconstruct each genus separately so its identity remains attached to the
# paleocoordinates returned by rgplates::reconstruct(). Do not use the PBDB
# paleolng/paleolat fields: those can represent a different reconstruction age.
reconstruct_one_genus <- function(x, genus_name, age, model) {
  reconstructed <- rgplates::reconstruct(as.data.frame(x[, c("lng", "lat")]), age = age, model = model) %>% as.data.frame()

  reconstructed$genus <- genus_name
  reconstructed
}

paleocoords <- dino_records %>%
  split(.$genus) %>%
  purrr::imap_dfr(~ reconstruct_one_genus(.x, .y, age = age, model = reconstruction_model))

n_failed <- sum(!stats::complete.cases(paleocoords[, c("paleolong", "paleolat")]))
if (n_failed > 0) {
  warning(n_failed, " localities could not be assigned by the plate model and were omitted.")
}

paleocoords <- paleocoords %>% filter(!is.na(paleolong), !is.na(paleolat))

dino_points <- sf::st_as_sf(paleocoords, coords = c("paleolong", "paleolat"), crs = "WGS84", remove = FALSE) %>% sf::st_transform(proj)

# Select and project the already-reconstructed background layers at the same age.
ctemp <- terra::resample(gmst[as.character(age)], terra::rast())
ccoast <- pc[as.character(age), "coast"]

moll_temp <- terra::project(ctemp, proj)
moll_coast <- sf::st_transform(ccoast, proj)

# Fetch one genus-specific silhouette for each dinosaur. The UUIDs are printed
# so a preferred silhouette can later be made reproducible by hard-coding it.
dino_uuid <- setNames(vapply(target_genera, function(x) rphylopic::get_uuid(name = x, n = 1), character(1)), target_genera)
print(dino_uuid)

# Save the image contributors/licenses next to the plot.
#attribution_file <- "kimmeridgian_dinosaurs_150Ma_phylopic_attribution.txt"
#attribution_text <- capture.output(rphylopic::get_attribution(uuid = unname(dino_uuid), text = TRUE, permalink = TRUE))
#writeLines(attribution_text, attribution_file)

# Fixed styling keeps points, labels, and silhouettes linked by color.
mako_colors <- viridis::viridis_pal(option = "mako")(6)
scales::show_col(mako_colors)


taxon_col <- c(Camarasaurus = "#0B0405FF", Allosaurus = "#3E356BFF", Apatosaurus = "#357BA2FF", Diplodocus = "#49C1ADFF", Stegosaurus = "#DEF5E5FF")
taxon_shape <- setNames(c(16, 17, 15, 18, 8), target_genera)
temp_col <- colorRampPalette(c("#33358A", "#76ACCE", "#FFF99A", "#E22C28", "#690720"))
dino_images <- purrr::map2(dino_uuid, unname(taxon_col[names(dino_uuid)]), ~ rphylopic::recolor_phylopic(rphylopic::get_phylopic(.x), fill = .y))
names(dino_images) <- names(dino_uuid)

# Convert the projected fossil points to a regular data frame for ggplot layers.
point_xy <- sf::st_coordinates(dino_points)
point_df <- sf::st_drop_geometry(dino_points) %>% mutate(x = point_xy[, 1], y = point_xy[, 2])

# Calculate a data-driven North America window around the reconstructed points.
# The multiplier responds to the fossil spread; the minimum dimensions stop a
# tightly clustered dataset from producing an excessively close crop.
point_x_range <- range(point_df$x, na.rm = TRUE)
point_y_range <- range(point_df$y, na.rm = TRUE)
map_center_x <- mean(point_x_range)
map_center_y <- mean(point_y_range)
map_width <- max(diff(point_x_range) * 5, 7000000)
map_height <- max(diff(point_y_range) * 5, 5000000)
target_aspect <- 1.4
if (map_width / map_height < target_aspect) map_width <- map_height * target_aspect
if (map_width / map_height > target_aspect) map_height <- map_width / target_aspect

x_limits <- map_center_x + c(-0.5, 0.5) * map_width
y_limits <- map_center_y + c(-0.5, 0.5) * map_height
plot_bbox <- sf::st_bbox(c(xmin = x_limits[1], ymin = y_limits[1], xmax = x_limits[2], ymax = y_limits[2]), crs = sf::st_crs(dino_points))

# Crop before converting the raster to a data frame; this keeps the ggplot layer
# small and avoids drawing the unused global raster.
temp_crop <- terra::crop(moll_temp, terra::ext(x_limits[1], x_limits[2], y_limits[1], y_limits[2]))
temp_df <- as.data.frame(temp_crop, xy = TRUE, na.rm = TRUE)
names(temp_df)[3] <- "temperature"
coast_crop <- suppressWarnings(sf::st_crop(moll_coast, plot_bbox))

# Stack the icon/name pairs vertically inside the left side of the map.
icon_df <- tibble(
  genus = target_genera,
  icon_x = x_limits[1] + 0.12 * map_width,
  icon_y = seq(
    y_limits[2] - 0.10 * map_height,
    y_limits[1] + 0.14 * map_height,
    length.out = length(target_genera)
  )
) %>%
  mutate(
    label_y = icon_y - 0.055 * map_height,
    img = dino_images[genus]
  )

plot_file <- "kimmeridgian_dinosaurs_150Ma_North_America.png"

p <- ggplot() +
  geom_raster(
    data = temp_df,
    aes(x = x, y = y, fill = temperature)
  ) +
  geom_sf(
    data = coast_crop,
    fill = "#00000030",
    color = "#55555580",
    linewidth = 0.25
  ) +
  geom_point(
    data = point_df,
    aes(x = x, y = y, color = genus),
    size = 1,
    alpha = 0.85,
    position = position_jitter(
      width = 30000,
      height = 30000,
      seed = 42
    )
  ) +
  rphylopic::geom_phylopic(
    data = icon_df,
    aes(x = icon_x, y = icon_y, img = img),
    height = 0.06 * map_height,
    color = "transparent",
    fill = "original",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = icon_df,
    aes(x = icon_x, y = label_y, label = genus, color = genus),
    fontface = "bold",
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_fill_gradientn(
    colors = temp_col(230),
    limits = c(-15, 37),
    oob = scales::squish,
    name = "Annual average air surface temperature (degrees C)"
  ) +
  scale_color_manual(values = taxon_col, guide = "none") +
  #scale_shape_manual(values = taxon_shape, guide = "none") +
  coord_sf(
    crs = sf::st_crs(dino_points),
    xlim = x_limits,
    ylim = y_limits,
    expand = FALSE,
    datum = NA
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(10, "cm"),
      barheight = grid::unit(0.35, "cm")
    )
  ) +
  labs(
    title = "Kimmeridgian dinosaurs in reconstructed North America",
    subtitle = paste0(
      "PALEOMAP remote reconstruction at ", age,
      " Ma | ", nrow(dino_points), " fossil localities"
    ),
    #caption = "Local fossil data: wusa_dino.csv | Silhouettes: PhyloPic"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      margin = margin(b = 3)
    ),
    plot.caption = element_text(
      size = 8,
      color = "gray35",
      hjust = 1,
      margin = margin(t = 6)
    ),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 12, 8, 12)
  )

p

ggsave(plot_file, plot = p, width = 12, height = 8.5, units = "in", dpi = 300, bg = "white")

message("Plot written to: ", normalizePath(plot_file, mustWork = FALSE))

