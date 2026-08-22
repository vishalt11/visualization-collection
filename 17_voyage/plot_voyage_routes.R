library(tidyverse)
library(sf)
library(rnaturalearth)


# Local fonts -------------------------------------------------------------

font_dir <- if (dir.exists("data/OTF")) "data/OTF" else file.path("..", "data", "OTF")
if (!dir.exists(font_dir)) stop("Could not find the data/OTF font directory.")

sysfonts::font_add(
  family = "Girassol-Regular",
  regular = file.path(font_dir, "Girassol-Regular.ttf")
)

sysfonts::font_add(
  family = "Prata-Regular",
  regular = file.path(font_dir, "Prata-Regular.ttf")
)

sysfonts::font_add(
  family = "RobotoCondensed-LightItalic",
  regular = file.path(font_dir, "RobotoCondensed-LightItalic.ttf")
)

sysfonts::font_add(
  family = "RobotoCondensed-ExtraLightItalic",
  regular = file.path(font_dir, "RobotoCondensed-ExtraLightItalic.ttf")
)

showtext::showtext_auto()
showtext::showtext_opts(dpi = 400)


# Data and colors ---------------------------------------------------------

magellan_route_file <- "data/magellan_elcano_routes.geojson"
vasco_route_file <- "data/vasco_da_gama_1497_1498.geojson"
tlcmap_files <- list.files(
  "data", pattern = "^TLCMLayer_[0-9]+\\.json$",
  full.names = TRUE
)

land_color <- "#fae1b0"
land_border_color <- "#edc48a"
water_color <- "#a4dee9"

robinson_crs <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

edge_lon <- seq(-179.999, 179.999, length.out = 721)
edge_lat <- seq(-89.999, 89.999, length.out = 361)

globe_coordinates <- rbind(
  cbind(edge_lon, -89.999),
  cbind(179.999, edge_lat[-1]),
  cbind(rev(edge_lon[-length(edge_lon)]), 89.999),
  cbind(-179.999, rev(edge_lat[-c(1, length(edge_lat))])),
  c(-179.999, -89.999)
)

globe_outline <- sf::st_sfc(
  sf::st_polygon(list(globe_coordinates)),
  crs = 4326
) %>%
  sf::st_transform(robinson_crs)

polar_land <- land %>%
  filter(
    admin %in% c("Greenland", "Antarctica") |
      name_long %in% c("Greenland", "Antarctica")
  )

read_tlcmap_journey <- function(path) {
  layer_id <- stringr::str_extract(basename(path), "[0-9]+")
  metadata <- jsonlite::fromJSON(path, simplifyVector = FALSE)$metadata

  route_name <- dplyr::recode(
    layer_id,
    "2838" = "Flinders expedition (1801–1810)",
    "299" = "Cook’s third voyage (1776–1780)",
    "530" = "Cook’s first voyage (1768–1771)",
    "538" = "Cook’s second voyage — Resolution (1772–1775)",
    "539" = "Cook’s second voyage — Adventure (1772–1774)",
    .default = metadata$name
  )

  points <- sf::st_read(path, quiet = TRUE) %>%
    mutate(
      route_name = route_name,
      date_order = as.Date(datestart),
      segment = if (layer_id == "2838") as.character(name) else route_name
    )

  if (layer_id == "299") {
    points <- points %>%
      mutate(
        date_order = if_else(
          placename == "highest latitude 70 44",
          as.Date("1778-08-18"), date_order
        )
      )
  }

  points %>%
    arrange(segment, date_order, id) %>%
    group_by(route_name, segment) %>%
    filter(n() >= 2) %>%
    summarise(geometry = sf::st_combine(geometry), .groups = "drop") %>%
    sf::st_cast("LINESTRING")
}

magellan_routes <- sf::st_read(magellan_route_file, quiet = TRUE) %>%
  filter(source_layer != "Alternative return") %>%
  transmute(
    route_name = "Magellan–Elcano expedition (1519–1522)",
    geometry
  )

vasco_routes <- sf::st_read(vasco_route_file, quiet = TRUE) %>%
  transmute(
    route_name = "Vasco da Gama (1497–1498)",
    geometry
  )

tlcmap_routes <- purrr::map(tlcmap_files, read_tlcmap_journey) %>%
  bind_rows() %>%
  select(route_name, geometry)

all_voyage_routes <- bind_rows(
  vasco_routes,
  magellan_routes,
  tlcmap_routes
) %>%
  sf::st_wrap_dateline(
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
    quiet = TRUE
  )

route_levels <- c(
  "Vasco da Gama (1497–1498)",
  "Magellan–Elcano expedition (1519–1522)"
)

route_colors <- c(
  "Vasco da Gama (1497–1498)" = "#2463A5",
  "Magellan–Elcano expedition (1519–1522)" = "#C92F2F"
)

voyage_routes <- all_voyage_routes %>%
  filter(route_name %in% route_levels) %>%
  mutate(route_name = factor(route_name, levels = route_levels))


# Geographic labels ------------------------------------------------------

continent_labels <- tribble(
  ~label,           ~lon,~lat,
  "North America", -100,   40,
  "South America",  -58,   -8,
  "Europe",          18,   49,
  "Africa",          20,    7,
  "Asia",            80,   47,
  "Australia",      135,  -25,
  "Antarctica",       0,  -80
) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

ocean_labels <- tribble(
  ~label,           ~lon,~lat,
  "Pacific Ocean", -145,    6,
  "Atlantic Ocean", -36,   12,
  "Indian Ocean",    80,  -22,
  "Arctic Ocean",     0,   76,
  "Southern Ocean",   0,  -59
) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)


# Plot -------------------------------------------------------------------

p <- ggplot() +
  geom_sf(data = globe_outline, fill = water_color, color = NA) +
  geom_sf(
    data = land, fill = land_color, color = land_border_color,
    linewidth = 0.2
  ) +
  geom_sf(
    data = polar_land, fill = "white", color = "grey80",
    linewidth = 0.2
  ) +
  geom_sf(
    data = voyage_routes,
    aes(color = route_name),
    linewidth = 0.6,
    lineend = "round", alpha = 0.9,
    key_glyph = "path"
  ) +
  geom_sf_text(
    data = continent_labels, aes(label = label),
    family = "RobotoCondensed-LightItalic",
    color = "#8a6336",
    size = 7, check_overlap = TRUE
  ) +
  geom_sf_text(
    data = ocean_labels, aes(label = label),
    family = "RobotoCondensed-LightItalic",
    color = "#397d89",
    size = 5, check_overlap = TRUE
  ) +
  scale_color_manual(
    values = route_colors,
    breaks = route_levels,
    name = NULL
  ) +
  coord_sf(crs = robinson_crs, expand = FALSE, datum = sf::st_crs(4326)) +
  labs(
    title = "Voyages Across the Known World",
    subtitle = "Routes of Vasco da Gama and the Magellan–Elcano expedition, 1497–1522",
    caption = paste(
      "Route data: Esri FeatureServers |",
      "Basemap: Natural Earth | Approximate historical routes"
    )
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 1.2, alpha = 1
      )
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = scales::alpha("white", 0.45), linewidth = 0.25),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      family = "Girassol-Regular", size = 24, hjust = 0.5
    ),
    plot.subtitle = element_text(
      family = "Prata-Regular", size = 16,
      hjust = 0.5, margin = margin(t = 10, b = 10)
    ),
    plot.caption = element_text(
      family = "RobotoCondensed-ExtraLightItalic", size = 12,
      color = "grey40", hjust = 1, margin = margin(t = 7)
    ),
    legend.position = "inside",
    legend.position.inside = c(0.4, 0.2),
    legend.justification = c(0, 0),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.text = element_text(family = "Prata-Regular", size = 16),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.key.width = grid::unit(1.4, "cm"),
    legend.key.height = grid::unit(0.38, "cm"),
    plot.margin = margin(12, 14, 9, 14)
  )

ggsave(
  "voyage_routes.png", p,
  width = 14, height = 8, units = "in",
  dpi = 600, bg = "grey90"
)

# ggsave(
#   "voyage_routes.svg", p,
#   width = 14, height = 8, units = "in",
#   device = svglite::svglite, bg = "grey90"
# )
