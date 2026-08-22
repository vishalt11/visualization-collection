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

route_file <- "data/magellan_elcano_routes.geojson"

land_color <- "#fae1b0"
land_border_color <- "#edc48a"
water_color <- "#a4dee9"
route_color <- "#c92f2f"

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

voyage_routes <- sf::st_read(route_file, quiet = TRUE) %>%
  sf::st_wrap_dateline(
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
    quiet = TRUE
  ) %>%
  mutate(
    route_style = if_else(
      source_layer == "Alternative return",
      "Alternative return", "Expedition route"
    )
  )


# Geographic labels ------------------------------------------------------

continent_labels <- tribble(
  ~label,           ~lon, ~lat,
  "North America", -100,   40,
  "South America",  -58,  -8,
  "Europe",          18,   49,
  "Africa",          20,    7,
  "Asia",            80,   47,
  "Australia",      135,  -25,
  "Antarctica",       0,  -80
) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

ocean_labels <- tribble(
  ~label,           ~lon, ~lat,
  "Pacific Ocean", -145,    6,
  "Atlantic Ocean", -36,    12,
  "Indian Ocean",    80,  -22,
  "Arctic Ocean",      0,   76,
  "Southern Ocean",     0,  -59
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
    aes(linetype = route_style),
    color = route_color, linewidth = 0.6,
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
  scale_linetype_manual(
    values = c("Expedition route" = "solid", "Alternative return" = "22"),
    breaks = c("Expedition route", "Alternative return"),
    name = NULL
  ) +
  coord_sf(crs = robinson_crs, expand = FALSE, datum = sf::st_crs(4326)) +
  labs(
    title = "The First Circumnavigation of the World",
    subtitle = "Routes of the Magellan–Elcano expedition, 1519–1522",
    caption = paste(
      "Route data: Esri Magellan FeatureServer |",
      "Basemap: Natural Earth | Approximate historical routes"
    )
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        color = route_color, fill = NA,
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
    legend.position = "bottom",
    legend.text = element_text(family = "Prata-Regular", size = 14),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.key.width = grid::unit(2.2, "cm"),
    plot.margin = margin(12, 14, 9, 14)
  )

p

ggsave(
  "magellan_elcano_routes_map.png", p,
  width = 14, height = 8, units = "in",
  dpi = 600, bg = "grey90"
)

# ggsave(
#   "magellan_elcano_routes_map.svg", p,
#   width = 14, height = 8, units = "in",
#   device = svglite::svglite, bg = "grey90"
# )
