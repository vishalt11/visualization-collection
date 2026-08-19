library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(readr)
library(ggrepel)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# URL of the Wikipedia page
url <- "https://en.wikipedia.org/wiki/List_of_cities_in_Ukraine"

# Read the HTML
page <- read_html(url)

# Extract all tables with class 'wikitable'
tables <- page %>% html_nodes("table.wikitable")

# Inspect how many tables and view structure
length(tables)
cities_table <- tables[[1]] %>% html_table(fill = TRUE) %>% clean_names()
cities_table <- cities_table %>%
  mutate_all(~ str_trim(str_replace_all(., "\\[.*?\\]", "")))

cities_table <- cities_table %>%
  mutate(
    popu_lation_2022_esti_mates_1 = parse_number(`popu_lation_2022_esti_mates_1`),
    popu_lation_2001_census_7 = parse_number(`popu_lation_2001_census_7`),
    popu_lationchange = round(((popu_lation_2022_esti_mates_1 - popu_lation_2001_census_7) / popu_lation_2001_census_7) * 100, 2)
  )

df_20k <- cities_table %>%
  filter(popu_lation_2022_esti_mates_1 >= 20000)

df_coords <- df_20k %>%
  mutate(full_name = paste0(name, ", Ukraine")) %>%
  tidygeocoder::geocode(full_name, method = "arcgis")


ukraine_sf <- ne_countries(scale = "large", country = "Ukraine", returnclass = "sf")
rivers <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
rivers_ua <- st_crop(rivers, st_bbox(ukraine_sf))

df_sf <- df_coords %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

write.csv(df_sf, "df_sf.csv", row.names = FALSE)
write.csv(rivers_ua, "rivers_ua.csv", row.names = FALSE)
write.csv(df_coords, "df_coords.csv", row.names = FALSE)

plot_df <- df_sf %>%
  filter(administrative_division != 'Crimea', name != 'Sevastopol', popu_lation_2022_esti_mates_1 >=50000)

ggplot() +
  geom_sf(data = ukraine_sf, fill = "#fae1b0", color = "#edc48a", size = 0.3) +
  geom_sf(data = rivers_ua, color = "#a4dee9", linewidth = 0.8) +
  #geom_sf(data = plot_df, aes(size = popu_lation_2022_esti_mates_1, color = popu_lation_2022_esti_mates_1)) + 
  geom_sf(data = plot_df, aes(size = popu_lation_2022_esti_mates_1), color = 'firebrick3') + 
  geom_text_repel(
    data = plot_df %>% filter(popu_lation_2022_esti_mates_1 >=300000), 
    aes(geometry = geometry, label = name),
    stat = "sf_coordinates", size = 4, min.segment.length = 3, box.padding = 0.3, max.overlaps = 50) + 
  scale_size_continuous(range = c(2, 8)) +
  #scale_color_viridis_c(option = "inferno",direction = -1,end = 0.8,name = "pop",breaks = c(50000, 100000, 500000, 1000000, 2000000),labels = c("1st of February", "1st of March", "1st of April", "1st of May", "1st of June")) +
  theme_minimal() +
  labs(title = "Ukrainian Cities with Population = 50,000", size = "Population (2022)") +
  theme(panel.background = element_rect(fill = "#a4dee9"),
        panel.grid = element_blank(),
        legend.position = "none")


















