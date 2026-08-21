library(paleobioDB)

canidae <- pbdb_occurrences(
  base_name = "canidae",
  interval = "Neogene",
  show = c("coords", "classext"),
  vocab = "pbdb",
  limit = "all"
)

dim(canidae)
pbdb_map(canidae)
pbdb_temp_range(canidae, rank = "genus")


pbdb_richness(canidae, rank = "species", temporal_extent = c(0, 25), res = 0.5)
