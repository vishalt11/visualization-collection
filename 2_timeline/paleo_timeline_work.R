# http://www.phytools.org/Cordoba2017/ex/2/Intro-to-phylogenies.html

# Load required libraries
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(stringdist)

# Create a data frame for Smilodon data
smilodon <- data.frame(
  species = c("Smilodon", "Homotherium", "Mammoth"),
  start = c(-100000,-400000,-50000),  
  end = c(-10000,-12000,-4000)
)

# Plot the duration
ggplot() +
  geom_segment(aes(x = -1000000, xend = 0, y = 0, yend = 0),size = 3, color = "black") +
  geom_rect(data = smilodon,
    aes(xmin = start, xmax = end, ymin = 0, ymax = 0.1, fill = species), alpha = 0.3) +
  #geom_segment(data = smilodon, aes(x = start, xend = end, y = 2, yend = 2),
  #  size = 2, color = "blue", alpha = 0.5) +
  geom_text_repel(
    data = smilodon, aes(x = (start + end) / 2, y = 0.1, label = species, color = species),
    fill = "white", size = 5, fontface = "bold",
    nudge_y = 0.1, box.padding = 0.5, point.padding = 0.5) +
  scale_x_continuous(
    expand = c(0,0),
    name = "Years Ago",
    limits = c(-1000000, 0),
    breaks = seq(-1000000, 0, by = 100000),
    labels = abs(seq(-1000000, 0, by = 100000))
  ) +
  scale_y_continuous(limits = c(-.1,0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank(), 
        axis.title.y = element_blank(),panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.3, 0.75, 0.3, 0.75), "inches"))
  labs(
    title = "Smilodon Existence Period", x = "Years Ago"
  )
  
library(ggridges)
  
data <- data.frame(
  value = c(c(-4,-5,-6),c(-5,-6,-7),c(-8,-9,-10)),
  group = factor(rep(c("Group A", "Group B", "Group C"), each = 3))
)

# Create the ridgeline plot
ggplot(data, aes(x = value, y = group, fill = group)) +
  geom_density_ridges(scale = 1.5, alpha = 0.7) +
  scale_fill_viridis_d() + # Optional: Nice color palette
  theme_minimal() +
  labs(
    title = "Ridgeline Plot Example",
    x = "Value",
    y = "Group",
    fill = "Group"
  )


#-------------------------------------------------------------------------------

paleodf <- read.csv('../data/cenozoic_mammals.csv')
paleodf$accepted_name <- tolower(paleodf$accepted_name)
#paleodf <- paleodf %>% select(accepted_name, max_ma, min_ma) %>% distinct()


cenozoic <- read.csv('../data/cenozoic_extinction.csv')
cenozoic$scientific <- tolower(cenozoic$scientific)
names(cenozoic)[names(cenozoic) == "scientific"] <- "accepted_name"


paleodf[grepl("smilodon", paleodf$accepted_name), ] %>%
  summarise(max_mya = max(max_ma), min_mya = min(min_ma))

#cenozoic <- merge(cenozoic, paleodf[, c("accepted_name", "max_ma", "min_ma")], by = "accepted_name", all.x = TRUE)


# Distance based partial matching, need to add logic for handling starting letter, whole words.
# find_partial_matches <- function(a, b, threshold) {
#   # Compute Levenshtein distances between all combinations of 'a' and 'b'
#   dist_matrix <- stringdist::stringdistmatrix(a, b, method = "lv")
# 
#   # Convert distances to similarities (max possible distance = max string length)
#   max_lengths <- outer(nchar(a), nchar(b), pmax)
#   sim_matrix <- 1 - (dist_matrix / max_lengths)
# 
#   # Find the best match for each element in 'a'
#   match_indices <- apply(sim_matrix, 1, function(row) {
#     best_match <- which.max(row) # Get the index of the highest similarity
#     if (row[best_match] >= threshold) return(best_match) else return(NA)
#   })
# 
#   return(match_indices)
# }
# 
# # Find partial matches between df_a$animal and df_b$animal
# matches <- find_partial_matches(cenozoic$accepted_name, paleodf$accepted_name, threshold = 0.6)

animal_a <- sapply(strsplit(cenozoic$accepted_name, " "), `[`, 1)

match_indices <- sapply(cenozoic$accepted_name, function(x) {
  index <- grep(x, paleodf$accepted_name)
  if (length(index) > 0){
    #print(index)
    maxV <- max(paleodf[index,]$max_ma)
    minV <- min(paleodf[index,]$min_ma)
    return(c(index[1], maxV, minV))
  } else {
    return(c(NA, NA, NA))
  }
})

match_indices <- as.data.frame(t(match_indices))

cenozoic$max_ma <- match_indices$V2
cenozoic$min_ma <- match_indices$V3
cenozoic$similar_name <- paleodf$accepted_name[match_indices$V1]

cenozoic <- cenozoic[complete.cases(cenozoic), ]
cenozoic[cenozoic$min_ma == 0.0000,]$min_ma <- 0.0117

write.csv(cenozoic, '../data/cenozoic_extinction_v2.csv', row.names = FALSE)

#-------------------------------------------------------------------------------

# Example data
data <- data.frame(
  group = factor(c("smilodon", "wo", "hom", "megal", "toxo", "arge")),
  x_start = c(-2, -8, -14, -10, -9, -8),
  x_end = c(-6, -12, -18, -15, -20, -30)
)

# Add x position and width for rectangles
data$x = (data$x_start + data$x_end) / 2  # Midpoint of each rectangle
data$width = data$x_end - data$x_start    # Width of each rectangle
data$y_position = as.numeric(data$group)/6 + 0.125  # Shift rectangles up by half height

data <- data %>%
  arrange(desc(group))

# Create the plot with top-aligned rectangles
ggplot(data, aes(x = x, y = y_position)) +
  geom_segment(aes(x = -40, xend = 0, y = min(data$y_position)/2, yend = min(data$y_position)/2),size = 3, color = "black") +
  geom_tile(aes(width = width, fill = group), height = 0.25, colour = "black", alpha = 0.9) +
  geom_text_repel(
    aes(label = group, colour = group),
    nudge_y = 2,
    nudge_x = -6, # Move the text slightly above the rectangles
    size = 5,
    box.padding = 0.5,
    point.padding = 0.3,
    max.overlaps = Inf
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank())+
  labs(
    title = "Ridgeline Plot with Top-Aligned Rectangles",
    x = "X Value",
    y = "Group") +
  scale_y_continuous(breaks = 1:length(unique(data$group)), labels = levels(data$group)) +
  theme(legend.position = "none")
  

# Define Cenozoic epochs with their time spans (start and end in millions of years)
epochs <- data.frame(
  Epoch = c("Holocene", "Pleistocene", "Pliocene", "Miocene",
            "Oligocene", "Eocene", "Paleocene"),
  Start = c(0.01, 2.58, 5.33, 23.03, 33.9, 56, 66),
  End = c(0, 0.01, 2.58, 5.33, 23.03, 33.9, 56),
  Color = c("#f6d55c", "#ed553b", "#3caea3", "#20639b", "#173f5f", "#f79d84", "#c70039")
)

# Add width of each epoch based on its time span
epochs <- epochs %>%
  mutate(Width = Start - End)

# Plot using ggplot2
ggplot(epochs, aes(x = Epoch, y = 1, fill = Color, width = Width)) +
  geom_tile(color = "black", aes(height = 0.5)) +  # Use height for better proportions
  scale_fill_identity() +  # Use colors as-is
  scale_x_discrete(limits = rev(epochs$Epoch)) +  # Reverse order for chronological order
  labs(
    title = "Cenozoic Epochs with Time Spans",
    x = "Epoch",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
