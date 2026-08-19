library(tidyverse)


creta <- read.csv('../data/creta_ornithischia.csv', skip = 15)

creta %>% summarise(across(everything(), ~n_distinct(.)))

creta <- creta %>% select(identified_name, identified_rank, accepted_name, accepted_attr, 
                          accepted_rank, early_interval, late_interval, max_ma, min_ma, lng, lat, 
                          cc, state, county, altitude_value, altitude_unit, formation, stratgroup, 
                          lithology1, environment)


# combine accepted name with identified giving prio to former

str(creta)

species <- sapply(strsplit(creta[]$identified_name, " "), `[`, 1)
head(sort(table(species), decreasing=TRUE), 20)

head(sort(table(creta$identified_name), decreasing=TRUE), 20)

creta %>%
  filter(cc == 'IN')

creta$id <- seq_len(nrow(creta))

ggplot(creta[1:1000,], aes(y = factor(id))) +
  geom_segment(aes(x = min_ma, xend = max_ma, yend = factor(id))) +
  labs(x = "Years Ago", y = "Entry", title = "Time Ranges") +
  theme_minimal()

# Sample structure
df <- data.frame(
  age_group = rep(c("0-9", "10-19", "20-29", "30-39"), each = 2),
  gender = rep(c("Male", "Female"), times = 4),
  count = c(-600, -520, -580, -550, -570, -550, 100, 340)
)

ggplot(df, aes(x = age_group, y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 0.8) +
  #coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "Age Group", y = "Population") +
  theme_minimal()
