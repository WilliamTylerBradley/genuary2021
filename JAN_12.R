##################################
# Use an API (e.g. the weather). #
##################################
set.seed(12)

library(tidycensus)
library(tidyverse)
library(sf)
library(ggridges)

# vars <- load_variables(2017, "acs5", cache = TRUE)

counties <- get_acs(geography = "county", 
                    variables = "B01001_001",
                    geometry = TRUE)

coordinates <- counties %>%
  st_transform(2163) %>%
  st_centroid() %>%
  st_coordinates()

counties$x <- coordinates[, 1]  
counties$y <- coordinates[, 2]  

counties <- counties %>%
  mutate(state = substr(GEOID, 1, 2)) %>%
  mutate(y_max = max(y),
         y_min = min(y),
         estimate_max = max(estimate)) %>%
  mutate(y_scale = (y - y_min) / (y_max - y_min)) %>%
  group_by(state) %>%
  mutate(x_max = max(x),
         x_min = min(x),
         y_mean = mean(y_scale)) %>%
  mutate(x_end = x - x_max + ((x_max - x_min) / 2),
         y_end = y_mean) %>%
  mutate(height = (estimate / estimate_max))

counties_x <- counties %>%
  group_by(state) %>%
  filter(x_end == min(x_end) | x_end == max(x_end)) %>%
  mutate(height = 0,
         x_end = if_else(x_end == min(x_end), x_end - 1, x_end + 1))

counties <- rbind(counties, counties_x)

ggplot(data = counties,
       aes(x = x_end,
           y = y_end,
           height = height,
           group = state)) +
  geom_ridgeline() +
  theme_void()

ggsave("JAN_12.png",
       width = 8,
       height = 5,
       units = "in")
