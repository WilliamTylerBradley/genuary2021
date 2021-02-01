#########################
# Triangle subdivision. #
#########################
set.seed(6)

library(dplyr)
library(tidyr)
library(colorspace)
library(ggplot2)
library(purrr)

change_color <- function(color, change) {
  if(change == -1) {
    darken(color)
  } else if(change == 0) {
    color
  } else {
    lighten(color)
  }
}

# Subdivide 
subdivide <- function(df, color_change, ...) {
  
  df_1 <- data.frame(x = c(df$x[1], 
                           (df$x[1] + df$x[2]) / 2, 
                           (df$x[1] + df$x[3]) / 2),
                     y = c(df$y[1], 
                           (df$y[1] + df$y[2]) / 2, 
                           (df$y[1] + df$y[3]) / 2),
                     id = df$id * 10 + 1,
                     color = df$color) %>%
    mutate(color = change_color(color, color_change[1]))
  
  df_2 <- data.frame(x = c((df$x[1] + df$x[2]) / 2,
                           df$x[2], 
                           (df$x[2] + df$x[3]) / 2),
                     y = c((df$y[1] + df$y[2]) / 2,
                           df$y[2], 
                           (df$y[2] + df$y[3]) / 2),
                     id = df$id * 10 + 2,
                     color = df$color) %>%
    mutate(color = change_color(color, color_change[2]))
  
  df_3 <- data.frame(x = c((df$x[1] + df$x[3]) / 2,
                           (df$x[2] + df$x[3]) / 2,
                           df$x[3]),
                     y = c((df$y[1] + df$y[3]) / 2,
                           (df$y[2] + df$y[3]) / 2,
                           df$y[3]),
                     id = df$id * 10 + 3,
                     color = df$color) %>%
    mutate(color = change_color(color, color_change[3]))
  
  df_4 <- data.frame(x = c((df$x[1] + df$x[2]) / 2,
                           (df$x[1] + df$x[3]) / 2,
                           (df$x[2] + df$x[3]) / 2),
                     y = c((df$y[1] + df$y[2]) / 2,
                           (df$y[1] + df$y[3]) / 2,
                           (df$y[2] + df$y[3]) / 2),
                     id = df$id * 10 + 4,
                     color = df$color) %>%
    mutate(color = change_color(color, color_change[4]))
  
  rbind(df_1, df_2, df_3, df_4)
}

# ----
color_change_full <- expand.grid(c(-1, 0, 1), c(-1, 0, 1), c(-1, 0, 1), c(-1, 0, 1))
color_change_sample <- color_change_full[sample(1:nrow(color_change_full), 6), ]

df_full <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df_full) <- c("x", "y", "id", "color", "hex")

for(i in 1:6){
  color_change <- color_change_sample[i, ]
  # Make hexagon ----
  theta <- seq(0, 300, by = 60) * pi/180
  
  center <- data.frame(x = rep(0, 6),
                       y = rep(0, 6),
                       id = seq(1, 6))
  
  df <- data.frame(x = cos(theta),
                   y = sin(theta)) %>%
    slice(c(seq(1, 6), seq(2, 6), 1)) %>%
    mutate(id = rep(seq(1, 6), 2)) %>%
    bind_rows(center) %>%
    mutate(color = hcl(h = (id - 1) * 60, c = 50, l = 75))
  
  df <- df %>%
    split(.$id) %>%
    map_dfr(~ subdivide(.x, color_change = color_change))
  
  df <- df %>%
    split(.$id) %>%
    map_dfr(~ subdivide(.x, color_change = color_change))
  
  df <- df %>%
    split(.$id) %>%
    map_dfr(~ subdivide(.x, color_change = color_change))
  
  df$hex <- i
  
  df_full <- rbind(df_full, df)
}
####----
ggplot(data = df_full,
       aes(x = x,
           y = y,
           group = id,
           fill = color)) +
  geom_polygon() +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_identity() +
  coord_equal() +
  facet_wrap(~ hex, nrow = 3) + 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

ggsave("JAN_06.png",
       width = 8,
       height = 8,
       units = "in")



