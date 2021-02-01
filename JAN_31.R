############################################
# 10 SEARCH FOR "ENO'S OBLIQUE STRATEGIES" #
# 20 OBTAIN ONE                            #
# 30 THAT IS YOUR PROMPT FOR TODAY         #
############################################
set.seed(31)

# http://stoney.sb.org/eno/oblique.html
# Emphasize repetitions

library(tidyverse)

knitting <- function(x_point, y_point, x_drop, y_drop, direction) {
  if(direction == "L") {
    data.frame(x = c(x_point - x_drop,
                     x_point,
                     x_point,
                     x_point - x_drop),
               y = c(y_point,
                     y_point - (1/3) * y_drop,
                     y_point - y_drop - (1/3) * y_drop,
                     y_point - y_drop),
               side = seq(1, 4))
  }
  else {
    data.frame(x = c(x_point - x_drop,
                     x_point,
                     x_point,
                     x_point - x_drop),
               y = c(y_point - (1/3) * y_drop,
                     y_point,
                     y_point - y_drop,
                     y_point - y_drop - (1/3) * y_drop),
               side = seq(1, 4)) 
  }
}

stranding <- function(x_point, y_point, x_drop, y_drop, direction) {
  if(direction == "L") {
    data.frame(x = x_point - x_drop,
               xend = x_point,
               y = seq(y_point, 
                       y_point - y_drop, 
                       length.out = 31),
               yend = seq(y_point - (1/3) * y_drop, 
                          y_point - y_drop - (1/3) * y_drop,
                          length.out = 31))
  }
  else {
    data.frame(x = x_point - x_drop,
               xend = x_point,
               y = seq(y_point - (1/3) * y_drop, 
                       y_point - y_drop - (1/3) * y_drop, 
                       length.out = 31),
               yend = seq(y_point, 
                          y_point - y_drop,
                          length.out = 31))
  }
}

df <- expand_grid(x_point = seq(1, 31),
                  y_point = seq(1, 31)) %>%
  mutate(stitch = row_number(),
         x_drop = .75,
         y_drop = 1,
         x_point = x_point * .75) %>%
  arrange(y_point, x_point) %>%
  mutate(x_point = if_else(y_point %% 2 == 0, 
                           max(x_point) - x_point + x_drop, x_point)) %>%
  mutate(direction = c('L', 'R')[(x_point * 1/.75) %% 2 + 1]) %>%
  mutate(color_value = arima.sim(n = 31 * 31,
                           model = list(ar = c((1 - 1/31), -1/31), 
                                        ma = c((1 - 1/31), -1/31)))) %>%
  mutate(color_value = 
           (color_value - min(color_value)) / 
           (max(color_value) - min(color_value)) *
           50 + 25) %>%
  mutate(color = hcl(h = 0, c = 0, l = color_value)) 

background <- df %>%
  rowwise() %>%
  mutate(knit = pmap(.l = list(x_point, y_point, x_drop, y_drop, direction),
                                 .f = knitting)) %>%
  unnest(knit)

strands <- df %>%
  rowwise() %>%
  mutate(knit = pmap(.l = list(x_point, y_point, x_drop, y_drop, direction),
                     .f = stranding)) %>%
  unnest(knit) %>%
  rowwise() %>%
  mutate(color = hcl(h = 0, c = 0, l = rnorm(1, color_value, 5)))

ggplot(data = background,
       aes(x, y,
           group = stitch,
           color = color,
           fill = color)) +
  geom_polygon() +
  geom_segment(data = strands,
               aes(xend = xend,
                   yend = yend),
               lineend = "round") +
  geom_polygon(color = alpha("#000000", .05),
               fill = NA,
               size = 2) +
  geom_polygon(color = alpha("#000000", .05),
               fill = NA,
               size = 1) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_equal() +
  theme_void()
ggsave("JAN_31.png",
       width = 5,
       height = 8,
       units = "in")