##########################################################
# #264653 #2a9d8f #e9c46a #f4a261 #e76f51, no gradients. #
# Optionally, you can use a black or white background.   #
##########################################################
set.seed(23)

library(ggplot2)
library(tidyverse)

shape_1 <- data.frame(x_start = c(-1, -1, 0, 1, 1, 0, -1,
                            -1, -1, 0, 1, 1, 0, -1) * 1.5,
                      y_start = c(0, .25, .75, .25, 0, .5, 0,
                            -.5, -.25, .25, -.25, -.5, 0, -.5) * 1.5,
                      subgroup = 1,
                      color = "#f4a261")

shape_2 <- data.frame(x_start = c(-sqrt(3)/2 + .25 * cos(seq(3*pi/2, 5*pi/6, length.out = 10)),
                            0 + .25 * cos(seq(5*pi/6, pi/6, length.out = 10)),
                            sqrt(3)/2 + .25 * cos(seq(pi/6, -pi/2, length.out = 10))) *.5,
                      y_start = c(-.5 + .25 * sin(seq(3*pi/2, 5*pi/6, length.out = 10)),
                            1 + .25 * sin(seq(5*pi/6, pi/6, length.out = 10)),
                            -.5 + .25 * sin(seq(pi/6, -pi/2, length.out = 10))) * .5,
                      subgroup = 1,
                      color = "#f4a261")

shape_3 <- data.frame(x_start = c(-2 + cos(seq(3*pi/2, pi/2, length.out = 25)),
                            seq(-1.5, -.5, length.out = 5),
                            seq(0, 2.25, length.out = 10),
                            2.4,
                            seq(2.5, 0, length.out = 10),
                            seq(-.5, -1, length.out = 5)),
                      y_start = c(sin(seq(3*pi/2, pi/2, length.out = 25)),
                            -.25/(.5^2)*(seq(-1.5, -.5, length.out = 5) + 1)^2+1.25,
                            1/(2.5^2) * (seq(0, 2.25, length.out = 10))^2 + 1,
                            1.9,
                            2.75/(2.5^2) * (seq(2.5, 0, length.out = 10))^2 - 1,
                            -.15/(.25^2) * (seq(-.5, -1, length.out = 5) + .75)^2 -.85),
                      subgroup = 1,
                      color = "#f4a261")

shape_3b <- data.frame(x_start = c(-2 + .75 * cos(seq(3*pi/2, pi/2, length.out = 25)),
                             seq(-1.5, -.5, length.out = 5),
                             seq(0, 2.25, length.out = 10),
                             seq(2.5, 0, length.out = 10)[2:10],
                             seq(-.5, -1, length.out = 5)),
                       y_start = c(.75 * sin(seq(3*pi/2, pi/2, length.out = 25)),
                             -.25/(.5^2)*(seq(-1.5, -.5, length.out = 5) + 1)^2 + 1,
                             1/(2.5^2) * (seq(0, 2.25, length.out = 10))^2 + .75,
                             2.75/(2.5^2) * (seq(2.5, 0, length.out = 10)[2:10])^2 - .75,
                             -.15/(.25^2) * (seq(-.5, -1, length.out = 5) + .75)^2 -.6),
                       subgroup = 2,
                       color = "#f4a261")
# should have written a function to get parabola values

shape_3 <- rbind(shape_3, shape_3b)
rm(shape_3b)

shape_1_prime <- 11
shape_1_set <- tibble(x_center = seq(1, 23*2, shape_1_prime),
                      y_center = seq(1, 23*2, shape_1_prime)) %>%
  expand(x_center, y_center) %>%
  mutate(x_center = x_center + runif(n(), 0, shape_1_prime),
         y_center = y_center + runif(n(), 0, shape_1_prime),
         rotate = runif(n(), 0, 2*pi)) %>%
  mutate(group = seq(1, n()) + (1 * 100),
         shape = list(shape_1)) %>%
  unnest(cols = c(shape)) %>%
  mutate(x = x_center + (x_start * cos(rotate) - y_start * sin(rotate)),
         y = y_center + (y_start * cos(rotate) + x_start * sin(rotate)))

shape_2_prime <- 7
shape_2_set <- tibble(x_center = seq(1, 23*2, shape_2_prime),
                      y_center = seq(1, 23*2, shape_2_prime)) %>%
  expand(x_center, y_center) %>%
  mutate(x_center = x_center + runif(n(), 0, shape_2_prime),
         y_center = y_center + runif(n(), 0, shape_2_prime),
         rotate = runif(n(), 0, 2*pi)) %>%
  mutate(group = seq(1, n()) + (2 * 100),
         shape = list(shape_2)) %>%
  unnest(cols = c(shape)) %>%
  mutate(x = x_center + (x_start * cos(rotate) - y_start * sin(rotate)),
         y = y_center + (y_start * cos(rotate) + x_start * sin(rotate)))

shape_3_prime <- 17
shape_3_set <- tibble(x_center = seq(1, 23*2, shape_3_prime),
                      y_center = seq(1, 23*2, shape_3_prime)) %>%
  expand(x_center, y_center) %>%
  mutate(x_center = x_center + runif(n(), 0, shape_3_prime),
         y_center = y_center + runif(n(), 0, shape_3_prime),
         rotate = runif(n(), 0, 2*pi)) %>%
  mutate(group = seq(1, n()) + (3 * 100),
         shape = list(shape_3)) %>%
  unnest(cols = c(shape)) %>%
  mutate(x = x_center + (x_start * cos(rotate) - y_start * sin(rotate)),
         y = y_center + (y_start * cos(rotate) + x_start * sin(rotate)))

shape_set <- bind_rows(shape_1_set,
                       shape_2_set,
                       shape_3_set)

background_shape <- tibble(x = c( 2.5*sin(.5*seq(-1, 60, by = .01)) + (60/3),
                                  rev(2.5*sin(-.5*seq(-1, 60, by = .01)) + (60*2/3))),
                           y = c(seq(-1, 60, by = .01),
                                 rev(seq(-1, 60, by = .01))),
                           color = "#2a9d8f",
                           group = 1,
                           subgroup = 1)


ggplot(data = background_shape,
       aes(x = x,
           y = y,
           fill = color)) +
  geom_polygon() +
  scale_fill_identity() +
  coord_equal()

ggplot(data = shape_set,
       aes(x = x,
           y = y,
           fill = color,
           group = group,
           subgroup = subgroup)) +
  geom_polygon(data = background_shape) +
  scale_fill_identity() +
  geom_polygon() +
  theme_void() + 
  theme(panel.background = element_rect(fill = "#264653", 
                                        color = "#264653")) +
  coord_cartesian(xlim = c(10, 50),
                  ylim = c(10, 50))
ggsave("JAN_23.png",
       width = 8,
       height = 8,
       units = "in")
