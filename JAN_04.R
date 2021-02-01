############################
# Small areas of symmetry. #
############################
set.seed(4)

## Libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)

## Setup ----
size <- 100

h_lines <- expand_grid(x = seq(1, (size - 1)),
                y = seq(1, size)) %>%
  mutate(xend = x + 1,
         yend = y,
         direction = 'horizontal')
v_lines <- expand_grid(x = seq(1, size),
                       y = seq(1, (size - 1))) %>%
  mutate(xend = x,
         yend = y + 1,
         direction = "vertical")
s_lines <- expand_grid(x = seq(1, (size - 1)),
                       y = seq(1, (size - 1))) %>%
  mutate(xend = x + 1,
         yend = y + 1,
         direction = "slash")
b_lines <- expand_grid(x = seq(1, (size - 1)),
                       y = seq(2, size)) %>%
  mutate(xend = x + 1,
         yend = y - 1,
         direction = "backslash")

lines <- bind_rows(h_lines,
                   v_lines,
                   s_lines,
                   b_lines)

rm(h_lines,
   v_lines,
   s_lines,
   b_lines)

ggplot(data = lines,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_segment() +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()

## Pattern 1 ----
lines <- lines %>%
  mutate(pattern1 = if_else(direction == "backslash" &
                              x %% 2 == 0 &
                              y %% 2 == 0, 1, 0))

ggplot(data = lines,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_segment(aes(color = pattern1)) +
  scale_colour_gradientn(values = c(0, 1),
                     colors = c("white", "black")) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()

## Pattern 2 ----
lines <- lines %>%
  mutate(pattern2 = if_else(direction == 'horizontal' &
                              ((x %% 2 == 0 & y %% 2 == 0) | 
                                 (x %% 2 == 1 & y %% 2 == 1)), 1, 0)) %>%
  mutate(pattern2 = if_else(direction == 'vertical' &
                              ((x %% 2 == 1 & y %% 2 == 0) | 
                                 (x %% 2 == 0 & y %% 2 == 1)), 1, pattern2))

ggplot(data = lines,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_segment(aes(color = pattern2)) +
  scale_colour_gradientn(values = c(0, 1),
                         colors = c("white", "black")) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()

## Pattern 3 ----
lines <- lines %>%
  mutate(pattern3 = if_else(direction == 'horizontal' & x %% 2 == 1, 1, 0)) %>%
  mutate(pattern3 = if_else(direction == 'vertical' & y %% 2 == 1, 1, pattern3)) %>%
  mutate(pattern3 = if_else(direction == 'slash' & 
                              x %% 8 %in% c(2, 6) & 
                              y %% 8 %in% c(2, 6), 1, pattern3)) %>%
  mutate(pattern3 = if_else(direction == 'slash' & 
                              x %% 8 %in% c(4, 0) & 
                              y %% 8 %in% c(4, 0), 1, pattern3)) %>%
  mutate(pattern3 = if_else(direction == 'backslash' & 
                              x %% 8 %in% c(2, 6) & 
                              y %% 8 %in% c(1, 5), 1, pattern3)) %>%
  mutate(pattern3 = if_else(direction == 'backslash' & 
                              x %% 8 %in% c(4, 0) & 
                              y %% 8 %in% c(3, 7), 1, pattern3))
  
ggplot(data = lines,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_segment(aes(color = pattern3)) +
  scale_colour_gradientn(values = c(0, 1),
                         colors = c("white", "black")) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()



## Pattern 4 ----
lines <- lines %>%
  mutate(pattern4 = if_else(direction == 'horizontal' & 
                              y %% 2 == 1, 1, 0)) %>%
  mutate(pattern4 = if_else(direction == 'horizontal' & 
                              x %% 2 == 1 &
                              y %% 2 == 0, 1, pattern4)) %>%
  mutate(pattern4 = if_else(direction == 'vertical' & 
                              x %% 2 == 1 &
                              y %% 2 == 1, 1, pattern4)) %>%
  mutate(pattern4 = if_else(direction == 'vertical' & 
                              x %% 2 == 0 &
                              y %% 2 == 0, 1, pattern4))
ggplot(data = lines,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_segment(aes(color = pattern4)) +
  scale_colour_gradientn(values = c(0, 1),
                         colors = c("white", "black")) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()

## Pattern 5 ----
lines <- lines %>%
  mutate(pattern5 = if_else(direction == 'horizontal' & 
                              x %% 6 == 1 &
                              y %% 3 == 1, 1, 0)) %>%
  mutate(pattern5 = if_else(direction == 'slash' & 
                              x %% 6 == 2 &
                              y %% 3 == 1, 1, pattern5)) %>%
  mutate(pattern5 = if_else(direction == 'vertical' & 
                              x %% 6 == 3 &
                              y %% 3 == 2, 1, pattern5)) %>%
  mutate(pattern5 = if_else(direction == 'slash' & 
                              x %% 6 == 3 &
                              y %% 3 == 0, 1, pattern5)) %>%
  mutate(pattern5 = if_else(direction == 'horizontal' & 
                              x %% 6 == 4 &
                              y %% 3 == 1, 1, pattern5)) %>%
  mutate(pattern5 = if_else(direction == 'backslash' & 
                              x %% 6 == 5 &
                              y %% 3 == 1, 1, pattern5)) %>%
  mutate(pattern5 = if_else(direction == 'vertical' & 
                              x %% 6 == 0 &
                              y %% 3 == 2, 1, pattern5)) %>%
  mutate(pattern5 = if_else(direction == 'backslash' & 
                              x %% 6 == 0 &
                              y %% 3 == 2, 1, pattern5))
  
ggplot(data = lines,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_segment(aes(color = pattern5)) +
  scale_colour_gradientn(values = c(0, 1),
                         colors = c("white", "black")) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()

## Pattern 6 ----
lines <- lines %>%
  mutate(pattern6 = if_else(direction == 'slash' & 
                              x %% 2 == 1 &
                              y %% 6 == 1, 1, 0)) %>%
  mutate(pattern6 = if_else(direction == 'backslash' & 
                              x %% 2 == 1 &
                              y %% 6 == 5, 1, pattern6)) %>%
  mutate(pattern6 = if_else(direction == 'vertical' & 
                              x %% 2 == 0 &
                              y %% 6 %in% c(2, 3), 1, pattern6)) %>%
  mutate(pattern6 = if_else(direction == 'slash' & 
                              x %% 2 == 0 &
                              y %% 6 == 4, 1, pattern6)) %>%
  mutate(pattern6 = if_else(direction == 'backslash' & 
                              x %% 2 == 0 &
                              y %% 6 == 2, 1, pattern6)) %>%
  mutate(pattern6 = if_else(direction == 'vertical' & 
                              x %% 2 == 1 &
                              y %% 6 %in% c(5, 0), 1, pattern6))
ggplot(data = lines,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_segment(aes(color = pattern6)) +
  scale_colour_gradientn(values = c(0, 1),
                         colors = c("white", "black")) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()

## Pattern 7 ----
lines <- lines %>%
  mutate(pattern7 = if_else(direction == 'vertical' & 
                              x %% 8 == 1 &
                              y %% 8 %in% c(1, 0), 1, 0)) %>%
  mutate(pattern7 = if_else(direction == 'vertical' & 
                              x %% 8 == 5 &
                              y %% 8 %in% c(4, 5), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'vertical' & 
                              x %% 8 %in% c(4, 6) &
                              y %% 8 %in% c(3, 6), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'vertical' & 
                              x %% 8 %in% c(2, 0) &
                              y %% 8 %in% c(2, 7), 1, pattern7)) %>% ##
  mutate(pattern7 = if_else(direction == 'horizontal' & 
                              x %% 8 %in% c(1, 0) &
                              y %% 8 == 1, 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'horizontal' & 
                              x %% 8 %in% c(4, 5) &
                              y %% 8 == 5, 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'horizontal' & 
                              x %% 8 %in% c(3, 6) &
                              y %% 8 %in% c(4, 6), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'horizontal' & 
                              x %% 8 %in% c(2, 7) &
                              y %% 8 %in% c(2, 0), 1, pattern7)) %>% ##
  mutate(pattern7 = if_else(direction == 'slash' & 
                              x %% 8 %in% c(1, 4, 5, 0) &
                              y %% 8 %in% c(1, 4, 5, 0) & 
                              ((y %% 8) == (x %% 8)), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'slash' & 
                              ((x %% 8 %in% c(4, 6) & y %% 8 == 1) |
                                 (x %% 8 == 1 & y %% 8 %in% c(4, 6))), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'slash' & 
                              ((x %% 8 %in% c(3, 5) & y %% 8 == 0) |
                                 (x %% 8 == 0 & y %% 8 %in% c(3, 5))), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'slash' & 
                              ((x %% 8 %in% c(4, 0) & y %% 8 == 3) |
                                 (x %% 8 == 3 & y %% 8 %in% c(4, 0))), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'slash' & 
                              ((x %% 8 %in% c(6, 0) & y %% 8 == 5) |
                                 (x %% 8 == 5 & y %% 8 %in% c(6, 0))), 1, pattern7)) %>% ##
  mutate(pattern7 = if_else(direction == 'backslash' & 
                              x %% 8 == 1 &
                              y %% 8 %in% c(1, 4, 6), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'backslash' & 
                              x %% 8 == 3 &
                              y %% 8 %in% c(2, 6), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'backslash' & 
                              x %% 8 == 4 &
                              y %% 8 %in% c(6, 7, 1), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'backslash' & 
                              x %% 8 == 5 &
                              y %% 8 %in% c(2, 4, 5), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'backslash' & 
                              x %% 8 == 6 &
                              y %% 8 %in% c(5, 1), 1, pattern7)) %>%
  mutate(pattern7 = if_else(direction == 'backslash' & 
                              x %% 8 == 0 &
                              y %% 8 %in% c(2, 5, 7), 1, pattern7)) 
                              
ggplot(data = lines,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_segment(aes(color = pattern7)) +
  scale_colour_gradientn(values = c(0, 1),
                         colors = c("white", "black")) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()


## Noise-ish
lines <- lines %>%
  mutate(x_height = .25 * sin( x/ 8) + .25 * sin(x / 10) + .25 * sin(x / 22) + .25 * sin(x / 30),
         y_height = .25 * sin( y/ 4) + .25 * sin(y / 5) + .25 * sin(y / 50) + .25 * sin(y / 70),
         height = x_height * y_height) %>%
  mutate(height = ntile(height, 7))

ggplot(data = lines,
       aes(x = x,
           y = y,
           fill = height)) +
  geom_tile() +
  theme_void() + 
  coord_equal()

## Add together
lines <- lines %>%
  mutate(pattern = recode(height,
                          pattern1,
                          pattern2,
                          pattern3,
                          pattern4,
                          pattern5,
                          pattern6,
                          pattern7))

ggplot(data = lines,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_segment(aes(color = pattern)) +
  scale_colour_gradientn(values = c(0, 1),
                         colors = c("white", "black")) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()
ggsave("JAN_04.png",
       width = 8,
       height = 8,
       units = "in")





  
