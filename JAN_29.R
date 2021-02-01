##############################
# Any shape, none can touch. #
##############################
set.seed(29)

library(tidyverse)
library(imager)
library(ggrepel)

r_text <- "R is a language and environment for statistical computing and graphics. It is a GNU project which is similar to the S language and environment which was developed at Bell Laboratories (formerly AT&T, now Lucent Technologies) by John Chambers and colleagues. R can be considered as a different implementation of S. There are some important differences, but much code written for S runs unaltered under R.

R provides a wide variety of statistical (linear and nonlinear modelling, classical statistical tests, time-series analysis, classification, clustering, …) and graphical techniques, and is highly extensible. The S language is often the vehicle of choice for research in statistical methodology, and R provides an Open Source route to participation in that activity.

One of R’s strengths is the ease with which well-designed publication-quality plots can be produced, including mathematical symbols and formulae where needed. Great care has been taken over the defaults for the minor design choices in graphics, but the user retains full control.

R is available as Free Software under the terms of the Free Software Foundation’s GNU General Public License in source code form. It compiles and runs on a wide variety of UNIX platforms and similar systems (including FreeBSD and Linux), Windows and MacOS."
r_text <- data.frame(words = unlist(strsplit(r_text, "\\s+"))) %>%
  mutate(id = row_number()) %>%
  rowwise() %>%
  mutate(characters = nchar(words)) %>%
  ungroup() %>%
  mutate(char_prop = cumsum(characters) / sum(characters))

pic <- load.image("pic_29.png")
pic <- rm.alpha(pic)
pic <- grayscale(pic)
pic <- as.data.frame(pic)

pic <- pic %>% 
  mutate(y = max(y) - y) %>%
  group_by(y) %>%
  mutate(value = if_else((lead(value, n = 1, order_by = x) == 0 |
                            lead(value, n = 2, order_by = x) == 0 |
                            lead(value, n = 3, order_by = x) == 0) &
                           (lag(value, n = 1, order_by = x) == 0 |
                              lag(value, n = 2, order_by = x) == 0 |
                              lag(value, n = 3, order_by = x) == 0),
                         0, value)) %>%
  group_by(x) %>%
  mutate(value = if_else((lead(value, n = 1, order_by = y) == 0 |
                            lead(value, n = 2, order_by = y) == 0 |
                            lead(value, n = 3, order_by = y) == 0) &
                           (lag(value, n = 1, order_by = y) == 0 |
                              lag(value, n = 2, order_by = y) == 0 |
                              lag(value, n = 3, order_by = y) == 0),
                         0, value))
  
y_rows <- 29

pic_summary <- pic %>%
  mutate(y_bin = ntile(y, y_rows),
         value = if_else(value > 0, 1, 0)) %>%
  group_by(y_bin) %>%
  summarise(x_sum = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(x_prop = cumsum(x_sum) / sum(x_sum))
  
r_text <- r_text %>%
  full_join(pic_summary, by = character()) %>%
  group_by(id) %>%
  filter(char_prop > lag(x_prop, default = 0, order_by = x_prop) & char_prop <= x_prop)

r_text <- r_text %>%
  group_by(y_bin) %>%
  mutate(x_prop_text = cumsum(characters) / sum(characters))

pic <- pic %>%
  mutate(y_bin = y_rows - ntile(y, y_rows)) %>%
  filter(!is.na(value) & value > 0) %>%
  group_by(y_bin) %>%
  arrange(x) %>%
  mutate(x_prop_pic = row_number() / n())

pic_text <- pic %>%
  full_join(r_text, by = c("y_bin")) %>%
  group_by(id) %>%
  filter(x_prop_text > lag(x_prop_pic, default = 0, order_by = x_prop_pic) & 
           x_prop_text <= x_prop_pic)

ggplot() +
  geom_label_repel(data = pic_text,
                  aes(x = x,
                      y = y,
                      label = words,
                      color = value),
                  point.size = NA,
                  min.segment.length = Inf,
                  size = 2,
                  max.overlaps = Inf) +
  theme_void() + 
  theme(legend.position = "none")
ggsave("JAN_29.png",
       width = 8,
       height = 8,
       units = "in")  

