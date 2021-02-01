##############
# 500 lines. #
##############
set.seed(24)

library(png)
library(colorspace)
library(ggplot2)
library(dplyr)
library(tidyr)
library(TSP)

pic <- readPNG('pic_24.png')
dim_pic <- dim(pic)
pic <- rgb(pic[,,1], pic[,,2], 
           pic[,,3], alpha = pic[,,4])
pic <- desaturate(pic)
pic <- col2rgb(pic)[1, ]/255
dim(pic) <- dim_pic[1:2]
pic <- data.frame(pic) %>%
  mutate(row = n() - row_number())
pic <- pivot_longer(data.frame(pic),
                     cols = -row) %>%
  mutate(column = as.numeric(substr(name, 2, length(name))),
         name = NULL)

pic <- pic %>%
  filter(value > 0) %>%
  mutate(value = (1 - value)^3)

pic <- pic[sample(seq(1, nrow(pic)),
                          500,
                          prob = pic$value), ]

dis <- dist(pic[, c("row", "column")])

dis_tsp <- as.TSP(dis)
dis_solve <- solve_TSP(dis_tsp, method = "nearest_insertion", start=5)
tour <- as.numeric(labels(dis_solve))

pic <- pic[tour, ] %>%
  mutate(tour = row_number())

first_point <- pic %>%
  filter(tour == 1)

pic <- pic %>%
  mutate(row_next = lead(row, default = first_point$row),
         column_next = lead(column, default = first_point$column))

ggplot(data = pic,
       aes(x = column,
           y = row,
           xend = column_next,
           yend = row_next,
           alpha = value)) +
  geom_segment(size = 2) +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal()
ggsave("JAN_24.png",
       width = 8,
       height = 8,
       units = "in")

