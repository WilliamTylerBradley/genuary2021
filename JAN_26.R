###################
# 2D Perspective. #
###################
set.seed(26)

library(ggplot2)
library(data.table)

positions_table <- data.table(letter = rep(c("A", "B", "C", "D"), each = 4),
                              suborder = rep(seq(0, 3), 4),
                              xdelta = c(c(0,  0,  1,  0),
                                         c(0, -1,  0,  1),
                                         c(0,  0, -1,  0),
                                         c(0,  1,  0, -1)),
                              ydelta = c(c(0,  1,  0, -1),
                                         c(0,  0, -1,  0),
                                         c(0, -1,  0,  1),
                                         c(0,  0,  1,  0)))
setkey(positions_table, letter, suborder)

letter_table <- data.table(letter = rep(c("A", "B", "C", "D"), each = 4),
                           suborder = rep(seq(0, 3), 4),
                           subletter = c(c("D", "A", "A", "B"),
                                         c("C", "B", "B", "A"),
                                         c("B", "C", "C", "D"),
                                         c("A", "D", "D", "C")),
                           xdelta = c(c(0,  0,  1,  0),
                                      c(0, -1,  0,  1),
                                      c(0,  0, -1,  0),
                                      c(0,  1,  0, -1)),
                           ydelta = c(c(0,  1,  0, -1),
                                      c(0,  0, -1,  0),
                                      c(0, -1,  0,  1),
                                      c(0,  0,  1,  0)))
setkey(letter_table, letter, suborder)

dt <- data.table(letter = "A",
                 x = 0,
                 y = 0,
                 order = 1)

for( i in 1:5) {
dt <- dt[rep(1:.N, 4)][,suborder := seq_len(.N) - 1, by = order]
dt[, ':='(x = ifelse(suborder == 0, x, 0),
          y = ifelse(suborder == 0, y, 0))]
setkey(dt, letter, suborder)

dt <- letter_table[dt]
dt[, ':='(order = (order - 1) * 4 + suborder + 1,
          letter = subletter,
          x = x + xdelta,
          y = y + ydelta,
          subletter = NULL,
          xdelta = NULL,
          ydelta = NULL)]
}


## Final points
dt <- dt[rep(1:.N, 4)][,suborder := seq_len(.N) - 1, by = order]
dt[, ':='(x = ifelse(suborder == 0, x, 0),
          y = ifelse(suborder == 0, y, 0))]
setkey(dt, letter, suborder)

dt <- positions_table[dt]
dt[, ':='(order = (order - 1) * 4 + suborder + 1,
          x = x + xdelta,
          y = y + ydelta,
          xdelta = NULL,
          ydelta = NULL)]
setkey(dt, order)

dt[, ':='(x_graph = cumsum(x),
          y_graph = cumsum(y))]
dt[, ':='(x_graph = x_graph - max(x_graph / 2),
          y_graph = y_graph - max(y_graph) / 2)]

# break the corners
x_breaks <- runif(4) * max(dt$x_graph) * c(-1, -1, 1, 1)
x_slides <- runif(4, 1, 13) * c(-1, -1, 1, 1)
y_breaks <- runif(4) * max(dt$y_graph) * c(-1, 1, 1, -1)
y_slides <- runif(4, 1, 13) * c(-1, 1, 1, -1)

dt[, ':='(x_graph = ifelse(y_graph < 0 & x_graph < 0 & x_graph < x_breaks[1],
                           x_graph + x_slides[1],
                           x_graph),
          y_graph = ifelse(y_graph < 0 & x_graph < 0 & y_graph < y_breaks[1],
                           y_graph + y_slides[1],
                           y_graph))]
dt[, ':='(x_graph = ifelse(y_graph > 0 & x_graph < 0 & x_graph < x_breaks[2],
                           x_graph + x_slides[2],
                           x_graph),
          y_graph = ifelse(y_graph > 0 & x_graph < 0 & y_graph > y_breaks[2],
                           y_graph + y_slides[2],
                           y_graph))]
dt[, ':='(x_graph = ifelse(y_graph > 0 & x_graph > 0 & x_graph > x_breaks[3],
                           x_graph + x_slides[3],
                           x_graph),
          y_graph = ifelse(y_graph > 0 & x_graph > 0 & y_graph > y_breaks[3],
                           y_graph + y_slides[3],
                           y_graph))]
dt[, ':='(x_graph = ifelse(y_graph < 0 & x_graph > 0 & x_graph > x_breaks[4],
                           x_graph + x_slides[4],
                           x_graph),
          y_graph = ifelse(y_graph < 0 & x_graph > 0 & y_graph < y_breaks[4],
                           y_graph + y_slides[4],
                           y_graph))]

# 2d perspective
dt[, ':='(x_graph = x_graph / (.01 * abs(x_graph) + 1),
          y_graph = y_graph / (.01 * abs(x_graph) + 1))]

dt[,':='(x_graph_end = shift(x_graph, type = 'lead'),
         y_graph_end = shift(y_graph, type = 'lead'))]
dt <- dt[!is.na(x_graph_end)]

ggplot(data = dt,
       aes(x = x_graph,
           y = y_graph,
           xend = x_graph_end,
           yend = y_graph_end)) +
  geom_segment(lineend = "square",
               linejoin = "mitre") +
  coord_equal() +
  theme_void()
ggsave("JAN_26.png",
       width = 8,
       height = 8,
       units = "in")