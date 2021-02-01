#############################################
# Make a grid of permutations of something. #
#############################################
set.seed(25)

library(ggplot2)
library(ggforce)
library(data.table)

fifths <- (90 + c(-1, 0, 1, 2, 3) * (360 / 5)) * pi/180

dt <- data.table(x1 = rep(0, 25 + 20),
                 x2 = rep(0, 25 + 20),
                 x3 = rep(1, 25 + 20),
                 x4 = rep(1, 25 + 20),
                 y1 = rep(0, 25 + 20),
                 y2 = rep(0, 25 + 20),
                 y3 = rep(0, 25 + 20),
                 y4 = rep(0, 25 + 20),
                 x_perm = rep(seq(1, 9), 5),
                 y_perm = rep(seq(1, 5), each = 5 + 4))
dt[, ':='(x1 = x1 + x_perm,
          x2 = x2 + x_perm + cos(fifths[ceiling(x_perm/2)]),
          x3 = x3 + x_perm + cos(fifths[y_perm]),
          x4 = x4 + x_perm,
          y1 = y1 + y_perm,
          y2 = y2 + y_perm + sin(fifths[ceiling(x_perm/2)]),
          y3 = y3 + y_perm + sin(fifths[y_perm]),
          y4 = y4 + y_perm,
          perm = x_perm * 10  + y_perm)]
dt[, ':='(x2 = ifelse(x_perm %% 2 == 1,
                      x2,
                      2 * shift(x4, 1, type = 'lag') - shift(x3, 1, type = 'lag')),
          x3 = ifelse(x_perm %% 2 == 1,
                      x3,
                      2 * shift(x1, 1, type = 'lead') - shift(x2, 1, type = 'lead')),
          y2 = ifelse(x_perm %% 2 == 1,
                      y2,
                      2 * shift(y4, 1, type = 'lag') - shift(y3, 1, type = 'lag')),
          y3 = ifelse(x_perm %% 2 == 1,
                      y3,
                      2 * shift(y1, 1, type = 'lead') - shift(y2, 1, type = 'lead')))]

dt <- melt(dt, 
            id.vars = c('x_perm', 'y_perm', 'perm'),
            measure.vars = list(x = c('x1', 'x2', 'x3', 'x4'),
                                y = c('y1', 'y2', 'y3', 'y4')))

ggplot(data = dt,
       aes(x = x,
           y = y,
           group = perm)) +
  geom_bezier() +
  theme_void() +
  coord_equal()
ggsave("JAN_25.png",
       width = 8,
       height = 5,
       units = "in")
  
