#############
# No loops. #
#############
set.seed(20)

# Hypotrochoid

library(data.table)
library(ggplot2)

t <- seq(0, 2*pi, length.out = 1000)

a <- 5
b <- 1
h <- c(1, 5, 10, 15, 20)

dt <- CJ(h, t)
dt[, ':='(x = (a - b) * cos(t) + h * cos((a - b) / b * t),
          y = (a - b) * sin(t) - h * sin((a - b) / b * t),
          xend = 0,
          yend = 0)]

dt2 <- CJ(x = seq(-20, 20, 2),
          y = seq(-20, 20, 2))
dt2[, ':='(diff = (.5 - (abs(abs(x) - abs(y)) / (abs(x) + abs(y) + 1))) * 10)]
dt2[, ':='(x = x + sign(x) * diff,
           y = y + sign(y) * diff,
           xend = 0,
           yend = 0)] 

ggplot() +
  geom_segment(data = dt2,
               aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend),
               color = "#001900") +
  geom_point(data = dt2,
             aes(x = x,
                 y = y),
             color = "#001900") +
  geom_segment(data = dt,
               aes(x = y,
                   y = x,
                   xend = xend,
                   yend = yend,
                   col = h,
                   group = h)) +
  geom_path(data = dt,
            aes(x = y,
                y = x,
                col = h,
                group = h)) +
  geom_point(data = dt,
             aes(x = y,
                 y = x,
                 col = h)) +
  scale_color_gradient(low = "#F5E489",
                       high = "#FFC0CB") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#99A399")) +
  coord_fixed()
ggsave("JAN_20.png",
       width = 8,
       height = 8,
       units = "in")
  
  

