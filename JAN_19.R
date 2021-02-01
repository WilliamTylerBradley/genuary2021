#############################################
# Increase the randomness along the Y-axis. #
#############################################
set.seed(19)

library(data.table)
library(ggplot2)

dt <- data.table(level = seq(1, 19),
                 n = seq(19, 1, by = -1))

dt <- dt[rep(1:.N, n)]
dt[, values := rt(n = n, df = level), by = level]

dt[, ':='(xend = 0, yend = 0)]

dt <- as.data.frame(dt)

?sign

ggplot(data = dt,
       aes(x = values,
           y = 30 - level)) +
  lapply(split(dt, 1:nrow(dt)), function(dat) {
    geom_curve(data = dat, aes(x = values, y = 30 - level, 
                               xend = xend, yend = yend), curvature = sign(dat["values"]) * .05,
               angle = 90,
               alpha = .2) }
  ) +
  geom_point(aes(size = n),
             alpha = .5,
             pch = 21,
             fill = "black") +
  theme_void() +
  theme(legend.position = "none")
ggsave("JAN_19.png",
       width = 8,
       height = 8,
       units = "in")
