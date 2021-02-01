##########################
# Interference patterns. #
##########################
set.seed(9)

library(ggplot2)
library(ggforce)

df <- data.frame(source_x = runif(9) * 3 + rep(c(-4.5, -1.5, 1.5), each = 3),
                  source_y = runif(9) * 3 + rep(c(-4.5, -1.5, 1.5), times = 3))

ggplot(data = df,
       aes(x = source_x, 
           y = source_y)) +
  geom_point() +
  geom_hline(yintercept = c(-4.5, -1.5, 1.5, 4.5)) +
  geom_vline(xintercept = c(-4.5, -1.5, 1.5, 4.5))

df <- merge(df,
            data.frame(expand.grid(radius = seq(0, 9, length.out = 25))))

ggplot(data = df,
       aes(x0 = source_x, 
           y0 = source_y,
           r = radius)) +
  geom_circle(alpha = .5) +
  xlim(c(-9, 9)) +
  ylim(c(-9, 9)) +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal() 
ggsave("JAN_09.png",
       width = 8,
       height = 8,
       units = "in")
