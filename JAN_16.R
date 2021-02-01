################
# Circles only #
################
set.seed(16)

library(ggplot2)

# Phyllotaxis
golden <- ((sqrt(5) + 1) / 2) * (2 * pi) # * 16?

circles <- rep(seq(1, 1600), 16)
df <- data.frame(circles = circles,
                 x = sqrt(circles) * cos(golden * circles),
                 y = sqrt(circles) * sin(golden * circles),
                 level = ceiling(seq(1:length(circles)) / 1600),
                 size = ((circles %% ceiling(seq(1:length(circles)) / 1600)) + 1) / 
                   ceiling(seq(1:length(circles)) / 1600))

ggplot(data = df,
       aes(x = x,
           y = y,
           size = size)) +
  geom_point(color = "white") +
  scale_size(range = c(0, .66)) +
  facet_wrap(. ~ level, nrow = 4) +
  theme_void() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill = "#013220")) +
  coord_fixed()
ggsave("JAN_16.png",
       width = 8,
       height = 8,
       units = "in")
