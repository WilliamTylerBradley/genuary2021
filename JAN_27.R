############################
# Gradients without lines. #
############################
set.seed(27)

# Apparently, it's supposed to be Monochrome gradients without lines now?

# devtools::install_github("coolbutuseless/poissoned")
# devtools::install_github("katiejolly/nationalparkcolors")
library(poissoned)
library(nationalparkcolors)
library(ggplot2)
library(ggvoronoi)
library(dplyr)

height <- 8
width <- 5
min_dist <- .5

brp <- park_palette("BlueRidgePkwy")
brp <- brp[c(6, 5, 4, 1, 2, 3)]

spaces <- seq(0, 8, length.out = 6)
sd <- (spaces[2] - spaces[1]) / 3

cell_size <- min_dist / sqrt(2)
ncols <- width / cell_size
nrows <- height / cell_size

points <- poissoned::poisson_disc(ncols = ncols, nrows = nrows, cell_size = cell_size, verbose = TRUE)
points$group <- 1

for(i in 2:27) {
  points_next <- poissoned::poisson_disc(ncols = ncols, nrows = nrows, cell_size = cell_size, verbose = TRUE)
  points_next$group <- i
  
  points <- rbind(points, points_next)
}

points <- points %>%
  mutate(bpr1 = dnorm(y, mean = spaces[1], sd = sd),
         bpr2 = dnorm(y, mean = spaces[2], sd = sd),
         bpr3 = dnorm(y, mean = spaces[3], sd = sd),
         bpr4 = dnorm(y, mean = spaces[4], sd = sd),
         bpr5 = dnorm(y, mean = spaces[5], sd = sd),
         bpr6 = dnorm(y, mean = spaces[6], sd = sd)) %>%
  rowwise() %>%
  mutate(color = sample(seq(1, 6), size = 1, prob = c(bpr1, bpr2, bpr3, bpr4, bpr5, bpr6))) %>%
  mutate(color = brp[color])

p <- ggplot() +
  geom_voronoi(data = points[points$group == 1, ],
               aes(x, y, fill = color, group = group),
               alpha = 1) 
  
for(i in 2:27) {
  p <- p + geom_voronoi(data = points[points$group == i, ],
                      aes(x, y, fill = color, group = group),
                      alpha = 1 / i) 
}

p +
scale_fill_identity() +
  coord_fixed() +
  theme_void()
ggsave("JAN_27.png",
       width = 5,
       height = 8,
       units = "in")
