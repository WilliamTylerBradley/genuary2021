##################
# Do not repeat. #
##################
set.seed(13)

library(primes)
library(data.table)
library(ggplot2)


generate_points <- function(prime) {
  points <- seq(0, 360, length.out = (prime + 1))[1:prime] + 
    (360/(prime) * runif(prime, -.5, .5)) + 90
  
  points <- points * pi/180
  
  x <- prime * cos(points) + runif(1, -1, 1)
  y <- prime * sin(points) + runif(1, -1, 1)
  theta <- atan2(y, x)
  r <- sqrt(x^2 + y^2)
  
  x_start <- (r - .5) * cos(theta)
  x_end <- (r + .5) * cos(theta)
  y_start <- (r - .5) * sin(theta)
  y_end <- (r + .5) * sin(theta)
  
  roman <- sample(prime, 1)
  
  label <- rep(NA, prime)
  label[roman] <- as.character(as.roman(prime))
  rotate <- theta * 180/pi 
  
  x_start[roman] <- NA
  x_end[roman] <- NA
  y_start[roman] <- NA
  y_end[roman] <- NA
  
  data.table(primes = prime, x = x, y = y,
             x_start = x_start, y_start = y_start,
             x_end = x_end, y_end = y_end,
             label = label, rotate = rotate)
}

df <- data.table(primes = generate_n_primes(13))
df <- df[, generate_points(primes), by = seq_len(nrow(df))][, seq_len := NULL]

ggplot(data = df,
       aes(x = x,
           y = y,
           group = primes)) +
  geom_polygon(fill = NA,
               color = "gold") +
  geom_segment(aes(x = x_start,
                   y = y_start,
                   xend = x_end,
                   yend = y_end),
               color = "black",
               size = 2) +
  geom_segment(aes(x = x_start,
                   y = y_start,
                   xend = x_end,
                   yend = y_end),
               color = "gold",
               size = 1) +
  geom_text(aes(label = label,
                angle = rotate),
            color = "black",
            size = 6) +
  geom_text(aes(label = label,
                angle = rotate),
            color = "gold",
            size = 4) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", 
                                        color = "black")) +
  coord_equal()

ggsave("JAN_13.png",
       width = 8,
       height = 8,
       units = "in")
