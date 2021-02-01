###############
# Curve only. #
###############
set.seed(8)

# https://mse.redwoods.edu/darnold/math55/DEproj/Sp98/PeterG/sld015.htm

library(deSolve)
library(ggplot2)
library(viridis)

pursuit_curve <- function(t, y, parms) {
  with(as.list(c(y, parms)), {
    
    y_prime_1 <- k * (cos(t) - y_1) / sqrt((cos(t) - y_1)^2 + (sin(t) - y_2)^2)
    
    y_prime_2 <- k * (sin(t) - y_2) / sqrt((cos(t) - y_1)^2 + (sin(t) - y_2)^2)
    
    return(list(c(y_prime_1, y_prime_2)))
  })
}

times <- seq(0, 5*pi, length.out = 200)

y_pop <- data.frame(y_1 = runif(8, -8, 8),
                     y_2 = runif(8, -8, 8))

k_pop <- c(.1, .5, .75, 1)

df <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df) <- c("time", "y_1", "y_2", "i", "k")

for(i in 1:nrow(y_pop)) {
  y <- c(y_1 = y_pop$y_1[i], y_2 = y_pop$y_2[i])
  for(j in 1:length(k_pop)) {
    parms  <- c(k = k_pop[j])
    out <- data.frame(ode(y, times, pursuit_curve, parms))
    out$line <- i * 10 + j
    out$k <- j
    df <- rbind(df, out) 
  }
}

ggplot(data = df,
       aes(x = y_1,
           y = y_2,
           group = line,
           color = k)) +
  geom_path() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#474766", 
                                        color = "#474766")) +
  coord_equal() +
  scale_color_viridis(option = "plasma")
ggsave("JAN_08.png",
       width = 8,
       height = 8,
       units = "in")

