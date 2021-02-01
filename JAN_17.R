##############################################
# Draw a line, pick a new color, move a bit. #
##############################################
set.seed(17)

library(grid)
library(colorspace)
library(ragg)


## x
# +
max_blue_c <- max_chroma(240, 55, floor = FALSE)
# -
max_purple_c <- max_chroma(300, 55, floor = FALSE)

## y
# +
max_red_c <- max_chroma(12, 55, floor = FALSE)
# -
max_pink_c <- max_chroma(0, 55, floor = FALSE)

hex(polarLUV(55, max_red_c, 0))

hex(mixcolor(.5, polarLUV(55, max_blue_c, 240), polarLUV(55, max_red_c, 0)),
    fixup = TRUE)

# df <- expand.grid(x = seq(0, max_blue_c, length.out = 400),
#                   y = seq(0, max_red_c, length.out = 400))
# df$blue <- hex(polarLUV(55, df$x, 240), fixup = TRUE)   
# df$red <- hex(polarLUV(55, df$y, 0), fixup = TRUE)   
# df$color <- hex(mixcolor(1, 
#                          polarLUV(55, df$x, 240), 
#                          polarLUV(55, df$y, 0)), fixup = TRUE)  
# library(ggplot2)
# ggplot(data = df,
#        aes(x = x,
#            y = y,
#            col = color)) +
#   geom_point() +
#   scale_color_identity()
    
x <- c(0, 0)
y <- c(0, 0)

x_hue <- c(0, 0)
y_hue <- c(0, 0)

x_chroma <- c(0, 0)
y_chroma <- c(0, 0)

agg_png(file = "JAN_17.png",
        width = 8,
        height = 8,
        units = "in",
        res = 144)

for(i in 1:170000) {
  ## Moves
  move <- sample(c(1, 0, -1), 1)
  if(abs(x[1] + move) > 100) {
    move <- -move
  }
  x[1] <- x[1] + move
  
  move <- sample(c(1, 0, -1), 1)
  if(abs(x[2] + move) > 100) {
    move <- -move
  }
  x[2] <- x[2] + move
  
  move <- sample(c(1, 0, -1), 1)
  if(abs(y[1] + move) > 100) {
    move <- -move
  }
  y[1] <- y[1] + move
  
  move <- sample(c(1, 0, -1), 1)
  if(abs(y[2] + move) > 100) {
    move <- -move
  }
  y[2] <- y[2] + move
  
  # Color
  if(x[1] > 0) {
    x_hue[1] <- 240
    x_chroma[1] <- x[1] / 100 * max_blue_c
  } else {
    x_hue[1] <- 300
    x_chroma[1] <- x[1] / 100 * max_purple_c
  }
  
  if(x[2] > 0) {
    x_hue[2] <- 240
    x_chroma[2] <- x[2] / 100 * max_blue_c
  } else {
    x_hue[2] <- 300
    x_chroma[2] <- x[2] / 100 * max_purple_c
  }
  
  if(y[1] > 0) {
    y_hue[1] <- 12
    y_chroma[1] <- y[1] / 100 * max_red_c
  } else {
    y_hue[1] <- 0
    y_chroma[1] <- y[1] / 100 * max_pink_c
  }
  
  if(y[2] > 0) {
    y_hue[2] <- 12
    y_chroma[2] <- y[2] / 100 * max_red_c
  } else {
    y_hue[2] <- 0
    y_chroma[2] <- y[2] / 100 * max_pink_c
  }
  
  color_1 <- mixcolor(.5, 
                      polarLUV(55, x_chroma[1], x_hue[1]), 
                      polarLUV(55, y_chroma[1], y_hue[1]))
  
  color_2 <- mixcolor(.5, 
                      polarLUV(55, x_chroma[2], x_hue[2]), 
                      polarLUV(55, y_chroma[2], y_hue[2]))
  
  color <- hex(mixcolor(.5, 
                        color_1, 
                        color_2),
               fixup = TRUE)

  grid.lines(x = (x / 100 * 4) + 4,
             y = (y / 100 * 4) + 4,
             default.units = "in",
             gp = gpar(col = color))
}

dev.off()
