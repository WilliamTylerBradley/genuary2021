#####################
# function f(x) {   #
#     DRAW(x);      #
#     f(1 * x / 4); #
#     f(2 * x / 4); #
#     f(3 * x / 4); # 
# }                 #
#####################
set.seed(21)

library(grid)
library(data.table)
library(ragg)

# points on triangles
xucoord <- c(sqrt(3)/2, 0, -sqrt(3)/2, sqrt(3)/2) # repeat to mimic loop
yucoord <- c(-1/2, 1, -1/2, -1/2)

DRAW <- function(x) {
  ## draw and check here
  xcoord <- x
  xcoord[, ':='(x1 = xcenter + radius * xucoord[side],
                y1 = ycenter + radius * yucoord[side],
                x2 = xcenter + radius * xucoord[side + 1],
                y2 = ycenter + radius * yucoord[side + 1])]
  xcoord[, ':='(xpos = (slide * x1 + (1 - slide) * x2),
                ypos = (slide * y1 + (1 - slide) * y2))]
  
  grid.circle(x = unlist(xcoord[, "xcenter"]),
              y = unlist(xcoord[, "ycenter"]),
              r = unlist(xcoord[, "ycenter"]) / 7)
  
  grid.curve(x1 = unlist(xcoord[, "xcenter"]),
             y1 = unlist(xcoord[, "ycenter"]),
             x2 = unlist(xcoord[, "xpos"]),
             y2 = unlist(xcoord[, "ypos"]),
             default.units = "in",
             square = FALSE,
             angle = unlist(xcoord[, "slide"]) * 180,
             inflect = TRUE,
             gp = gpar(col = "#741AEC"))
  
  grid.circle(x = unlist(xcoord[, "xcenter"]),
              y = unlist(xcoord[, "ycenter"]),
              r = (1/21),
              default.units = "in",
              gp = gpar(fill = NA,
                        col = "#D5BAF9"))
  
  grid.circle(x = unlist(xcoord[, "xcenter"]),
              y = unlist(xcoord[, "ycenter"]),
              r = (1/42),
              default.units = "in",
              gp = gpar(fill = "#D5BAF9",
                        col = "#D5BAF9"))
  
  xcoord[, ':='(xcenter = xpos,
                ycenter = ypos)]
  
}

x <- data.table(xcenter = 4,
                ycenter = 4,
                radius = 3,
                side = sample(c(1, 2, 3), 21, replace = TRUE),
                slide = runif(21))

f <- function(replication, x, shrink) {

  x <- copy(x)
  x <- x[rep(x[, .I], replication )]
  
  x[, ':='(radius = radius / shrink,
           side = sample(c(1, 2, 3), .N, replace = TRUE),
           slide = runif(.N))]
  
  x <- x[radius >= 3/4/4/4]
  
  if(nrow(x) > 0) {
    x <- DRAW(x)
    f(1, x, 4)
    f(2, x, 4)
    f(3, x, 4)
  }
}

agg_png(file = "JAN_21.png",
        width = 8,
        height = 8,
        units = "in",
        res = 144)

grid.circle(x = 4,
            y = 4,
            r = 4,
            default.units = "in",
            gp = gpar(fill = "#17052F",
                      col = "#17052F"))

f(1, x, 1)

dev.off()

# Note: can see the lines over some circles
#  because of the drawing order

