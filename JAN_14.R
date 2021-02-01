##################
# // SUBDIVISION #
##################
set.seed(14)

library(sf)
library(ggplot2)
library(tvthemes)

create_shape <- function(point, radius, shape) {
  if(shape == "square") {
    rbind(c(point[1] - radius, point[2] - radius),
          c(point[1] + radius, point[2] - radius),
          c(point[1] + radius, point[2] + radius),
          c(point[1] - radius, point[2] + radius),
          c(point[1] - radius, point[2] - radius))
  } else if(shape == "circle") {
    perim <- seq(from = 0, to = 2*pi, length.out = 100)
    perim <- c(perim[1:100], perim[1])
    
    cbind(radius * cos(perim) + point[1], 
          radius * sin(perim) + point[2])
  } else if(shape == "triangle") {
    rbind(c(point[1] - radius, point[2] - radius),
          c(point[1] + radius, point[2] - radius),
          c(point[1], point[2] + radius),
          c(point[1] - radius, point[2] - radius))
  } else {
    point
  }
}

# Square
main <- st_polygon(list(rbind(c(0,0), c(100,0), c(100,100), c(0,100), c(0,0))))
sub <- (main - st_centroid(main)) * 
  matrix(c(0, 1, -1, 0), 2, 2) * .50 + st_centroid(main)
sub <- st_cast(sub, to = "LINESTRING")

max_distance <- st_distance(sub, st_centroid(sub))

shapes <- data.frame(x = runif(140, max = 100),
                     y = runif(140, max = 100))

shape_list <- vector("list", 141)

for(shape_num in 1:140) {
  point <- st_point(x = as.numeric(shapes[shape_num, ]))
  dis <- st_distance(sub, point)
  prop <- max(c(1 - dis / max_distance, 0))
  check <- runif(1) < prop
  
  if(check) {
    radius <- runif(1, min = 1, max = 10)
    shape_list[[shape_num]] <- st_polygon(list(create_shape(point, radius, shape = "square")))
  }
}

shape_list[[141]] <- main
shapes <- st_intersection(st_set_precision(st_sfc(shape_list), 1e6))
shapes <- shapes[unlist(st_covers(main, shapes))]
main <- (main - st_centroid(main)) * 
  matrix(c(0, 1, -1, 0), 2, 2) * 1.001 + st_centroid(main)
shapes <- st_sf(ID = 1:length(shapes), geom = shapes)

ggplot(data = shapes,
       aes(fill = ID)) +
  geom_sf() +
  scale_fill_avatar(palette = "EarthKingdom",
                    type = "continuous") +
  theme_void() +
  theme(legend.position = "none")
ggsave("JAN_14.png",
       width = 8,
       height = 8,
       units = "in")

# # Circle
# perim <- seq(from = 0, to = 2*pi, length.out = 50)
# perim <- c(perim[1:50], perim[1])
# 
# main <- st_polygon(list(cbind(50 * cos(perim) + 50,
#                               50 * sin(perim) + 50)))
# 
# sub <- (main - st_centroid(main)) *
#   matrix(c(0, 1, -1, 0), 2, 2) * .50 + st_centroid(main)
# sub <- st_cast(sub, to = "LINESTRING")
# 
# max_distance <- st_distance(sub, st_centroid(sub))
# 
# shapes <- data.frame(x = runif(140, max = 100),
#                      y = runif(140, max = 100))
# 
# shape_list <- vector("list", 141)
# 
# for(shape_num in 1:140) {
#   point <- st_point(x = as.numeric(shapes[shape_num, ]))
#   dis <- st_distance(sub, point)
#   prop <- max(c(1 - dis / max_distance, 0))
#   check <- runif(1) < prop
# 
#   if(check) {
#     radius <- runif(1, min = 1, max = 10)
#     shape_list[[shape_num]] <- st_polygon(list(create_shape(point, radius, shape = "circle")))
#   }
# }
# 
# shape_list[[141]] <- main
# shapes <- st_buffer(st_make_valid(st_set_precision(st_sfc(shape_list), 1e6)), 0)
# shapes <- st_intersection(st_snap(shapes, shapes, tolerance = 5))
# main <- (main - st_centroid(main)) *
#   matrix(c(0, 1, -1, 0), 2, 2) * 1.1 + st_centroid(main)
# shapes <- shapes[unlist(st_contains(main, shapes))]
# 
# shapes <- st_sf(ID = 1:length(shapes), geom = shapes)
# 
# ggplot(data = shapes,
#        aes(fill = ID)) +
#   geom_sf() +
#   scale_fill_avatar(palette = "WaterTribe",
#                     type = "continuous") +
#   theme_void() +
#   theme(legend.position = "none")
# 
# # Triangle
# main <- st_polygon(list(rbind(c(0,0), c(100,0), c(50,100), c(0,0))))
# 
# sub <- (main - st_centroid(main)) *
#   matrix(c(0, 1, -1, 0), 2, 2) * .50 + st_centroid(main)
# sub <- st_cast(sub, to = "LINESTRING")
# 
# max_distance <- st_distance(sub, st_centroid(sub))
# 
# shapes <- data.frame(x = runif(140, max = 100),
#                      y = runif(140, max = 100))
# 
# shape_list <- vector("list", 141)
# 
# for(shape_num in 1:140) {
#   point <- st_point(x = as.numeric(shapes[shape_num, ]))
#   dis <- st_distance(sub, point)
#   prop <- max(c(1 - dis / max_distance, 0))
#   check <- runif(1) < prop
# 
#   if(check) {
#     radius <- runif(1, max = 10)
#     shape_list[[shape_num]] <- st_polygon(list(create_shape(point, radius, shape = "triangle")))
#   }
# }
# 
# shapes <- st_intersection(st_set_precision(st_sfc(shape_list), 1e5))
# main <- (main - st_centroid(main)) *
#   matrix(c(0, 1, -1, 0), 2, 2) * 10 + st_centroid(main)
# shapes <- shapes[unlist(st_contains(main, shapes))]
# 
# shapes <- st_sf(ID = 1:length(shapes), geom = shapes)
# 
# ggplot(data = shapes,
#        aes(fill = ID)) +
#   geom_sf() +
#   scale_fill_avatar(palette = "WaterTribe",
#                     type = "continuous") +
#   theme_void() +
#   theme(legend.position = "none")
# 
# ggplot(data = shape_list[[16]]) +
#   geom_sf()
