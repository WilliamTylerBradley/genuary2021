####################################
# Draw a line. Wrong answers only. #
####################################
set.seed(22)

library(ggplot2)
library(sp)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geosphere)

world <- ne_countries(scale = "medium", returnclass = "sf")

lats <- runif(2, -90, 90)
lons <- runif(2, -180, 180)

coords <- matrix(c(lons, lats), ncol = 2)
antipodes <- antipode(coords)

world <- sf::st_transform(world,
                          paste0("+proj=ortho +lat_0=", lats[1], 
                                 " +lon_0=", lons[1]))

gc <- greatCircle(p1 = coords[1, ],
                  p2 = coords[2, ])

sites <- st_sfc(st_linestring(matrix(gc, ncol = 2)), crs = 4326)
sites <- sf::st_transform(sites,
                          paste0("+proj=ortho +lat_0=", lats[1], 
                                 " +lon_0=", lons[1]))

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = sites, size = 2) +
  theme_void()
ggsave("JAN_22.png",
       width = 8,
       height = 8,
       units = "in")
