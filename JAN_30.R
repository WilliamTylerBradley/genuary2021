#########################################################################
# Replicate a natural concept (e.g. gravity, flocking, path following). #
#########################################################################
set.seed(30)

library(rayshader)
library(data.table)
library(extrafont)
library(ggplot2)
library(imager)
library(ambient)

# font_import()
# loadfonts(device="win")

fonts <- fonttable()
fonts <- fonts[sample(seq(1, nrow(fonts)), 30), ]
fonts$size <- sample(seq(1, 30), 30) / 2 + 2
fonts$x <- runif(30, 10, 20)
fonts$y <- runif(30, 0, 30)

p <- ggplot(df = data.frame()) + geom_point() + xlim(0, 30) + ylim(0, 30)

for(i in 1:10) {
  p + 
    geom_text(data = fonts[i, ],
              aes(x, y),
              label = "GENUARY 2021",
              size = fonts$size[i], family = fonts$FamilyName[i],
              hjust = .5, vjust = .5) +
    theme_void()
  ggsave(paste0("JAN_30/pic_30_", i, ".png"), width = 3, height = 3, dpi = 100)
}

pic <- load.image("JAN_30/pic_30_1.png")
pic <- rm.alpha(pic)
pic <- grayscale(pic)
pic <- data.table(as.data.frame(pic))
pic[, ':='(y = max(y) - y,
           value = 1 * 10 * (1 - value))]

for(i in seq(2, 10)) {
  pic_next <- load.image(paste0("JAN_30/pic_30_", i, ".png"))
  pic_next <- rm.alpha(pic_next)
  pic_next <- grayscale(pic_next)
  pic_next <- data.table(as.data.frame(pic_next))
  pic_next[, ':='(y = max(y) - y,
                  value = i * 10 * (1 - value))]
  
  pic <- rbindlist(list(pic, pic_next))
  pic <- pic[, .(value = max(value)), by = c("x", "y")]
}

pic[, value := round(value, 5)]
pic[, ':='(x = 300 - x,
           value = ifelse(value > 0,
                          value,
                          (150 - (300 - y)) * 3))]

pic <- dcast(pic, x ~ y, value.var = "value")
pic[, x := NULL]
pic <- as.matrix(pic)

weights <- c(1, 1/30, 1/30, 1/(30^2), 1/(30^2))
weights <- weights / sum(weights)
average_out <- diag(weights[1], nrow = 300)
delta <- abs(row(average_out) - col(average_out))
average_out[delta == 1] <- weights[2]
average_out[delta == 2] <- weights[3]
pic <- pic %*% average_out %*% average_out %*% average_out 

noise <- noise_perlin(dim = c(300, 300)) * 500
pic <- pic + noise

# pic %>%
#   sphere_shade(zscale = 10, texture = "imhof3") %>%
#   add_shadow(ray_shade(pic, zscale = 50)) %>%
#   add_shadow(ambient_shade(pic, zscale = 50)) %>%
#   plot_3d(pic, zscale = 50, theta = 180, phi = 45, water = TRUE,
#           windowsize = c(1000,800), zoom = 0.75, waterlinealpha = 0.3,
#           wateralpha = 0.5, watercolor = "lightblue", waterlinecolor = "white")
# render_snapshot()

# montery water gif ==== https://wcmbishop.github.io/rayshader-demo/
elev_matrix <- pic
n_frames <- 180
zscale <- 50
# frame transition variables
waterdepthvalues <- min(pic)/2 - min(pic)/2 * cos(seq(0,2*pi,length.out = n_frames))
thetavalues <- 180 + 30 * cos(seq(0, 2*pi, length.out = n_frames))
# shadow layers
ambmat <- ambient_shade(pic, zscale = zscale)
raymat <- ray_shade(pic, zscale = zscale, lambert = TRUE)

# generate .png frame images
img_frames <- paste0("JAN_30/tide", seq_len(n_frames), ".png")
for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  pic %>%
    sphere_shade(texture = "imhof3") %>%
    add_shadow(ambmat, 0.5) %>%
    add_shadow(raymat, 0.5) %>%
    plot_3d(pic, solid = TRUE, shadow = TRUE, zscale = zscale, 
            water = TRUE, watercolor = "imhof3", wateralpha = 0.8, 
            waterlinecolor = "#ffffff", waterlinealpha = 0.5,
            waterdepth = waterdepthvalues[i], 
            theta = thetavalues[i], phi = 45)
  render_snapshot(img_frames[i])
  rgl::clear3d()
}

# build gif
magick::image_write_gif(magick::image_read(img_frames), 
                        path = "JAN_30.gif", 
                        delay = 6/n_frames)


