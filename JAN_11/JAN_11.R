########################################################################
# Use something other than a computer as an autonomous process (or use #
# a non-computer random source).                                       #
########################################################################
set.seed(11)

library(magick)
library(tidyr)
library(ggplot2)

lava_lamp <- image_read("JAN_11/lava_lamp.gif")

for(s in 1:72) {
  
  scene <- image_flatten(lava_lamp[1:s])[[1]]

  green <- matrix(strtoi(scene[2,,], base = 16), nrow = 250, byrow = TRUE)
  green <- green[, 6:435]
  
  new_green <- matrix(0, nrow = 250/10, ncol = 430/10)
  
  for(i in 1:25) {
    for(j in 1:43) {
      iblock <- (1:10) + 10*(i - 1)
      jblock <- (1:10) + 10*(j - 1)
      if(median(green[iblock, jblock]) > 150) {
        new_green[i, j] <- 0 }
      else if(median(green[iblock, jblock]) > 75){
        new_green[i, j] <- 1
      }
      else{
        new_green[i, j] <- NA
      }
    }
  }
  
  new_green <- data.frame(new_green)
  new_green$row <- nrow(new_green) - seq(1, nrow(new_green))
  
  new_green <- pivot_longer(new_green, cols = -row, names_to = "col")
  
  ggplot(data = new_green,
         aes(x = col, 
             y = row)) +
    geom_text(aes(label = value),
              col = "white",
              size = 1) +
    theme_void() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "black", 
                                          color = "black")) +
    coord_equal() 
  
  ggsave(paste0(c("JAN_11/JAN_11_", 
                  formatC(s, width = 2, format = "d", flag = "0"), 
                  ".png"), collapse = ""),
         width = 2,
         height = 2)
}  
  
list.files(path='JAN_11', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("JAN_11.gif")
