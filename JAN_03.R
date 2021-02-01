#########################
# Make something human. #
#########################
set.seed(3)

## Libraries ----
#devtools::install_github("jespermaag/gganatogram") #had to update R to work
library(gganatogram)
library(dplyr)
library(viridis)
library(gridExtra)

plots <- vector(mode = "list", length = 8*5)

for(i in 1:length(plots)) {
  if(runif(1) < .5) {
  organ_set <- data.frame(organ = hgMale_key$organ,
                       value = sample(1:68, 68), # length(hgMale_key$organ)
                       stringsAsFactors=F)
  plots[[i]] <- gganatogram(data=organ_set, organism='human', sex='male', fill="value", outline=F) + 
    theme_void() + scale_fill_viridis() + theme(legend.position = "none") } 
  else {
  organ_set <- data.frame(organ = hgFemale_key$organ,
                       value = sample(1:70, 70), # length(hgFemale_key$organ)
                       stringsAsFactors=F)
  plots[[i]] <- gganatogram(data=organ_set, organism='human', sex='female', fill="value", outline=F) + 
    theme_void() + scale_fill_viridis() + theme(legend.position = "none") }
}

grid.arrange(grobs = plots, ncol = 5)
ggsave("JAN_03.png",
       plot = arrangeGrob(grobs = plots, ncol = 5),
       width = 8,
       height = 5,
       units = "in")
