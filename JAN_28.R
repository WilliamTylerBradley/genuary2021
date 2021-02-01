##############
# Use sound. #
##############
set.seed(28)

# devtools::install_github('charlie86/spotifyr')
# devtools::install_github("moldach/vapoRwave")
library(spotifyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(vapoRwave)

Sys.setenv(SPOTIFY_CLIENT_ID = "") # get from Spotify dev dashboard
Sys.setenv(SPOTIFY_CLIENT_SECRET = "")

access_token <- get_spotify_access_token()

top_50_features <- get_playlist_audio_features("spotifycharts",
                                               "37i9dQZEVXbMDoHDwVN2tF")

max_tempo <- max(top_50_features$tempo)
min_tempo <- min(top_50_features$tempo)

top_50_features <- top_50_features %>%
  select(track.id, loudness, danceability, energy, speechiness, acousticness, 
         liveness, valence, tempo, track.popularity, key)

top_50_features <- top_50_features %>%
  mutate(x = 150 - (100 - track.popularity)) %>%
  uncount(weights = x,
          .id = "x") %>%
  mutate(x = x + (100 - track.popularity))

top_50_features <- top_50_features %>%
  mutate(y = loudness + danceability * sin(energy * x) +
           speechiness * sin(acousticness * x) +
           liveness * sin(valence * x) +
           x * (tempo - min_tempo) / max_tempo)

ggplot(data = top_50_features) +
  geom_rect(fill = "black",
            xmin = 1, xmax = 150,
            ymin = min(top_50_features$y) - 1, ymax = max(top_50_features$y) + 1) +
  geom_line(aes(x = x,
                y = y,
                color = as.character(key),
                group = track.id)) +
  scale_color_vapoRwave() +
  theme_void() + 
  coord_polar() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank())
ggsave("JAN_28.png",
       width = 8,
       height = 8,
       units = "in")
