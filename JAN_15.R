############################################################
# Let someone else decide the general rules of your piece. #
############################################################
set.seed(15)

library(ggplot2)
# Create a board

board <- matrix(seq(100, 1, by = -1), nrow = 10, byrow = TRUE)
board[seq(2, 10, 2), ] <- board[seq(2, 10, 2), seq(10, 1, by = -1)]
board

board <- data.frame(space = as.vector(board),
                    x = rep(seq(1, 10), each = 10),
                    y = rep(seq(10, 1, by = -1), 10))

end_points <- sample(5:95, 20)
end_points <- data.frame(space = end_points,
                         end = c(end_points[11:20], end_points[1:10]),
                         type = c(rep("snake", 5), rep("ladder", 5)))
end_points <- end_points[(end_points$type == "snake" & 
                            end_points$space > end_points$end) |
                           (end_points$type == "ladder" & 
                              end_points$space < end_points$end), ]
end_points <- merge(end_points, board, by.x = "end", by.y = "space")
names(end_points)[names(end_points) == "x"] <- "end_x"
names(end_points)[names(end_points) == "y"] <- "end_y"

board <- merge(board, end_points, all.x = TRUE)

# ggplot() +
#   geom_tile(data = board,
#             aes(x = x,
#                 y = y),
#             col = "white")+
#   geom_segment(data = board[!is.na(board$type), ],
#             aes(x = x,
#                 xend = end_x,
#                 y = y,
#                 yend = end_y,
#                 col = type),
#             arrow = arrow())

board$end[is.na(board$end)] <- board$space[is.na(board$end)]

df_game <- data.frame(matrix(ncol = 6, nrow = 0))
names(df_game) <- c("player", "turn", "space",
                    "x", "y", "roll")

for(player in 1:150) {
  df_player <- data.frame(player = player,
                     turn = 0,
                     space = 0,
                     x = 0,
                     y = 0,
                     roll = 0)
  
  turn <- 1
  
  not_finished <- TRUE
  
  while(not_finished) {
    roll <- sample(1:6, 1)
    new_space <- df_player[df_player$turn == (turn - 1), ]$space + roll
    if(new_space > 100) {
      new_space <- df_player[df_player$turn == (turn - 1), ]$space
    } 
    new_space <- board[board$space == new_space, ]
    new_space <- board[board$space == new_space$end, ]
    
    df_player <- rbind(df_player, 
                data.frame(player = player,
                           turn = turn,
                           space = new_space$space,
                           x = new_space$x,
                           y = new_space$y,
                           roll = roll))
    
    turn <- turn + 1
    not_finished <- new_space$end != 100
  }
  
  df_game <- rbind(df_game, df_player)
}

## 1 player test
# ggplot() +
#   geom_tile(data = board,
#             aes(x = x,
#                 y = y),
#             col = "white") +
#   geom_text(data = board,
#             aes(x = x,
#                 y = y,
#                 label = space),
#             col = "gray") +
#   geom_segment(data = board[!is.na(board$type), ],
#                aes(x = x,
#                    xend = end_x,
#                    y = y,
#                    yend = end_y,
#                    col = type),
#                arrow = arrow()) +
#   geom_point(data = df,
#              aes(x = x,
#                  y = y),
#              col = "white")

df_game <- df_game[df_game$turn != 0, ]

ranks <- aggregate(df_game$turn, by = list(player = df_game$player), FUN = max)
ranks$rank <- rank(ranks$x) / 150
ranks$x <- NULL

df_game <- merge(df_game, ranks)
df_game$turn_rank <- df_game$turn + df_game$rank

df_last <- df_game[df_game$space == 100, ]

ggplot(data = df_game,
       aes(x = turn_rank,
           y = space,
           group = player)) +
  geom_line(color = "#D0E1F9") +
  geom_point(data = df_last,
             aes(x = turn_rank,
                 y = space), 
             color = "#D0E1F9") +
  theme_void() + 
  theme(panel.background = element_rect(fill = "#283655", 
                                        color = "#283655")) +
  coord_fixed() +
  scale_y_reverse()
ggsave("JAN_15.png",
       width = 5,
       height = 8,
       units = "in")

