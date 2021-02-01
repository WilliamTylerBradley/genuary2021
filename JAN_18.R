##############################################
# One process grows, another process prunes. #
##############################################
set.seed(18)

library(MASS)
library(purrr)
library(dplyr)
library(ggplot2)
library(hexbin)

mu_grow <- as.list(data.frame(rbind(runif(90, 0, 100), runif(90, 0, 100))))
df_grow <- map(mu_grow, mvrnorm, n = 180, Sigma = matrix(c(15, 0, 0, 15), 2))
df_grow <- data.frame(do.call(rbind, df_grow))
df_grow$ID <- seq(1:nrow(df_grow))

# df_grow <- df_grow %>%
#   mutate(X1_round = round(X1),
#          X2_round = round(X2)) %>%
#   group_by(X1_round, X2_round) %>%
#   mutate(X_rn = row_number())

mu_prune <- as.list(data.frame(rbind(runif(90, 0, 100), runif(90, 0, 100))))
df_prune <- map(mu_prune, mvrnorm, n = 180, Sigma = matrix(c(5, 0, 0, 5), 2))
df_prune <- data.frame(do.call(rbind, df_prune))
df_prune$ID <- seq(1:nrow(df_prune))

# df_prune <- df_prune %>%
#   mutate(X1_round = round(X1),
#          X2_round = round(X2)) %>%
#   group_by(X1_round, X2_round) %>%
#   mutate(X_rn = row_number())
# 
# df <- anti_join(df_grow, df_prune,
#                       by = c("X1_round", "X2_round",
#                              "X_rn"))

nearest <- function(X1, X2, ...) {
  distances <- sqrt((X1 - df_grow$X1)^2 + 
                      (X2 - df_grow$X2)^2)
  
  df_grow$ID[distances == min(distances)][1]
}

df_prune$nearest <- pmap_int(df_prune, nearest)

df <- anti_join(df_grow, df_prune, by = c("ID" = "nearest"))

ggplot(data = df,
       aes(X1, X2)) +
  geom_point()

ggplot(data = df_grow,
       aes(X1, X2)) +
  geom_hex() +
  theme_void() +
  theme(legend.position = "none")
ggsave("JAN_18.png",
       width = 8,
       height = 8,
       units = "in")

