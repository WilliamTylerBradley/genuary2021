######################
# TRIPLE NESTED LOOP #
######################
set.seed(1)

## Libraries ----
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

## Set up ----
# Levels
# Feature Set
## Trial Number
## Raw Data

# Features
## coefficient
## standard deviation

# Plot
## x - coefficient
## y - number of observations
## color - coefficient
## size - sd
## alpha - sd

## Create data ----
coefs <- c(-5, 0, 5)
sds <- rbeta(n = 50, 2, 3) * 50

df <- expand.grid(coefs = coefs,
                  sds = sds)

df$trials <- sample(seq(15, 25), nrow(df), replace = TRUE)

n_obs_function <- function(trials, ...) { # Note, must have ...
  tibble(n_obs = round(rexp(n = trials, rate = 1/30) + 5))
}

df <- tibble(df) %>%
  mutate(n_obs = pmap(., n_obs_function))

create_data <- function(coefs, sds, n_obs, ...) {
  #print(as.list(match.call()))
  df <- n_obs %>%
    pmap(create_data_sub, coefs = coefs, sds = sds)
}

create_data_sub <- function(coefs, sds, n_obs, ...) {
  #print(as.list(match.call()))
  x <- runif(n_obs, 0, 10)
  y <- x * coefs + rnorm(n_obs, 0, sds)
  
  new_df <- tibble(x = x,
                   y = y)
}

df <- df %>%
  pmap(create_data)

## Create Models ----
lm_function <- function(df) {
  lm(y ~ x, data = df)
}

df <- df %>%
  map_depth(2, lm_function)

n_obs_function <- function(lm_model) {
  nobs(lm_model)
}

coef_function <- function(lm_model) {
  coefficients(lm_model)[[2]]
}

sigma_function <- function(lm_model) {
  summary(lm_model)$sigma
}

df_n_obs <- df %>%
  map_depth(2, n_obs_function) %>%
  tibble() %>%
  unnest(cols = c(.)) %>%
  rename(n_obs = 1)

df_coef <- df %>%
  map_depth(2, coef_function) %>%
  tibble() %>%
  unnest(cols = c(.)) %>%
  rename(coef = 1)

df_sigma <- df %>%
  map_depth(2, sigma_function) %>%
  tibble() %>%
  unnest(cols = c(.)) %>%
  rename(sigma = 1)

df <- cbind(df_n_obs,
             df_coef,
             df_sigma) %>%
  mutate_all(as.numeric)

# Plot ----
## x - coefficient
## y - number of observations
## color - coefficient
## size - sd
## alpha - sd

ggplot(data = df,
       aes(x = coef,
           y = n_obs,
           col = coef,
           size = sigma,
           alpha = (1 - (sigma/100)))) +
  geom_point(shape = 16) + 
  theme_void() +
  theme(legend.position = "none") 
ggsave("JAN_01.png",
       width = 5,
       height = 8,
       units = "in")
