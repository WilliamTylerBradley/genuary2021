###########################################
# Rule 30 (elementary cellular automaton) #
###########################################
set.seed(2)

## Libraries ----
library(data.table)
library(ggplot2)

cells <- data.table(matrix(data = 0,
                nrow = 100,
                ncol = 50))
cells[50, 1] <- 1
#cells[, V1 := as.numeric(rbinom(100, 1, .5))]

for(j in 2:ncol(cells)) {
  set(cells, 
      j = j,
      value = cells[, xor(unlist(shift(.SD, 1, fill = first(.SD), type = "lead")),
                          (.SD | shift(.SD, 1, fill = last(.SD)))) + 0,
                    .SDcols = (j - 1)])
}

cells[, x := .N - .I + 1]

cells <- melt(cells,
              id.vars = "x",
              measure.vars = 1:50,
              variable.name = "y")

cells[, y := 50 - as.numeric(y) + 1]

ggplot(data = cells,
       aes(x = x,
           y = y,
           col = value)) +
  geom_point(shape = 16) + 
  coord_equal() +
  theme_void() +
  theme(legend.position = "none") 

cells[, ':='(y_end = y - 1,
             x_1_end = x - 1,
             x_2_end = x,
             x_3_end = x + 1)]

cells <- melt(cells,
              id.vars = c("x", "y", "y_end", "value"),
              measure.vars = c("x_1_end", "x_2_end", "x_3_end"),
              value.name = "x_end")
cells[, variable := NULL]

ggplot(data = cells,
       aes(x = x,
           y = y,
           xend = x_end,
           yend = y_end,
           col = value)) +
  geom_segment() + 
  coord_equal() +
  theme_void() +
  theme(legend.position = "none") 
ggsave("JAN_02.png",
       width = 8,
       height = 5,
       units = "in")
