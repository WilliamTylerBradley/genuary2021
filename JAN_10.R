###########
# // TREE #
###########
set.seed(10)

library(ape)
library(phytools)
library(stringr)

text_vector <- c("(", rep("-", 98), ")", ";")
tree_bag <- seq(2, 99)

for(i in 1:10) {
  parentheses <- sort(sample(tree_bag, 2))
  text_vector[parentheses[1]] <- "("
  text_vector[parentheses[2]] <- ")"
  tree_bag <- tree_bag[!(tree_bag %in% parentheses)]
}

text_vector <- paste0(text_vector, collapse = "")
text_vector <- str_replace_all(text_vector,
                               "--",
                               "-,-")
text_vector <- str_replace_all(text_vector,
                               "--",
                               "-,-")
text_vector <- str_replace_all(text_vector,
                               "\\)\\(",
                               "\\),\\(")
text_vector <- str_replace_all(text_vector,
                               "\\)-",
                               "\\),-")
text_vector <- str_replace_all(text_vector,
                               "-\\(",
                               "-,\\(")

tree <- read.tree(text = text_vector)

plotTree(tree,
         type = "fan")

png("JAN_10.png", 
    width = 2400, height = 2400, res = 300)
plotTree(tree,
         type="fan")
dev.off()
