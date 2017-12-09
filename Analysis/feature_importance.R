library(dplyr)


cor <- as.numeric(cor(y_train[, 2], d_train[, c(-1:-8)]))
cor_matrix <- data.frame(names(d_train[, c(-1:-8)]), cor)
cor_matrix <- cor_matrix[order(cor_matrix[, 2], decreasing = T), ]





