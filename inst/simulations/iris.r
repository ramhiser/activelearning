library('activelearning')
library('MASS')

x <- data.matrix(iris[,-5])
y <- rep(NA, nrow(x))
y_truth <- as.vector(iris[,5])
