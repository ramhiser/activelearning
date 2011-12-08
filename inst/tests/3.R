# Testing to make sure query_by_bagging works.

# Generate a list of lists that resembles the output we want for the bagged classifier.
# The 'class' contains randomly chosen classifications: there are a specified number of classes.
# The 'filler_noise' is just that: it is intended to make sure that other list elements don't interfere.
# The 'posterior' is a matrix of size 'n x num_classes' with randomly selected posterior probabilities for each test observation.
# There are n test observations and B bagged committee members.


f1 <- function(x, y) x + y


z1 <- list(name = "f1", args = list(x = 2, y = 3))
z2 <- list(name = "f2", args = list(x = 1, y = 4))
z3 <- list(name = "f3", args = list(x = 2))
z <- list(z1, z2, z3)

x <- z1 # Replace with lapply(z, function(x) { ... })
# Need to check that x$args is not null
args_string <- paste(names(x$args), x$args, sep = "=", collapse = ", ")
function_call <- paste(x$name, "(", args_string, ")", sep = "")
out <- eval(parse(text = function_call))

x <- data.matrix(subset(iris, select=-Species))
y <- iris$Species
n <- length(y)
training <- sample(seq_len(n), 2/3 * n)
train_x <- x[training, ]
train_y <- y[training]
test_x <- x[-training, ]
test_y <- y[-training]












library(kernlab)
library(caret)
rbf_out <- ksvm(x = train_x, y = train_y, kernel = rbfdot(sigma=0.1))
pbf_out <- ksvm(x = train_x, y = train_y, kernel = polydot())

classifiers <- list(knn3, ksvm)
sapply(classifiers, function(cl) {
  cl_out <- cl(x = train_x, y = train_y)
  mean(predict(cl_out, test_x) != test_y)
})