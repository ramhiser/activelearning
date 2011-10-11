library('activelearning')
library('plyr')
library('caret')
library('MASS')

set.seed(42)

# Fisher's Iris data set.
x <- data.matrix(iris[,-5])
y <- as.vector(iris[,5])
class_names <- unique(y)

# We randomly partition the data into an unlabeled training data set and a test data set. The training data set contains 2/3 of the original observations, and the test data set contains the other 1/3 of the data set.
# The vector 'training_idx' contains the indices of the original data set corresponding to the training observations.
N <- 2/3 * length(y)
training_idx <- sample(seq_along(y), N)

test_x <- x[-training_idx, ]
test_y <- y[-training_idx]
x <- x[training_idx, ]
y_truth <- y[training_idx]

# We randomly label a specified number of observations from each class. We select the indices with stratified sampling, where each class is a stratum.
num_labels <- 10
label_which <- tapply(seq_along(y_truth), y_truth, sample, size = num_labels)
label_which <- unlist(label_which, use.names = F)

# We create a list of known labels for each classifier.
# The remaining observations are labeled NA to indicate unlabeled.
# Each classifier directly matches its name in the 'caret' package.
classifiers <- c("lda", "svmRadial", "rf")
y <- replace(rep(NA, N), label_which, y_truth[label_which])
y <- rlply(length(classifiers), factor(y, levels = class_names))
names(y) <- classifiers

trained_classifiers <- sapply(classifiers, function(cl) {
  training_idx <- which(!is.na(y[[cl]]))
  train(x = x[training_idx, ], y = y[[cl]][training_idx], method = cl)
}, simplify = F)

error_rate <- lapply(predict(trained_classifiers, test_x), function(pred) {
  mean(pred != test_y)
})

# TODO Next need to use random querying to query the next observation
# TODO Repeat the classification in a loop.


# TODO Add a function that generates the experimental setup for active learning.
