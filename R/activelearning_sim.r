#' Active Learning Simulation
#'
#' For a given data set, we may be interested in how well a specified active
#' learning method performs (in terms of classification accuracy) for a given
#' classifier. Furthermore, we may be interested in the impact of the number of
#' oracle queries on the classification accuracy. To simulate the performance
#' of a active learning method on a data set, we first partition the data into a
#' training and test data set. The user may specify the observation indices
#' (rows) to use as training data, or the user can specify a percentage of the
#' observations that will be retained as training data, while the remaining data
#' are utilized as test data. By default, the training percentage is 2/3.
#'
#' Then, from the training data we randomly select at least 2 observations from
#' each class to label; the remainder of the observations are kept unlabeled.
#' Classifiers are constructed from the labeled data, and the unlabeled data are
#' ignored in this process. With the specified active learning method, we aim to
#' label a subset of the unlabeled data by determining the unlabeled observations
#' that empirically optimize the active learning method's objective function.
#' The labels for the queried unlabeled observations are selected from the true
#' labels that are provided in 'y'. This querying approach emulates an oracle
#' that provides the true class label for an observation for which we do not
#' know its true label. Notice that we are querying a flawless oracle, which
#' might not be realistic. For example, if the oracle is a human, the human
#' might be wrong occasionally.
#' 
#' Next, we apply the following steps until no training observations are
#' unlabeled:
#'
#' \itemize{
#'   \item Determine the training observations that should be labeled via the
#' specified active learning method.
#'   \item Query the true labels of the corresponding observations.
#'   \item Construct the specified classifier with the labeled training data,
#' which includes the previously queried observations.
#'   \item Classify the test data set with the constructed classifier.
#'   \item Compute the proportion of incorrectly classified test data
#' observations to estimate the (conditional) error rate.
#' }
#'
#' If the number of unlabeled observations decreases below the specified number
#' of unlabeled observations to query, we only query the unlabeled observations.
#'
#' Keep in mind that for many classifiers if the number of observations is small
#' relative to the number of features, the classifier may be unstable or
#' incalculable. Furthermore, our random labeling method induces equal a priori
#' probability of class membership, which may yield biased classification
#' performance results. The user may instead specify the training observations
#' that are considered as unlabeled. To be clear, the provided indices should
#' correspond to the training indices (rows), which may not match the indices of
#' the original data.
#'
#' @param x a matrix containing the features for a data set.
#' @param y a vector of the labels for each observation in x.
#' @param method a string that contains the active learning method to be used.
#' @param classifier a string that contains the supervised classifier as given in
#' the 'caret' package.
#' @param num_query the number of observations to be queried.
#' @param training the observation indices (rows) for the given data set to be
#' used as the training data set. The remainder of the observations are used
#' as a test data set. If NULL, we randomly select the indices. See details.
#' @param train_pct The percentage of observations to be used as training data.
#' Ignored if training is not NULL.
#' @param labeled The training observation indices that are labeled. See details.
#' @param num_labels The number of observations to label for each class at the
#' start of the simulation. Ignored if labeled is not NULL.
#' @param all_results logical. If TRUE, all activelearning results are returned.
#' Otherwise, only the test (conditional) error rates are returned.
#' @param num_cores the number of CPU cores to use in parallel processing
#' @param ... additional arguments sent to the chosen active learning method and
#' classifier.
#' @return list with the results with the active learning results for each
#' simulation iteration.
#' @export
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#' activelearning_sim(x = x, y = y, method = "random", classifier = "lda")
#' activelearning_sim(x = x, y = y, method = "uncertainty", classifier = "qda",
#'                    num_labels = 10)
activelearning_sim <- function(x, y, method, classifier, num_query = 1,
                               training = NULL, train_pct = 2/3, labeled = NULL,
                               num_labels = 2, all_results = FALSE,
                               num_cores = 1, ...) {
  if (num_query < 1) {
    stop("The number of observations to query must be a positive integer.")
  }
  if (is.null(training)) {
    if (train_pct >= 1 || train_pct <= 0) {
      stop("The training percentage must be between 0 and 1, exclusively.")
    }
    training <- sample(seq_along(y), size = train_pct * length(y))
  }

  # Partitions the data and the corresponding labels into training and test
  # data.
  train_x <- x[training, ]
  train_y <- y[training]
  test_x <- x[-training, ]
  test_y <- y[-training]
  
  # By default, we choose the observations from each class to label to begin the
  # simulation.
  if (is.null(labeled)) {
    labeled <- tapply(seq_along(train_y), train_y, sample, size = num_labels)
    labeled <- do.call(c, labeled)
  }

  # Determines which observations are unlabeled to begin the simulation.
  y_unlabeled <- replace(train_y, labeled, NA)
  unlabeled <- which_unlabeled(y_unlabeled)
  num_unlabeled <- length(unlabeled)

  if (num_unlabeled <= 0) {
    stop("The number of unlabeled observations must be positive.")
  }
  
  l_results <- list()

  # Constructs the sequence of the number of observations to query for each
  # iteration of the simulation.
  seq_num_query <- rep.int(x = num_query, times = num_unlabeled %/% num_query)
  if(num_unlabeled %% num_query > 0) {
    seq_num_query <- c(seq_num_query, num_unlabeled %% num_query)
  }

  for (i in seq_along(seq_num_query)) {
    # Constructs a string that specifies the name of the elements in the current
    # iteration of results.
    iter_string <- paste("Iteration", i, sep = "")
    l_results[[iter_string]] <- list()

    # Determines which observations to query via active learning.
    al_out <- activelearning(x = x, y = y_unlabeled, train_y = train_y,
                             method = method, classifier = classifier,
                             num_cores = num_cores, ...)

    # Queries the oracle for the labels of the selected unlabeled observations.
    query <- al_out$query
    oracle_out <- query_oracle(query, train_y)

    # Updates the unlabeled observations with the labels from the oracle.
    y_unlabeled <- replace(y_unlabeled, query, train_y)

    # Determines which observations are labeled after the oracle is queried.
	  labeled <- which_labeled(y, logical = TRUE)
    unlabeled <- which_unlabeled(y)

    # Trains the classifier with caret:::train
    train_out <- caret:::train(x = subset(train_x, labeled),
                               y = subset(train_y, labeled),
                               classifier = classifier, ...)

    # Classifies the test data set with the constructed classifier.
    test_predictions <- predict(train_out, newdata = test_x)
    
    # Computes the proportion of incorrectly classified test data observations to
    # estimate the (conditional) error rate.
    test_error <- mean(test_predictions != test_y)

    # Stores the results. If all_results is TRUE, then the stored results are
    # verbose.
    if (all_results) {
      l_results[[iter_string]]$al_results <- al_out
      l_results[[iter_string]]$test_predictions <- test_predictions
    }
    l_results[[iter_string]]$queried <- query
    l_results[[iter_string]]$test_error <- test_error
  }
  
  l_results
}
