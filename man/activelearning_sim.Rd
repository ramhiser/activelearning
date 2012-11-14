\name{activelearning_sim}
\alias{activelearning_sim}
\title{Active Learning Simulation}
\usage{
  activelearning_sim(x, y, method, classifier,
    num_query = 1, training = NULL, train_pct = 2/3,
    labeled = NULL, num_labels = 2, all_results = FALSE,
    num_cores = 1, ...)
}
\arguments{
  \item{x}{a matrix containing the features for a data
  set.}

  \item{y}{a vector of the labels for each observation in
  x.}

  \item{method}{a string that contains the active learning
  method to be used.}

  \item{classifier}{a string that contains the supervised
  classifier as given in the 'caret' package.}

  \item{num_query}{the number of observations to be
  queried.}

  \item{training}{the observation indices (rows) for the
  given data set to be used as the training data set. The
  remainder of the observations are used as a test data
  set. If NULL, we randomly select the indices. See
  details.}

  \item{train_pct}{The percentage of observations to be
  used as training data. Ignored if training is not NULL.}

  \item{labeled}{The training observation indices that are
  labeled. See details.}

  \item{num_labels}{The number of observations to label for
  each class at the start of the simulation. Ignored if
  labeled is not NULL.}

  \item{all_results}{logical. If TRUE, all activelearning
  results are returned. Otherwise, only the test
  (conditional) error rates are returned.}

  \item{num_cores}{the number of CPU cores to use in
  parallel processing}

  \item{...}{additional arguments sent to the chosen active
  learning method and classifier.}
}
\value{
  list with the results with the active learning results
  for each simulation iteration.
}
\description{
  For a given data set, this function simulates the
  classification performance of a specified active learning
  method for a given classifier. Furthermore, this
  simulation is useful to investigate training sample size
  on classification accuracy.
}
\details{
  To simulate the performance of a active learning method
  on a data set, we partition the data into a training and
  test data set. From the training data we randomly select
  at least 2 observations from each class to label; the
  remainder of the observations are kept unlabeled.
  Classifiers are constructed from the labeled data, and
  the unlabeled data are ignored in this process. With the
  specified active learning method, we aim to label a
  subset of the unlabeled data by determining the unlabeled
  observations that empirically optimize the active
  learning method's objective function. The labels for the
  queried unlabeled observations are selected from the true
  labels that are provided in \code{y}. This querying
  approach emulates an oracle that provides the true class
  label for an observation for which we do not know its
  true label. Notice that we are querying a flawless
  oracle, which might be unrealistic. For example, if the
  oracle is a human, the human might be wrong occasionally.

  Next, we apply the following steps until no training
  observations are unlabeled:

  \itemize{ \item Determine the training observations that
  should be labeled via the specified active learning
  method.  \item Query the true labels of the corresponding
  observations.  \item Construct the specified classifier
  with the labeled training data, which includes the
  previously queried observations.  \item Classify the test
  data set with the constructed classifier.  \item Compute
  the proportion of incorrectly classified test data
  observations to estimate the (conditional) error rate. }

  If the number of unlabeled observations decreases below
  the specified number of unlabeled observations to query,
  we only query the unlabeled observations.

  The user may specify the observation indices (rows) to
  use as training data, or the user can specify a
  percentage of the observations that will be retained as
  training data, while the remaining data are utilized as
  test data. By default, the training percentage is 2/3.

  Keep in mind that for many classifiers if the number of
  observations is small relative to the number of features,
  the classifier may be unstable or incalculable.
  Furthermore, our random labeling method induces equal a
  priori probability of class membership, which may yield
  biased classification performance results. The user may
  instead specify the training observations that are
  considered as unlabeled. To be clear, the provided
  indices should correspond to the training indices (rows),
  which may not match the indices of the original data.
}
\examples{
x <- iris[, -5]
y <- iris[, 5]
activelearning_sim(x = x, y = y, method = "random", classifier = "lda",
                   num_labels = 5, num_query = 3)
activelearning_sim(x = x, y = y, method = "uncertainty", classifier = "qda",
                   num_labels = 10, num_query = 5)
}

