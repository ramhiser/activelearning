\name{activelearning}
\alias{activelearning}
\title{Active Learning in R}
\usage{
  activelearning(x = NULL, y, y_truth = NULL,
    method = c("random", "uncertainty", "qbb", "qbc"),
    classifier, num_query = 1, num_cores = 1, ...)
}
\arguments{
  \item{x}{a matrix containing the labeled and unlabeled
  data. By default, \code{x} is \code{NULL} for the case
  that \code{method} is \code{random}. If \code{x} is
  \code{NULL}, and the \code{method} is something other
  than \code{random}, an error will be thrown.}

  \item{y}{a vector of the labels for each observation in
  x. Use \code{NA} for unlabeled.}

  \item{y_truth}{an optional vector containing the true
  classification labels of the observations in \code{x}. By
  default, this vector is \code{NULL} as this vector may
  not be unavailable in a realistic situation. This vector
  is useful in empirical comparisons.}

  \item{method}{a string that contains the active learning
  method to be used.}

  \item{classifier}{a string that contains the supervised
  classifier as given in the \code{\link{caret}} package.}

  \item{num_query}{the number of observations to be
  queried.}

  \item{num_cores}{the number of CPU cores to use in
  parallel processing}

  \item{...}{additional arguments sent to the chosen active
  learning method and classifier.}
}
\value{
  a list that contains the observations to query and
  miscellaneous results. See above for details.
}
\description{
  TODO
}
\examples{
x <- iris[, -5]
y <- iris[, 5]
activelearning(x = x, y = y, method = "random", classifier = "lda",
              num_query = 3)
activelearning(x = x, y = y, method = "uncertainty", classifier = "qda",
              num_query = 5)
}

