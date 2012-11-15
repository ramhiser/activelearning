\docType{package}
\name{activelearning}
\alias{activelearning}
\alias{activelearning-package}
\alias{package-activelearning}
\title{A Collection of Active Learning Methods in R}
\usage{
  activelearning(x = NULL, y, y_truth = NULL,
    method = c("uncertainty", "qbb", "qbc", "random"),
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
  is useful for comparing empirically classifiers and/or
  active-learning methods.}

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
  Active learning is a machine learning paradigm for
  optimally choosing unlabeled observations in a training
  data set to query for their true labels. The framework is
  particularly useful when there are very few labeled
  observations relative to a large number of unlabeled
  observations, and the user seeks to determine as few true
  labels as possible to achieve highly accurate
  classifiers. This package is a collection of various
  active learning methods from the literature to optimally
  query observations with respect to a variety of objective
  functions. Some active learning methods require posterior
  probability estimates of the unlabeled observations from
  a single classifier or a committee of classifiers; this
  package allows the user to specify custom classifiers. An
  excellent literature survey has been provided by Dr. Burr
  Settles.

  This function acts as a front-end wrapper function to
  apply one of several active-learning methods to select
  optimally unlabeled observations in a training data set
  to query their true labels from a gold-standard source.
  The active-learning framework is particularly useful when
  there are very few labeled observations relative to a
  large number of unlabeled observations, and the user
  seeks to determine as few true labels as possible to
  achieve highly accurate classifiers.
}
\details{
  We have implemented several active-learning methods that
  can be specified via the \code{method} argument. These
  methods include:

  \describe{ \item{uncertainty}{Uncertainty sampling}
  \item{qbb}{Query-by-bagging}
  \item{qbc}{Query-by-committee} \item{random}{Random
  selection} }

  By default, uncertainty sampling is applied.

  For more details about the active-learning methods above,
  see \url{http://github.com/ramey/activelearning} and
  \url{http://www.cs.cmu.edu/~bsettles/pub/settles.activelearning.pdf}.
  The latter is an excellent literature survey from Burr
  Settles.
}
\examples{
x <- iris[, -5]
y <- iris[, 5]

# For demonstration, suppose that few observations are labeled in 'y'.
y <- replace(y, -c(1:10, 51:60, 101:110), NA)

activelearning(x = x, y = y, method = "random", classifier = "lda",
              num_query = 3)
activelearning(x = x, y = y, method = "uncertainty", classifier = "qda",
              num_query = 5)
}

