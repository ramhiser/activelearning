\name{activelearning}
\alias{activelearning}
\title{Active Learning in R}
\usage{
  activelearning(x = NULL, y, y_truth = NULL, method,
    classifier, num_query = 1, num_cores = 1, ...)
}
\arguments{
  \item{x}{a matrix containing the labeled and unlabeled
  data. By default, 'x' is NULL for the case that 'method'
  is "random." If 'x' is NULL, and the 'method' is
  something other than "random," an error will be thrown.}

  \item{y}{a vector of the labels for each observation in
  x. Use NA for unlabeled.}

  \item{method}{a string that contains the active learning
  method to be used.}

  \item{classifier}{a string that contains the supervised
  classifier as given in the 'caret' package.}

  \item{num_query}{the number of observations to be
  queried.}

  \item{num_cores}{the number of CPU cores to use in
  parallel processing}

  \item{...}{additional arguments sent to the chosen active
  learning method and classifier.}
}
\value{
  a list that contains the least_certain observation and
  miscellaneous results. See above for details.
}
\description{
  TODO
}
\examples{
TODO
}

