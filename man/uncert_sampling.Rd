\name{uncert_sampling}
\alias{uncert_sampling}
\title{Active Learning with Uncertainty Sampling}
\usage{
  uncert_sampling(x, y, uncertainty = "entropy",
    classifier, num_query = 1, ...)
}
\arguments{
  \item{x}{a matrix containing the labeled and unlabeled
  data}

  \item{y}{a vector of the labels for each observation in
  x. Use NA for unlabeled.}

  \item{uncertainty}{a string that contains the uncertainty
  measure. See above for details.}

  \item{classifier}{a string that contains the supervised
  classifier as given in the 'caret' package.}

  \item{num_query}{the number of observations to be
  queried.}

  \item{...}{additional arguments that are sent to the
  'caret' classifier.}
}
\value{
  a list that contains the least_certain observation and
  miscellaneous results. See above for details.
}
\description{
  The 'uncertainty sampling' approach to active learning
  determines the unlabeled observation which the
  user-specified supervised classifier is "least certain."
  The "least certain" observation should then be queried by
  the oracle in the "active learning" framework.
}
\details{
  The least certainty term is quite general, but we have
  implemented three of the most widely used methods:

  1. least_confidence: query the unlabeled observation with
  the least posterior probability under the trained
  classifier 2. margin: query the unlabeled observation
  that minimizes the difference in the largest two
  posterior probabilities under the trained classifier 3.
  entropy: query the unlabeled observation maximizing
  posterior probabilities of each class under the trained
  classifier

  The 'uncertainty' argument must be one of the three:
  'entropy' is the default. Note that the three methods are
  equivalent (they yield the same observation to be
  queried) with binary classification.

  We require a user-specified supervised classifier from
  the 'caret' R package. Furthermore, we assume that the
  classifier returns posterior probabilities of class
  membership; otherwise, an error is thrown. To obtain a
  list of valid classifiers, see the 'caret' vignettes,
  which are available on CRAN. Also, see the 'modelLookup'
  function in the 'caret' package.

  Additional arguments to the specified 'caret' classifier
  can be passed via '...'.

  Unlabeled observations in 'y' are assumed to have NA for
  a label.

  It is often convenient to query unlabeled observations in
  batch. By default, we query the unlabeled observations
  with the largest uncertainty measure value. With the
  'num_query' the user can specify the number of
  observations to return in batch. If there are ties in the
  uncertainty measure values, they are broken by the order
  in which the unlabeled observations are given.
}
\examples{
TODO
}

