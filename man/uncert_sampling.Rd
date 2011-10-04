\name{uncert_sampling}
\alias{uncert_sampling}
\title{Active Learning with Uncertainty Sampling}
\usage{
  uncert_sampling(x, y, uncertainty = "entropy", cl_train,
  cl_predict, num_query = 1, ...)
}
\arguments{
  \item{x}{a matrix containing the labeled and unlabeled
  data}

  \item{y}{a vector of the labels for each observation in
  x. Use NA for unlabeled.}

  \item{uncertainty}{a string that contains the uncertainty
  measure. See above for details.}

  \item{cl_train}{a string that contains the supervised
  classifier's training function's name}

  \item{cl_predict}{a string that contains the supervised
  classifier's prediction function's name}

  \item{num_query}{the number of observations to be be
  queried.}

  \item{...}{additional arguments that are sent to cl_train
  and cl_predict}
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
  implemented three of the most widely used methods: 1.
  least_confidence: query the unlabeled observation with
  the least posterior probability under the trained
  classifier 2. margin: query the unlabeled observation
  that minimizes the difference in the largest two
  posterior probabilities under the trained classifier 3.
  entropy: query the unlabeled observation maximizing
  posterior probabilities of each class under the trained
  classifier The 'uncertainty' argument must be one of the
  three: 'entropy' is the default. Note that the three
  methods are equivalent (they yield the same observation
  to be queried) with binary classification.

  We require a user-specified supervised classifier and its
  corresponding prediction (classification) function. These
  must be specified as strings in 'cl_train' and
  'cl_predict', respectively. We assume that the 'cl_train'
  function accepts two arguments, x and y, as the matrix of
  feature vectors and their corresponding labels,
  respectively. The 'cl_predict' function is assumed to
  accept a trained object as its first argument and a
  matrix of test observations as its second argument.
  Furthermore, we assume that 'cl_predict' returns a list
  that contains a 'posterior' component that is a matrix of
  the posterior probabilities of class membership; the
  (i,j)th entry of the matrix must be the posterior
  probability of the ith observation belong to class j.
  Usually, it is straightforward to implement a wrapper
  function so that 'cl_train' and 'cl_predict' can be used.
  Additional arguments to 'cl_train' and 'cl_predict' can
  be passed via '...'

  Unlabeled observations in 'y' are assumed to have NA for
  a label.

  It is often convenient to query unlabeled observations in
  batch. By default, we query the unlabeled observation
  with the largest uncertainty measure value. With the
  'num_query' the user can specify the number of
  observations to return in batch. If there are ties in the
  uncertainty measure values, they are broken by the order
  in which the unlabeled observations are given.
}

