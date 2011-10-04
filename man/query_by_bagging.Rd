\name{query_by_bagging}
\alias{query_by_bagging}
\title{Active learning with "Query by Bagging"}
\usage{
  query_by_bagging(x, y, disagreement = "kullback",
  cl_train, cl_predict, num_query = 1, C = 50, ...)
}
\arguments{
  \item{x}{a matrix containing the labeled and unlabeled
  data}

  \item{y}{a vector of the labels for each observation in
  x. Use NA for unlabeled.}

  \item{disagreement}{a string that contains the
  disagreement measure among the committee members. See
  above for details.}

  \item{cl_train}{a string that contains the supervised
  classifier's training function's name}

  \item{cl_predict}{a string that contains the supervised
  classifier's prediction function's name}

  \item{num_query}{the number of observations to be be
  queried.}

  \item{C}{the number of bootstrap committee members}

  \item{...}{additional arguments that are sent to cl_train
  and cl_predict}
}
\value{
  a list that contains the least_certain observation and
  miscellaneous results. See above for details.
}
\description{
  The 'query by bagging' approach to active learning
  applies bootstrap aggregating (bagging) by randomly
  sampling with replacement B times from the training data
  to create a committe of B classifiers. Our goal is to
  "query the oracle" with the observations that have the
  maximum disagreement among the B trained classifiers.
}
\details{
  Note that this approach is similar to "Query by
  Committee" (QBC), but each committee member uses the same
  classifier trained on a resampled subset of the labeled
  training data. With the QBC approach, the user specifies
  a comittee with C supervised classifiers that are each
  trained on the labeled training data. Also, note that we
  we have implemented QBC as query_by_committee.

  To determine maximum disagreement among bagged committe
  members, we have implemented three approaches: 1.
  vote_entropy: query the unlabeled observation that
  maximizes the vote entropy among all commitee members 2.
  post_entropy: query the unlabeled observation that
  maximizes the entropy of average posterior probabilities
  of all committee members 3. kullback: query the unlabeled
  observation that maximizes the Kullback-Leibler
  divergence between the label distributions of any one
  committe member and the consensus. The 'disagreement'
  argument must be one of the three: 'kullback' is the
  default.

  To calculate the committee disagreement, we use the
  formulae from Dr. Burr Settles' "Active Learning
  Literature Survey" available on his website. At the time
  this function was coded, the literature survey had last
  been updated on January 26, 2010.

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
  the posterior probabilities of class membership and a
  'class' component that is a vector with the
  classification of each test observation; the (i,j)th
  entry of the 'posterior' matrix must be the posterior
  probability of the ith observation belong to class j.
  Usually, it is straightforward to implement a wrapper
  function so that 'cl_train' and 'cl_predict' can be used.
  Additional arguments to 'cl_train' and 'cl_predict' can
  be passed via '...'

  Unlabeled observations in 'y' are assumed to have NA for
  a label.

  It is often convenient to query unlabeled observations in
  batch. By default, we query the unlabeled observation
  with the largest disagreement measure value. With the
  'num_query' the user can specify the number of
  observations to return in batch. If there are ties in the
  disagreement measure values, they are broken by the order
  in which the unlabeled observations are given.

  This method uses the 'foreach' package and is set to do
  the bagging in parallel if a parallel backend is
  registered. If there is no parallel backend registered, a
  warning is thrown, but everything will work just fine.
}

