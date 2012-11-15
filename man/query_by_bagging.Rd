\name{query_by_bagging}
\alias{query_by_bagging}
\title{Active learning with "Query by Bagging"}
\usage{
  query_by_bagging(x, y,
    disagreement = c("kullback", "vote_entropy", "post_entropy"),
    classifier, num_query = 1, C = 50, num_cores = 1, ...)
}
\arguments{
  \item{x}{a matrix containing the labeled and unlabeled
  data}

  \item{y}{a vector of the labels for each observation in
  \code{x}. Use \code{NA} for unlabeled observations.}

  \item{disagreement}{a string that contains the
  disagreement measure among the committee members. See
  above for details.}

  \item{classifier}{a string that contains the supervised
  classifier as given in the \code{\link{caret}} package.}

  \item{num_query}{the number of observations to be
  queried.}

  \item{C}{the number of bootstrap committee members}

  \item{...}{additional arguments that are sent to the
  \code{\link{caret}} classifier.}
}
\value{
  a list that contains the least_certain observation and
  miscellaneous results. See above for details.
}
\description{
  The 'query by bagging' approach to active learning
  applies bootstrap aggregating (bagging) by randomly
  sampling with replacement \code{C} times from the
  training data to create a committe of B classifiers. Our
  goal is to "query the oracle" with the observations that
  have the maximum disagreement among the \code{C} trained
  classifiers.
}
\details{
  Note that this approach is similar to "Query by
  Committee" (QBC), but each committee member uses the same
  classifier trained on a resampled subset of the labeled
  training data. With the QBC approach, the user specifies
  a comittee with \code{C} supervised classifiers that are
  each trained on the labeled training data. Also, note
  that we have implemented QBC as
  \code{\link{query_by_committee}}.

  To determine maximum disagreement among bagged committee
  members, we have implemented three approaches: \describe{
  \item{kullback}{query the unlabeled observation that
  maximizes the Kullback-Leibler divergence between the
  label distributions of any one committe member and the
  consensus} \item{vote_entropy}{query the unlabeled
  observation that maximizes the vote entropy among all
  commitee members} \item{post_entropy}{query the unlabeled
  observation that maximizes the entropy of average
  posterior probabilities of all committee members} }

  The \code{disagreement} argument must be one of the
  three: \code{kullback} is the default.

  To calculate the committee disagreement, we use the
  formulae from Dr. Burr Settles' excellent "Active
  Learning Literature Survey" available on his website. At
  the time this function was coded, the literature survey
  had last been updated on January 26, 2010.

  We require a user-specified supervised classifier from
  the \code{\link{caret}} R package. Furthermore, we assume
  that the classifier returns posterior probabilities of
  class membership; otherwise, an error is thrown. To
  obtain a list of valid classifiers, see the
  \code{\link{caret}} vignettes, which are available on
  CRAN. Also, see the \code{\link{modelLookup}} function in
  the \code{\link{caret}} package.

  Additional arguments to the specified \code{\link{caret}}
  classifier can be passed via \code{...}.

  Unlabeled observations in \code{y} are assumed to have
  \code{NA} for a label.

  It is often convenient to query unlabeled observations in
  batch. By default, we query the unlabeled observations
  with the largest uncertainty measure value. With the
  \code{num_query} the user can specify the number of
  observations to return in batch. If there are ties in the
  uncertainty measure values, they are broken by the order
  in which the unlabeled observations are given.

  We use the \code{\link{parallel}} package to perform the
  bagging in parallel.
}
\examples{
x <- iris[, -5]
y <- iris[, 5]

# For demonstration, suppose that few observations are labeled in 'y'.
y <- replace(y, -c(1:10, 51:60, 101:110), NA)

query_by_bagging(x = x, y = y, classifier = "lda")
query_by_bagging(x = x, y = y, disagreement = "vote_entropy",
                    classifier = "qda", num_query = 5)
}

