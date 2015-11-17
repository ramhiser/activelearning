#' Active Learning with Random Querying
#'
#' A simple, naive approach is to query unlabeled observations chosen at random.
#' Random querying is often a baseline to which proposed active learning methods
#' are compared.
#'
#' Unlabeled observations in \code{y} are assumed to have \code{NA} for a label.
#' It is often convenient to query unlabeled observations in batch. With the
#' \code{num_query} the user can specify the number of observations to return in
#' batch.
#'
#' @param y a vector of the classification labels for each observation in a data
#' set. Use \code{NA} for unlabeled observations.
#' @param num_query the number of observations to be be queried.
#' @return a list indicates which observations to \code{query}
query_random <- function(y, num_query=1) {
	unlabeled <- which_unlabeled(y)
  if (length(unlabeled) <= 1 || length(unlabeled) <= num_query) {
    query <- unlabeled
  } else {
    query <- sample(unlabeled, num_query)
  }
	list(query=query, unlabeled=unlabeled)
}

# TODO: Deprecate `random_query` because uniformity.
random_query <- query_random
