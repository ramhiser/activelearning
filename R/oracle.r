#' Automatic query of an oracle.
#'
#' This function emulates a subject matter expert reporting the true classification for a set of observations.
#'
#' @export
#' @param i a vector of the queried observation indices
#' @param y_truth the true classification labels for the data
#' @return a vector containing the classifications of observations x[i]
query_oracle <- function(i, y_truth) {
  as.vector(y_truth[i])
}