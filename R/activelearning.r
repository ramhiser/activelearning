#' Active Learning in R
#'
#' TODO
#'
#' @param x a matrix containing the labeled and unlabeled data. By default,
#' \code{x} is \code{NULL} for the case that \code{method} is \code{random}. If
#' \code{x} is \code{NULL}, and the \code{method} is something other than
#' \code{random}, an error will be thrown.
#' @param y a vector of the labels for each observation in x. Use \code{NA} for
#' unlabeled.
#' @param y_truth an optional vector containing the true classification labels
#' of the observations in \code{x}. By default, this vector is \code{NULL} as
#' this vector may not be unavailable in a realistic situation. This vector is
#' useful in empirical comparisons.
#' @param method a string that contains the active learning method to be used.
#' @param classifier a string that contains the supervised classifier as given in
#' the \code{\link{caret}} package.
#' @param num_query the number of observations to be queried.
#' @param num_cores the number of CPU cores to use in parallel processing
#' @param ... additional arguments sent to the chosen active learning method and
#' classifier.
#' @return a list that contains the observations to query and miscellaneous
#' results. See above for details.
#' @export
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#' activelearning(x = x, y = y, method = "random", classifier = "lda",
#'               num_query = 3)
#' activelearning(x = x, y = y, method = "uncertainty", classifier = "qda",
#'               num_query = 5)
activelearning <- function(x = NULL, y, y_truth = NULL,
                           method = c("random", "uncertainty", "qbb", "qbc"),
                           classifier, num_query = 1, num_cores = 1, ...) {
  method <- match.arg(method)

  if (is.null(x) && method != "random") {
    stop("The matrix 'x' cannot be NULL for the method, ", method)
  }

  if (!is.null(x) && nrow(x) != length(y)) {
    stop("The number of observations in 'x' must match the length of 'y'.")
  }
  
  method_out <- switch(method,
    random = random_query(y = y, num_query = num_query),
    uncertainty = uncertainty_sampling(x = x, y = y, classifier = classifier,
      num_query = num_query, ...),
    qbb = query_by_bagging(x = x, y = y, classifier = classifier,
      num_query = num_query, num_cores = num_cores, ...),
    qbc = query_by_committee(x = x, y = y, num_query = num_query,
      num_cores = num_cores, ...)
  )
  
  # In a realistic situation, the y_truth vector may not be available (NULL).
  # If so, then we cannot query an oracle automatically and return the results
  # from the specified active learning method.
  if(!is.null(y_truth)) {
    method_out$queried_y <- query_oracle(i = method_out$query, y_truth)
    y[method_out$query] <- method_out$queried_y
    method_out$y <- y
  }
  method_out
}
