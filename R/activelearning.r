#' Active Learning in R
#'
#' TODO
#'
#' @param x a matrix containing the labeled and unlabeled data. By default, 'x'
#' is NULL for the case that 'method' is "random." If 'x' is NULL, and the
#' 'method' is something other than "random," an error will be thrown.
#' @param y a vector of the labels for each observation in x. Use NA for unlabeled.
#' @param method a string that contains the active learning method to be used.
#' @param classifier a string that contains the supervised classifier as given in
#' the 'caret' package.
#' @param num_query the number of observations to be queried.
#' @param num_cores the number of CPU cores to use in parallel processing
#' @param ... additional arguments sent to the chosen active learning method and classifier.
#' @return a list that contains the least_certain observation and miscellaneous results. See above for details.
#' @export
#' @examples
#' TODO
activelearning <- function(x = NULL, y, y_truth = NULL, method, classifier,
                           num_query = 1, num_cores = 1, ...) {
  methods <- c("random", "uncertainty", "qbb", "qbc")
  stopifnot(method %in% c("random", "uncertainty", "qbb", "qbc"))

  if (is.null(x) && method != "random") {
    stop("The matrix 'x' cannot be NULL for the method, ", method)
  }
  
  method_out <- switch(method,
    random = random_query(y = y, num_query = num_query),
    uncertainty = uncert_sampling(x = x, y = y, num_query = num_query, ...),
    qbb = query_by_bagging(x = x, y = y, num_query = num_query, num_cores = num_cores, ...),
    qbc = query_by_committee(x = x, y = y, num_query = num_query, num_cores = num_cores, ...)
  )
  
  # In a realistic situation, the y_truth vector may not be available (NULL).
  # If so, then we cannot query an oracle automatically and return the results from the specified active learning method.
  if(!is.null(y_truth)) {
    method_out$queried_y <- query_oracle(i = method_out$query, y_truth)
    y[method_out$query] <- method_out$queried_y
    method_out$y <- y
  }
  method_out
}
