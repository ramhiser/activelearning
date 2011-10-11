#' Active Learning in R
#'
#' TODO
#'
#' @export
#' @param x a matrix containing the labeled and unlabeled data
#' @param y a vector of the labels for each observation in x. Use NA for unlabeled.
#' @param uncertainty a string that contains the uncertainty measure. See above for details.
#' @param num_query the number of observations to be be queried.
#' @param ... additional arguments sent to the chosen active learning method
#' @return a list that contains the least_certain observation and miscellaneous results. See above for details.
actlearn <- function(x, y, y_truth = NULL, method, num_query = 1, ...) {
  methods <- c("random", "uncertainty", "qbb", "qbc")
  stopifnot(method %in% c("random", "uncertainty", "qbb", "qbc"))
  
  out <- switch(method,
    random = random_query(y, num_query = num_query, ...),
    uncertainty = uncert_sampling(y, num_query = num_query, ...),
    qbb = query_by_bagging(y, num_query = num_query, ...),
    qbc = query_by_committee(y, num_query = num_query, ...)
  )
  
  # TODO Implement a query_oracle function
  query_oracle(y, y_truth, num_query = 1)
  
  # TODO Return a summary of the active learning query and an updated 'y'
  list()
}