#' A Collection of Active Learning Methods in R
#'
#' Active learning is a machine learning paradigm for optimally choosing
#' unlabeled observations in a training data set to query for their true labels.
#' The framework is particularly useful when there are very few labeled
#' observations relative to a large number of unlabeled observations, and the
#' user seeks to determine as few true labels as possible to achieve highly
#' accurate classifiers. This package is a collection of various active learning
#' methods from the literature to optimally query observations with respect to a
#' variety of objective functions. Some active learning methods require posterior
#' probability estimates of the unlabeled observations from a single classifier
#' or a committee of classifiers; this package allows the user to specify custom
#' classifiers. An excellent literature survey has been provided by Dr. Burr
#' Settles.
#' 
#' @docType package
#' @name activelearning
#' @aliases activelearning package-activelearning
NULL

#' Active Learning in R
#'
#' This function acts as a front-end wrapper function to apply one of several
#' active-learning methods to select optimally unlabeled observations in a
#' training data set to query their true labels from a gold-standard source.
#' The active-learning framework is particularly useful when there are very few
#' labeled observations relative to a large number of unlabeled observations, and
#' the user seeks to determine as few true labels as possible to achieve highly
#' accurate classifiers.
#'
#' We have implemented several active-learning methods that can be specified via
#' the \code{method} argument. These methods include:
#'
#' \describe{
#'   \item{uncertainty}{Uncertainty sampling}
#'   \item{qbb}{Query-by-bagging}
#'   \item{qbc}{Query-by-committee}
#'   \item{random}{Random selection}
#' }
#'
#' By default, uncertainty sampling is applied.
#'
#' For more details about the active-learning methods above, see
#' \url{http://github.com/ramey/activelearning} and
#' \url{http://www.cs.cmu.edu/~bsettles/pub/settles.activelearning.pdf}. The
#' latter is an excellent literature survey from Burr Settles.
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
#' useful for comparing empirically classifiers and/or active-learning methods.
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
#'
#' # For demonstration, suppose that few observations are labeled in 'y'.
#' y <- replace(y, -c(1:10, 51:60, 101:110), NA)
#'
#' activelearning(x = x, y = y, method = "random", classifier = "lda",
#'               num_query = 3)
#' activelearning(x = x, y = y, method = "uncertainty", classifier = "qda",
#'               num_query = 5)
activelearning <- function(x = NULL, y, y_truth = NULL,
                           method = c("uncertainty", "qbb", "qbc", "random"),
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
