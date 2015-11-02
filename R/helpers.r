# Returns a vector of indices of unlabeled observations.
which_unlabeled <- function(y) {
  which(is.na(y))
}

# Returns a vector of indices of labeled observations.
which_labeled <- function(y, return_logical = FALSE) {
  which(!is.na(y))
}

# Splits a matrix and its class labels into labeled and unlabeled pairs.
split_labeled <- function(x, y) {
  x <- as.matrix(x)
  y <- factor(y)

  unlabeled_i <- which_unlabeled(y)
  list(x_labeled=x[-unlabeled_i, ],
       y_labeled=y[-unlabeled_i],
       x_unlabeled=x[unlabeled_i, ],
       y_unlabeled=y[unlabeled_i])
}

#' Validates the classifier specified from the 'caret' package
#'
#' We ensure that the specified classifier is a valid classifier in the
#' \code{caret} package.
#'
#' @export
#' @param classifier string that contains the supervised classifier as given in
#' the \code{caret} package.
#' @param posterior_prob Are posterior probabilities required? If so, set to
#' \code{TRUE}. By default, set to \code{FALSE}.
#' @return \code{TRUE} invisibly if no errors occur.
#' @examples
#' validate_classifier('lda')
#' validate_classifier('What else floats? ... Very small rocks. ... Gravy.')
validate_classifier <- function(classifier, posterior_prob = FALSE) {
  # Tests that the specified classifier is given in 'caret', is actually a
  # classifier, and provides posterior probabilities of class membership.
  if (missing(classifier) || is.null(classifier) || is.na(classifier)) {
    stop("A classifier must be specified")
  }
  caret_lookup <- try(modelLookup(classifier), silent = TRUE)
  if (inherits(caret_lookup, "try-error")) {
    stop("Cannot find, '", classifier, "' in the 'caret' package")
  } else if (!any(caret_lookup$forClass)) {
    stop("The method, '", classifier, "' must be a classifier")
  }

  if (posterior_prob && !any(caret_lookup$probModel)) {
    stop("The method, '", classifier, "' must return posterior probabilities")
  }

  invisible(TRUE)
}
