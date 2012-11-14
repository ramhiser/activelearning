# @param return_logical If TRUE, then a Boolean vector if returned. If FALSE,
# then a vector of indices corresponding to the Boolean values is returned.
which_unlabeled <- function(y, return_logical = FALSE) {
  if (return_logical) {
    out <- is.na(y)
  } else {
    out <- which(is.na(y))
  }
  out
}

# @param return_logical If TRUE, then a Boolean vector if returned. If FALSE,
# then a vector of indices corresponding to the Boolean values is returned.
which_labeled <- function(y, return_logical = FALSE) {
  if (return_logical) {
    out <- !is.na(y)
  } else {
    out <- which(!is.na(y))
  }
  out
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
