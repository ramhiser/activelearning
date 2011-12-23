#' Active Learning with Uncertainty Sampling
#'
#' The 'uncertainty sampling' approach to active learning determines the
#' unlabeled observation which the user-specified supervised classifier is "least
#' certain." The "least certain" observation should then be queried by the oracle
#' in the "active learning" framework.
#'
#' The least certainty term is quite general, but we have implemented three of
#' the most widely used methods:
#'
#' 1. least_confidence: query the unlabeled observation with the least posterior
#' probability under the trained classifier
#' 2. margin: query the unlabeled observation that minimizes the difference in
#' the largest two posterior probabilities under the trained classifier
#' 3. entropy: query the unlabeled observation maximizing posterior probabilities
#' of each class under the trained classifier
#'
#' The 'uncertainty' argument must be one of the three: 'entropy' is the default.
#' Note that the three methods are equivalent (they yield the same observation to
#' be queried) with binary classification.
#'
#' We require a user-specified supervised classifier from the 'caret' R package.
#' Furthermore, we assume that the classifier returns posterior probabilities of
#' class membership; otherwise, an error is thrown. To obtain a list of valid
#' classifiers, see the 'caret' vignettes, which are available on CRAN. Also,
#' see the 'modelLookup' function in the 'caret' package.
#'
#' Additional arguments to the specified 'caret' classifier can be passed via
#' '...'.
#' 
#' Unlabeled observations in 'y' are assumed to have NA for a label.
#'
#' It is often convenient to query unlabeled observations in batch. By default,
#' we query the unlabeled observations with the largest uncertainty measure
#' value. With the 'num_query' the user can specify the number of observations
#' to return in batch. If there are ties in the uncertainty measure values, they
#' are broken by the order in which the unlabeled observations are given.
#'
#' @param x a matrix containing the labeled and unlabeled data
#' @param y a vector of the labels for each observation in x. Use NA for
#' unlabeled.
#' @param uncertainty a string that contains the uncertainty measure. See above
#' for details.
#' @param classifier a string that contains the supervised classifier as given in
#' the 'caret' package.
#' @param num_query the number of observations to be queried.
#' @param ... additional arguments that are sent to the 'caret' classifier.
#' @return a list that contains the least_certain observation and miscellaneous
#' results. See above for details.
#' @export
#' @examples
#' TODO
uncert_sampling <- function(x, y, uncertainty = "entropy", classifier,
                            num_query = 1, ...) {

  # Tests that the specified classifier is given in 'caret', is actually a
  # classifier, and provides posterior probabilities of class membership.
  if (is.null(classifier) || is.na(classifier)) {
    stop("A classifier must be specified")
  }
  caret_lookup <- try(modelLookup(classifier), silent = TRUE)
  if (inherits(caret_lookup, "try-error")) {
    stop("Cannot find, '", classifier, "' in the 'caret' package")
  } else if (!any(caret_lookup$forClass)) {
    stop("The method, '", classifier, "' must be a classifier")
  } else if (!any(caret_lookup$probModel)) {
    stop("The method, '", classifier, "' must return posterior probabilities")
  }

  # Determines which observations (rows) are labeled.
	labeled <- which_labeled(y, logical = TRUE)
  unlabeled <- which_unlabeled(y)

  # Trains the classifier with caret:::train
  train_out <- caret:::train(x = subset(train_x, labeled),
                             y = subset(train_y, labeled),
                             classifier = classifier, ...)

  # Extracts the class posterior probabilities for the unlabeled observations.
	posterior <- predict(train_out, newdata = x[unlabeled, ], type = "prob")
  posterior <- unname(data.matrix(posterior))
    
  # Computes the specified uncertainty for each of the unlabeled observations
  # based on the posterior probabilities of class membership.
  obs_uncertainty <- switch(uncertainty,
                       least_confidence = apply(posterior, 1, max),
                       margin = apply(posterior, 1, function(post_i) {
                         post_i[order(post_i, decreasing = T)[1:2]] %*% c(1, -1)
                       }),
                       entropy = apply(posterior, 1, entropy.plugin)
                     )
  # Determines the order of the unlabeled observations by uncertainty measure.
	query <- order(obs_uncertainty, decreasing = T)[seq_len(num_query)]
	
	list(query = query, obs_uncertainty = obs_uncertainty, posterior = posterior,
       unlabeled = unlabeled)
}
