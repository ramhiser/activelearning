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
#' \describe{
#' \item{entropy}{query the unlabeled observation maximizing posterior
#' probabilities of each class under the trained classifier}
#' \item{least_confidence}{query the unlabeled observation with the least
#' posterior probability under the trained classifier}
#' \item{margin}{query the unlabeled observation that minimizes the difference in
#' the largest two posterior probabilities under the trained classifier}
#' }
#'
#' The \code{uncertainty} argument must be one of the three: \code{entropy} is
#' the default. Note that the three methods are equivalent (they yield the same
#' observation to be queried) with binary classification.
#'
#' We require a user-specified supervised classifier from the \code{\link{caret}}
#' R package. Furthermore, we assume that the classifier returns posterior
#' probabilities of class membership; otherwise, an error is thrown. To obtain a
#' list of valid classifiers, see the \code{\link{caret}} vignettes, which are
#' available on CRAN. Also, see the \code{\link{modelLookup}} function in the
#' \code{\link{caret}} package.
#'
#' Additional arguments to the specified \code{\link{caret}} classifier can be
#' passed via \code{...}.
#' 
#' Unlabeled observations in \code{y} are assumed to have \code{NA} for a label.
#'
#' It is often convenient to query unlabeled observations in batch. By default,
#' we query the unlabeled observations with the largest uncertainty measure
#' value. With the \code{num_query} the user can specify the number of
#' observations to return in batch. If there are ties in the uncertainty measure
#' values, they are broken by the order in which the unlabeled observations are
#' given.
#'
#' @param x a matrix containing the labeled and unlabeled data
#' @param y a vector of the labels for each observation in x. Use NA for
#' unlabeled.
#' @param uncertainty a string that contains the uncertainty measure. See above
#' for details.
#' @param classifier a string that contains the supervised classifier as given in
#' the \code{caret} package.
#' @param num_query the number of observations to be queried.
#' @param ... additional arguments that are sent to the \code{caret} classifier.
#' @return a list that contains the least_certain observation and miscellaneous
#' results. See above for details.
#' @export
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # For demonstration, suppose that few observations are labeled in 'y'.
#' y <- replace(y, -c(1:10, 51:60, 101:110), NA)
#'
#' uncertainty_sampling(x=x, y=y, classifier="lda")
#' uncertainty_sampling(x=x, y=y, uncertainty="entropy",
#'                     classifier="qda", num_query=5)
uncertainty_sampling <- function(x, y, uncertainty="entropy", classifier,
                                 num_query=1, ...) {

  # Validates the classifier string.
  validate_classifier(classifier, posterior_prob=TRUE)
  
  x <- as.matrix(x)
  y <- factor(y)
  split_out <- split_labeled(x, y)

  train_out <- train(x=split_out$x_labeled, y=split_out$y_labeled,
                     method=classifier, verbose=FALSE, ...)

  # Extracts the class posterior probabilities for the unlabeled observations.
	posterior <- predict(train_out, newdata=split_out$x_unlabeled, type="prob")
  posterior <- unname(data.matrix(posterior))
    
  # Computes the specified uncertainty for each of the unlabeled observations
  # based on the posterior probabilities of class membership.
  # TODO: Refactor and move function to disagreement.r
  obs_uncertainty <- switch(uncertainty,
                       least_confidence=apply(posterior, 1, max),
                       margin=apply(posterior, 1, function(post_i) {
                         post_i[order(post_i, decreasing=T)[1:2]] %*% c(1, -1)
                       }),
                       entropy=apply(posterior, 1, entropy.plugin)
                     )
  # Determines the order of the unlabeled observations by uncertainty measure.
	query <- order(obs_uncertainty, decreasing=T)[seq_len(num_query)]
	
	out_list <- list(query=query, posterior=posterior, unlabeled=unlabeled)
  out_list[[uncertainty]] <- obs_uncertainty
  out_list
}
