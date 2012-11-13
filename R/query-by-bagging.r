#' Active learning with "Query by Bagging"
#'
#' The 'query by bagging' approach to active learning applies bootstrap
#' aggregating (bagging) by randomly sampling with replacement B times from the
#' training data to create a committe of B classifiers. Our goal is to "query
#' the oracle" with the observations that have the maximum disagreement among the
#' B trained classifiers.
#'
#' Note that this approach is similar to "Query by Committee" (QBC), but each
#' committee member uses the same classifier trained on a resampled subset of
#' the labeled training data. With the QBC approach, the user specifies a
#' comittee with \code{C} supervised classifiers that are each trained on the
#' labeled training data. Also, note that we have implemented QBC as
#' \code{\link{query_by_committee}}.
#'
#' To determine maximum disagreement among bagged committee members, we have
#' implemented three approaches:
#' \describe{
#' \item{kullback}{query the unlabeled observation that maximizes the
#' Kullback-Leibler divergence between the label distributions of any one
#' committe member and the consensus}
#' \item{vote_entropy}{query the unlabeled observation that maximizes the vote
#' entropy among all commitee members}
#' \item{post_entropy}{query the unlabeled observation that maximizes the entropy
#' of average posterior probabilities of all committee members}
#' }
#'
#' The \code{disagreement} argument must be one of the three: \code{kullback} is
#' the default.
#'
#' To calculate the committee disagreement, we use the formulae from Dr. Burr
#' Settles' excellent "Active Learning Literature Survey" available on his
#' website. At the time this function was coded, the literature survey had last
#' been updated on January 26, 2010.
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
#' We use the \code{\link{parallel}} package to perform the bagging in parallel.
#'
#' @param x a matrix containing the labeled and unlabeled data
#' @param y a vector of the labels for each observation in \code{x}. Use
#' \code{NA} for unlabeled observations.
#' @param disagreement a string that contains the disagreement measure among the
#' committee members. See above for details.
#' @param classifier a string that contains the supervised classifier as given in
#' the \code{\link{caret}} package.
#' @param num_query the number of observations to be queried.
#' @param C the number of bootstrap committee members
#' @param ... additional arguments that are sent to the \code{\link{caret}}
#' classifier.
#' @return a list that contains the least_certain observation and miscellaneous
#' results. See above for details.
#' @export
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#' query_by_bagging(x = x, y = y, classifier = "lda")
#' query_by_bagging(x = x, y = y, disagreement = "vote_entropy",
#'                     classifier = "qda", num_query = 5)
query_by_bagging <- function(x, y, disagreement = c("kullback", "vote_entropy",
                             "post_entropy"), classifier, num_query = 1, C = 50,
                             num_cores = 1, ...) {
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
  }

  # Determines which observations (rows) are labeled.
	labeled <- which_labeled(y, logical = TRUE)
  unlabeled <- which_unlabeled(y)

  train_x <- subset(x, labeled)
  train_y <- subset(y, labeled)
  test_x <- subset(x, !labeled)
	n <- nrow(train_x)

  # Constructs a list of resampled indices from the labeled data.
  # Each list member is a 'committee' member.
  l_labeled_obs <- createResample(train_y, times = C, list = TRUE)

	# For each 'committee' member, we build a classifier from the labeled data
  # and predict each of the unlabeled data. The vote entropy method is
  # based on the classifications of the unlabeled data, whereas the posterior
  # entropy and kullback methods utilize the posterior probabilities.
  if (disagreement == "vote_entropy") {
    predict_type <- "raw"
  } else if (disagreement == "kullback" || disagreement == "post_entropy") {
    predict_type <- "prob"
    if (!any(caret_lookup$probModel)) {
      stop("The method, '", classifier, "' must return posterior probabilities")
    }
  }
  # TODO: Rather than manually applying parallel processing, let 'caret' do it
  # using its built-in mechanisms.
  bagged_out <- mclapply(l_labeled_obs, function(obs) {
    train_out <- train(x = train_x[obs, ], y = train_y[obs], ...)
    predict(train_out, test_x, type = predict_type)
  }, mc.cores = num_cores)
	
  # Computes the disagreement measure for each of the unlabeled observations
  # based on the either the predicted class labes or the posterior probailities
  # of class membership.
	disagree <- switch(uncertainty,
                     vote_entropy = {
                       bagged_out <- do.call(rbind, bagged_out)
                       apply(bagged_out, 2, function(x) {
                         entropy.empirical(table(factor(x, levels = classes)))
                       })
                     },
                     post_entropy = {
                       avg_post <- Reduce('+', bagged_out) / length(bagged_out)
                       apply(avg_post, 1, function(obs_post) {
                         entropy.plugin(obs_post)
                       })
                     },
                     kullback = {
                       consensus_prob <- Reduce('+', bagged_out) /
                                         length(bagged_out)
                       kl_member_post <- lapply(bagged_out, function(x) {
                         rowSums(x * log(x / consensus_prob))
                       })
                       Reduce('+', kl_member_post) / length(kl_member_post)
                     }
                     )

  # Determines the order of the unlabeled observations by disagreement measure.
	query <- order(disagree, decreasing = TRUE)[seq_len(num_query)]
	
	out_list <- list(query = query, bagged_out = bagged_out, unlabeled = unlabeled)
  out_list[[uncertainty]] <- disagree
  out_list
}
