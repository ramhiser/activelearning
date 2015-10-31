#' Active learning with "Query by Committee"
#'
#' The 'query by committee' approach to active learning uitilizes a committee of
#' \code{C} classifiers that are each trained on the labeled training data. Our
#' goal is to "query the oracle" with the observations that have the maximum
#' disagreement among the \code{C} trained classifiers.
#'
#' Note that this approach is similar to "Query by Bagging" (QBB), but each
#' committee member is specified by the user. With the QBB approach, only one
#' supervised classifier is specified by the user, and each committee member is
#' trained on a resampled subset of  the labeled training data. Also, note that
#' we have implemented QBB as \code{\link{query_by_bagging}}.
#'
#' To determine maximum disagreement among committee committe members, we have
#' implemented three approaches:
#'
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
#' Settles' "Active Learning Literature Survey" available on his website.
#' At the time this function was coded, the literature survey had last been
#' updated on January 26, 2010.
#'
#' We require a user-specified supervised classifier from the \code{\link{caret}}
#' R package. Furthermore, we assume that the classifier returns posterior
#' probabilities of class membership; otherwise, an error is thrown. To obtain a
#' list of valid classifiers, see the \code{\link{caret}} vignettes, which are
#' available on CRAN. Also, see the \code{\link{modelLookup}} function in the
#' \code{\link{caret}} package.
#'
#' To specify the committee members, we require a character vector of classifiers
#' in the \code{committee} argument with elements corresponding to each
#' supervised classifier (each committee member). The classifiers must match the
#' naming in the \code{caret} package.
#'
#' Unlabeled observations in \code{y} are assumed to have \code{NA} for a label.
#'
#' It is often convenient to query unlabeled observations in batch. By default,
#' we query the unlabeled observation with the largest disagreement measure
#' value. With the \code{num_query} the user can specify the number of
#' observations to return in batch. If there are ties in the disagreement measure
#' values, they are broken by the order in which the unlabeled observations are
#' given.
#'
#' @export
#' @param x a matrix containing the labeled and unlabeled data
#' @param y a vector of the labels for each observation in \code{x}. Use
#' \code{NA} for unlabeled.
#' @param committee a list containing the committee of classifiers. See details
#' for the required format.
#' @param disagreement a string that contains the disagreement measure among the
#' committee members. See above for details.
#' @param num_query the number of observations to be queried.
#' @return a list that contains the least_certain observation and miscellaneous
#' results. See above for details.
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # For demonstration, suppose that few observations are labeled in 'y'.
#' y <- replace(y, -c(1:12, 51:62, 101:112), NA)
#'
#' committee <- c("lda", "qda", "rda")
#' query_committee(x=x, y=y, committee=committee, num_query=3)$query
#' query_committee(x=x, y=y, committee=committee, disagreement="post_entropy",
#'                 num_query=5)$query
query_committee <- function(x, y, committee,
                            disagreement=c("kullback", "vote_entropy",
                                 "post_entropy"), num_query=1, num_cores=1,
                            ...) {
  # Validates the classifier string.
  dev_null <- lapply(committee, validate_classifier, posterior_prob=TRUE)

  disagreement <- match.arg(disagreement)

	unlabeled <- which_unlabeled(y)
	n <- length(y) - length(unlabeled)
  
  train_x <- x[-unlabeled, ]
  train_y <- y[-unlabeled]
  test_x <- x[unlabeled, ]

	# Trains each classifier in the committee with the 'caret' implementation.
  committee_fits <- mclapply(committee, function(classifier) {
    train(x=train_x, y=train_y, method=classifier)
  }, mc.cores=num_cores)

  # Classifies the unlabeled observations with each committee member and also
  # determines their posterior probabilities.
	committee_class <- predict(committee_fits, test_x)
  committee_class <- do.call(cbind, lapply(committee_class, as.character))

	committee_post <- predict(committee_fits, test_x, type="prob")
  
	disagree <- switch(disagreement,
                     vote_entropy=apply(committee_class, 1, function(x) {
                       entropy(table(factor(x, levels=levels(y))))
                     }),
                     post_entropy={
                       avg_post <- Reduce('+', committee_post)
                       avg_post <- avg_post / length(committee_post)
                       apply(avg_post, 1, function(obs_post) {
                         entropy(obs_post)
                       })
                     },
                     # TODO: Refactor K-L and make it its own helper function.
                     kullback={
                       consensus_prob <- Reduce('+', committee_post)
                       consensus_prob <- consensus_prob / length(committee_post)
                       kl_post_by_member <- lapply(committee_post, function(x) {
                         # NOTE: This is equivalent to:
                         # rowSums(x * log(x / consensus_prob))
                         # The identity improves numerical stability.
                         rowSums(log(exp(x) + x / consensus_prob))
                       })
                       Reduce('+', kl_post_by_member) / length(kl_post_by_member)
                     }
                    )

  query <- order(disagree, decreasing=TRUE)[seq_len(num_query)]
	
	out_list <- list(query=query, committee_class=committee_class,
                   committee_post=committee_post, unlabeled=unlabeled)
  out_list$disagreement <- disagree
  out_list
}

query_by_committee <- query_committee
