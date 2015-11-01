#' Active learning with "Query by Bagging"
#'
#' The 'query by bagging' approach to active learning applies bootstrap
#' aggregating (bagging) by randomly sampling with replacement \code{C} times
#' from the training data to create a committe of \code{C} classifiers. Our goal
#' is to "query the oracle" with the observations that have the maximum
#' disagreement among the \code{C} trained classifiers.
#'
#' Note that this approach is similar to "Query by Committee" (QBC) in
#' \code{\link{query_committee}}, but each committee member uses the same
#' classifier trained on a resampled subset of the labeled training data.
#'
#' To determine maximum \code{disagreement} among bagged committee members, we
#' have implemented three approaches:
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
#' To calculate the committee disagreement, we use the formulae from Dr. Burr
#' Settles' excellent "Active Learning Literature Survey" available at
#' \url{http://burrsettles.com/pub/settles.activelearning.pdf}.
#'
#' Unlabeled observations in \code{y} are assumed to have \code{NA} for a label.
#'
#' It is often convenient to query unlabeled observations in batch. By default,
#' we query the unlabeled observations with the largest uncertainty measure
#' value. With the \code{num_query} the user can specify the number of
#' observations to return in batch. If there are ties in the uncertainty
#' measure values, they are broken by the order in which the unlabeled
#' observations are given.
#'
#' A parallel backend can be registered for building a QBB model using multiple
#' workers. For more details, see \code{\link[caret]{train}} or
#' \url{http://topepo.github.io/caret/parallel.html}.
#'
#' @param x a matrix containing the labeled and unlabeled data
#' @param y a vector of the labels for each observation in \code{x}. Use
#'     \code{NA} for unlabeled observations.
#' @param fit a function that has arguments \code{x}, \code{y}, and \code{...}
#'     and produces a model object that can later be used for
#'     prediction. function that generates predictions for each sub-model. See
#'     \code{\link[caret]{bagControl}} for more details.
#'@param predict a function that generates predictions for each sub-model. See
#'     \code{\link[caret]{bagControl}} for more details.
#' @param disagreement a string that contains the disagreement measure among
#'     the committee members. See above for details.
#' @param num_query the number of observations to be queried.
#' @param C the number of bootstrap committee members
#' @param ... additional arguments passed to the function specified in
#'     \code{fit}
#' @return a list that contains the least_certain observation and miscellaneous
#'     results. See above for details.
#' @importFrom caret bag bagControl
#' @export
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # For demonstration, suppose that few observations are labeled in 'y'.
#' y <- replace(y, -c(1:10, 51:60, 101:110), NA)
#'
#' fit_f <- function(x, y, ...) {
#'   MASS::lda(x, y, ...)
#' }
#' predict_f <- function(object, x) {
#'   predict(object, x)$class
#' }
#'
#' query_bagging(x=x, y=y, fit=fit_f, predict=predict_f, C=5)
#' query_bagging(x=x, y=y, fit=fit_f, predict=predict_f, C=10
#'               disagreement="vote_entropy", num_query=5)
query_bagging <- function(x, y, fit, predict,
                          disagreement=c("kullback", "vote_entropy", "post_entropy"),
                          num_query=1, C=50, ...) {

  disagreement <- match.arg(disagreement)
  disagree_f <- switch(disagreement,
                       "kullback"=kullback,
                       "vote_entropy"=vote_entropy,
                       "post_entropy"=post_entropy)

  # TODO: Refactor with a function that splits train/test based on x and y.
  y <- factor(y)
  classes <- levels(y)

  # Determines which observations (rows) are labeled.
	labeled <- which_labeled(y, return_logical=TRUE)
  unlabeled <- which_unlabeled(y)

  train_x <- subset(x, labeled)
  train_y <- subset(y, labeled)
  test_x <- subset(x, !labeled)

	n <- nrow(train_x)
  p <- ncol(train_x)

  bag_control <- caret::bagControl(
      fit=fit,
      predict=predict,
      aggregate=disagree_f,
      oob=FALSE,
      allowParallel=TRUE
  )

  bag_out <- bag(x=train_x, y=train_y, B=C, vars=p, bagControl=bag_control, ...)
  disagreement <- predict(bag_out, test_x)

  # Determines the order of the unlabeled observations by disagreement measure.
	query <- head(order(disagreement, decreasing=TRUE), n=num_query)

	list(query=query, disagreement=disagreement, unlabeled=unlabeled)
}

# TODO: Deprecate `query_by_bagging` because verbose.
query_by_bagging <- query_bagging
