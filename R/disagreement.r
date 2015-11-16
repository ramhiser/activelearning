#' Computes entropy of committee's classifications
#'
#' Computes the disagreement measure for each of the unlabeled observations
#' based on the either the predicted class labels or the posterior
#' probabilities of class membership.
#'
#' @importFrom itertools2 izip
#' @importFrom entropy entropy
vote_entropy <- function(x, type='class', entropy_method='ML') {
  it <- do.call(itertools2::izip, x)
  disagreement <- sapply(it, function(obs) {
    entropy(table(unlist(obs)), method=entropy_method)
  })
  disagreement
}

#' @importFrom entropy entropy.plugin
post_entropy <- function(x, type='posterior') {
  avg_post <- Reduce('+', x) / length(x)
  apply(avg_post, 1, function(obs_post) {
    entropy.plugin(obs_post)
  })
}

kullback <- function(x, type='posterior') {
  avg_post <- Reduce('+', x) / length(x)
  kullback_members <- lapply(x, function(obs) {
    rowSums(obs * log(obs / avg_post))
  })

  Reduce('+', kullback_members) / length(kullback_members)
}

#' @importFrom entropy entropy.plugin
entropy_uncertainty <- function(posterior) {
  apply(posterior, 1, entropy.plugin)
}

least_confidence <- function(posterior) {
  apply(posterior, 1, max)
}

margin_uncertainty <- function(posterior) {
  apply(posterior, 1, function(post_i) {
    post_i[order(post_i, decreasing=T)[1:2]] %*% c(1, -1)
  })
}
