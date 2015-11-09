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

post_entropy <- function(x, type='class') {
  avg_post <- Reduce('+', x) / length(x)
  apply(avg_post, 1, function(obs_post) {
    entropy.plugin(obs_post)
  })
}

kullback <- function(x, type='class') {
  # TODO: Check if this is a better approach.
  # rowSums(log(exp(x) + x / consensus_prob))
  # NOTE: This is equivalent to:
  # rowSums(x * log(x / consensus_prob))
  # The identity improves numerical stability.
  consensus_prob <- Reduce('+', x) / length(x)
  kl_member_post <- lapply(x, function(obs) {
    rowSums(obs * log(obs / consensus_prob))
  })
  Reduce('+', kl_member_post) / length(kl_member_post)
}
