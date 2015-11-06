# Computes the disagreement measure for each of the unlabeled observations
# based on the either the predicted class labes or the posterior probailities
# of class membership.
vote_entropy <- function(x, type='class', entropy_method='ML') {
  x <- do.call(rbind, x)
  disagreement <- apply(x, 2, function(col) {
    entropy(table(factor(col)), method=entropy_method)
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
  # NOTE: This is equivalent to:
  # rowSums(x * log(x / consensus_prob))
  # The identity improves numerical stability.
  consensus_prob <- Reduce('+', x) / length(x)
  kl_member_post <- lapply(x, function(obs) {
    rowSums(obs * log(obs / consensus_prob))
  })
  Reduce('+', kl_member_post) / length(kl_member_post)
}
