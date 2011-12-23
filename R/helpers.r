# @param logical If TRUE, then a Boolean vector if returned. If FALSE, then a
# vector of indices corresponding to the Boolean values is returned.
which_unlabeled <- function(y, logical = FALSE) {
  ifelse(logical, is.na(y),  which(is.na(y)))
}

# @param logical If TRUE, then a Boolean vector if returned. If FALSE, then a
# vector of indices corresponding to the Boolean values is returned.
which_labeled <- function(y, logical = FALSE) {
  ifelse(logical, !is.na(y),  which(!is.na(y)))
}
