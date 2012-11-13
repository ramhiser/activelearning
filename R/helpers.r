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
