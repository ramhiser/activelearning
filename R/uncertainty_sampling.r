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
#' We require a user-specified supervised classifier and its corresponding
#' prediction (classification) function. These must be specified as functions in
#' 'train' and 'predict', respectively. We assume that the 'train' function
#' accepts two arguments, x and y, as the matrix of feature vectors and their
#' corresponding labels, respectively. The 'predict' function is assumed to
#' accept a trained object as its first argument and a matrix of test
#' observations as its second argument. Furthermore, we assume that 'predict'
#' returns a list that contains a 'posterior' component that is a matrix of the
#' posterior probabilities of class membership; the (i,j)th entry of the matrix
#' must be the posterior probability of the ith observation belong to class j.
#' If the 'posterior' component is not available, an error is thrown.
#'
#' Usually, it is straightforward to implement a wrapper function so that 'train'
#' and 'predict' can be used.
#'
#' Additional arguments to 'train' can be passed via '...'.
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
#' @param y a vector of the labels for each observation in x. Use NA for unlabeled.
#' @param uncertainty a string that contains the uncertainty measure. See above for details.
#' @param train a string that contains the supervised classifier's training function's name
#' @param predict a string that contains the supervised classifier's prediction function's name
#' @param num_query the number of observations to be be queried.
#' @param ... additional arguments that are sent to train
#' @return a list that contains the least_certain observation and miscellaneous results. See above for details.
uncert_sampling <- function(x, y, uncertainty = "entropy", train, predict, num_query = 1, ...) {
	unlabeled <- which(is.na(y))
  
  train_out <- train(x = x[-unlabeled, ], y = y[-unlabeled], ...)
	posterior <- predict(train_out, x[unlabeled, ])$posterior

  if (is.null(posterior)) {
    stop("The specified 'predict' function must return a list with a 'posterior' component.")
  }

	if (is.vector(posterior)) {
	  posterior <- matrix(posterior, nrow = 1)
	}
    
  obs_uncertainty <- switch(uncertainty,
                       least_confidence = apply(posterior, 1, max),
                       margin = apply(posterior, 1, function(obs_post) {
                         obs_post[order(obs_post, decreasing = T)[1:2]] %*% c(1, -1)
                       }),
                       entropy = apply(posterior, 1, entropy.plugin)
                     )
  
	query <- order(obs_uncertainty, decreasing = T)[seq_len(num_query)]
	
	list(query = query, obs_uncertainty = obs_uncertainty, posterior = posterior, unlabeled = unlabeled)
}
