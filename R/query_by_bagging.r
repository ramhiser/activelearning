#' Active learning with "Query by Bagging"
#'
#' The 'query by bagging' approach to active learning applies bootstrap aggregating (bagging) by randomly
#' sampling with replacement B times from the training data to create a committe of B classifiers. Our goal
#' is to "query the oracle" with the observations that have the maximum disagreement among the
#' B trained classifiers.
#'
#' To determine maximum disagreement among bagged committe members, we have implemented three approaches:
#' 1. vote_entropy: query the unlabeled observation that maximizes the vote entropy among all commitee members
#' 2. post_entropy: query the unlabeled observation that maximizes the posterior entropy of all committee members
#' 3. kullback: query the unlabeled observation that maximizes the Kullback-Leibler divergence between the label distributions of any one committe member and the consensus.
#' The 'disagreement' argument must be one of the three: 'kullback' is the default.
#'
#' We require a user-specified supervised classifier and its corresponding prediction
#' (classification) function. These must be specified as strings in 'cl_train' and 'cl_predict', respectively.
#' We assume that the 'cl_train' function accepts two arguments, x and y, as the matrix of feature vectors and
#' their corresponding labels, respectively. The 'cl_predict' function is assumed to accept a trained
#' object as its first argument and a matrix of test observations as its second argument. Furthermore, we
#' assume that 'cl_predict' returns a list that contains a 'posterior' component that is a matrix of the posterior
#' probabilities of class membership and a 'class' component that is a vector with the classification of each test
#' observation; the (i,j)th entry of the 'posterior' matrix must be the posterior probability of the
#' ith observation belong to class j.
#' Usually, it is straightforward to implement a wrapper function so that 'cl_train' and 'cl_predict' can be used.
#' Additional arguments to 'cl_train' and 'cl_predict' can be passed via '...'
#'
#' Unlabeled observations in 'y' are assumed to have NA for a label.
#'
#' It is often convenient to query unlabeled observations in batch. By default, we query the unlabeled observation
#' with the largest disagreement measure value. With the 'num_query' the user can specify the number of observations
#' to return in batch. If there are ties in the disagreement measure values, they are broken by the order
#' in which the unlabeled observations are given.
#'
#' This method uses the 'foreach' package and is set to do the bagging
#' in parallel if a parallel backend is registered. If there is no
#' parallel backend registered, a warning is thrown, but everything will
#' work just fine.
#'
#' @param x a matrix containing the labeled and unlabeled data
#' @param y a vector of the labels for each observation in x. Use NA for unlabeled.
#' @param disagreement a string that contains the disagreement measure among the committee members. See above for details.
#' @param cl_train a string that contains the supervised classifier's training function's name
#' @param cl_predict a string that contains the supervised classifier's prediction function's name
#' @param num_query the number of observations to be be queried.
#' @param ... additional arguments that are sent to cl_train and cl_predict
#' @return a list that contains the least_certain observation and miscellaneous results. See above for details.
query_by_bagging <- function(x, y, disagreement = "kullback", cl_train, cl_predict, num_query = 1, B = 50, ...) {
	unlabeled <- which(is.na(y))
	n <- length(y) - length(unlabeled)

  cl_train <- get(cl_train)
  cl_predict <- get(cl_predict)
  
  train_x <- x[-unlabeled, ]
  train_y <- y[-unlabeled]
  test_x <- x[unlabeled, ]

	if(is.vector(posterior)) {
	  posterior <- matrix(posterior, nrow = 1)
	}

	# Bagged predictions
	bagged_pred <- foreach(b = seq_len(B)) %dopar% {
		boot <- sample(n, replace = T)
		train_out <- cl_train(x = train_x[boot, ], y = train_y[boot], ...)
		cl_predict(train_out, test_x, ...)
	}
	
	bagged_post <- lapply(bagged_pred, function(x) x$posterior)
	bagged_class <- lapply(bagged_pred, function(x) x$class)
	
	if(uncertainty == "vote_entropy") {
    obs_uncertainty <- lapply(bagged_pred, function() {
      
    })
  } else if(uncertainty == "post_entropy") {
    # TODO: Replace vote_entropy's V(y_i) / C with the average posterior probability for class y_i
  } else if(uncertainty == "kullback") {
    
  } # else: Should never get here

	# Computes the voting entropy for each unlabeled observation.
	vote_entropy <- sapply(boot_pred, function(pred) {
		table_pred <- table(pred)
		-sum((table_pred / B) * log(table_pred / B))
	})
	
	query <- order(obs_uncertainty, decreasing = T)[seq_len(num_query)]
	
	list(query = query, obs_uncertainty = obs_uncertainty, posterior = posterior, unlabeled = unlabeled)
}