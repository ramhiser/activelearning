library('mvtnorm')

# Generates three different p-dimensional multivariate normal
# samples. The means are randomly sampled from a uniform distribution.
# The covariance matrix from each class is the identity matrix.
gen_data <- function(n1 = 50, n2 = 50, n3 = 50, p = 20) {
	mu1 <- runif(p, -1, 1)
	mu2 <- runif(p, 1, 3)
	mu3 <- runif(p, -3, 1)
	Sigma1 <- Sigma2 <- Sigma3 <- diag(p)
	n1 <- 50
	n2 <- 50
	n3 <- 50
	x1 <- rmvnorm(n1, mu1, Sigma1)
	x2 <- rmvnorm(n2, mu2, Sigma2)
	x3 <- rmvnorm(n3, mu3, Sigma3)
	y <- factor(c(rep(1, n1), rep(2, n2), rep(3, n3)))
	data.frame(rbind(x1, x2, x3), y = y)
}

# Chooses the unlabeled observation about which the user-specified
# supervised classifier is "least certain." This observation should
# then be queried by the oracle in the "active learning" framework.
#
# Currently uses the LDA classifier. Can extend this to others.
uncertainty_sampling <- function(df) {
	train_data <- subset(df, obs_label != "unlabeled", select = -y)
	train_data$obs_label <- factor(train_data$obs_label)

	lda_out <- lda(obs_label ~ ., data = train_data)
	lda_pred <- predict(lda_out, subset(df, obs_label == "unlabeled", select = -y))

	least_certain_obs <- which.min(apply(lda_pred$posterior, 1, max))
	which(df$obs_label == "unlabeled")[least_certain_obs]
}


# This function "queries the oracle" for the true label of an observation.
#
# Currently, this replaces the "obs_label" that is marked "unlabeled"
# with the true label in "y".
#
# The data frame with the queried label is returned along with the label itself.
query_oracle <- function(df, obs_num) {
	df$obs_label[obs_num] <- df$y[obs_num]
	list(obs_num = obs_num, obs_label = df$y[obs_num], df = df)
}

# Generate the multivariate normal data
df <- gen_data()
df$obs_label <- "unlabeled"

# Randomly choose 10 observations from each class to label.
num_labels <- 10
labeled_elements <- c(sapply(levels(df$y), function(cl) {
	sample(which(df$y == cl), num_labels)
}))

df$obs_label[labeled_elements] <- df$y[labeled_elements]
num_unlabeled <- sum(df$obs_label == "unlabeled")

# We query the labels for each unlabeled observation and compute
# the test error rate with the LDA classifier.
#
# To compute the test error rate, we generate 100 observations from each
# population and calculate the proportion of misclassified observations.
test_size <- 1000
test_errors <- foreach(i=seq_len(num_unlabeled), .combine = c) %do% {
	# Generates the test data to calculate the test error rate.
	test_data <- gen_data(n1 = test_size, n2 = test_size, n3 = test_size)
	
	df <- query_oracle(df, obs_num = uncertainty_sampling(df))$df
	
	train_data <- subset(df, obs_label != "unlabeled", select = -y)
	train_data$obs_label <- factor(train_data$obs_label)
	lda_out <- lda(obs_label ~ ., data = train_data)
	lda_pred <- predict(lda_out, subset(test_data, select = -y))
	mean(lda_pred$class != test_data$y)
}



# Active learning with "Query by Committee" (QBC).
#
# The observations in 'df' that are not "unlabeled" are treated as
# training data, and the remainder of the observations are test data.
#
# We apply a bagging approach by randomly sampling with replacement
# B times from the training data. From these B bootstrapped replicates,
# we construct B trained classifiers. We apply each of the trained
# classifiers to the test data. Our goal is to "query the oracle"
# with the observation that has the maximum disagreement among the
# B trained classifiers.
#
# We use 'voting entropy' as the measure of disagreement.
# Also, right now, we use LDA as the classifier.
#
# There may be ties for observations that maximize the voting entropy,
# so we return the indices of all the observations from df that maximize
# this voting entropy.
#
# This method uses the 'foreach' package and is set to do the bagging
# in parallel if a parallel backend is registered. If there is no
# parallel backend registered, a warning is thrown, but everything will
# work just fine.
query_by_committee <- function(df, B = 50) {
	# Splits the data.frame 'df' into training and test data.
	train_data <- subset(df, obs_label != "unlabeled", select = -y)
	train_data$obs_label <- factor(train_data$obs_label)
	test_data <- subset(df, obs_label == "unlabeled", select = -c(y, obs_label))
	n <- nrow(train_data)

	# Bagged predictions
	boot_pred <- foreach(b = seq_len(B)) %dopar% {
		boot <- sample(n, replace = T)
		train_boot <- train_data[boot, ]
		lda_out <- lda(obs_label ~ ., data = train_boot)
		predict(lda_out, test_data)$class
	}

	# Computes the voting entropy for each unlabeled observation.
	vote_entropy <- sapply(boot_pred, function(pred) {
		table_pred <- table(pred)
		-sum((table_pred / B) * log(table_pred / B))
	})
	
	# Determines the indices of the unlabeled observations that
	# maximize the voting entropy.
	which_max <- which(vote_entropy == max(vote_entropy))
	which(df$obs_label == "unlabeled")[which_max]
}

query_by_committee(df)