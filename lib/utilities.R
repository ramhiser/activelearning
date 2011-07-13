clean.variable.name <- function(variable.name)
{
  variable.name <- gsub('_', '.', variable.name, perl = TRUE)
  variable.name <- gsub('-', '.', variable.name, perl = TRUE)
  variable.name <- gsub('\\s+', '.', variable.name, perl = TRUE)
  return(variable.name)
}

# Generates multivariate normal data
# Can specify the number of groups and their shapes.
# dist controls the distance of each mean from the origin
# The means lie on the x- and y-axes.
gen_data <- function(n = 30, num_groups = 3, shape = "Spherical", dist = 3) {
	message("n: ", n)
	message("num_groups: ", num_groups)
	message("shape: ", shape)
	message("dist: ", dist)
	# Only generates data for 2-5 groups
	stopifnot(num_groups >= 2 && num_groups <= 5)
	# The specified distance needs to be positive.
	stopifnot(dist > 0)
	# Check that the shape is valid
	shapes <- c("Spherical", "Low Correlation", "High Correlation")
	shape <- pmatch(shape, shapes)
	if(is.na(shape)) {
		stop("Invalid shape specified")
	}
	if(shape == 1) {
		rho <- 0
	} else if(shape == 2) {
		rho <- 0.3
	} else if(shape == 3) {
		rho <- 0.8
	}
	# Common covariance matrix
	Sigma <- matrix(c(1, rho, rho, 1), nrow = 2)

	# The first group's mean is the origin.
	# The means of the remaining groups are the specified distance away.
	# Also, the second group is to the right of the origin, and if more groups
	# are specified, the means are counter-clockwise around the origin.
	means <- matrix(
		c(c(0,0), c(dist, 0), c(0, dist), c(-dist, 0), c(0, -dist)),
		ncol = 2, byrow = TRUE)
	means <- means[seq.int(num_groups),]
	
	# Here, we generate n observations from each population and
	# return a vector of labels in y for the observations in x.
	x <- alply(means, 1, function(mu) rmvnorm(n, mean = mu, sigma = Sigma))
	x <- do.call(rbind, x)
	y <- gl(num_groups, n)
	
	out <- cbind.data.frame(x, y)
	names(out) <- c("X1", "X2", "y")
	out
}

# Generates partially labeled bivariate normal data.
#
# For active learning demonstrations, it is useful to generate data
# that is partially labeled. We do this by generating data
# with gen_data(), changing "y" to "true_label," and then adding
# "obs_label," which contain the labels observed by the user.
# Ideally, the "obs_label" should match the ground truth labels in
# "true_label," but a major concern that must be taken serious is
# an imperfect oracle, who made label an observation incorrectly.
gen_partial_labeled_data <- function(num_labeled_per_class = 5, ...) {
	if(num_labeled_per_class <= 1) {
		warning("At least 2 observations per class need to be labeled.")
		return(NULL)
	}
	data <- gen_data(...)
	names(data) <- c("X1", "X2", "true_label")
	N <- length(data$y)
	data$obs_label <- "unlabeled"
	
	labeled_elements <- c(sapply(levels(data$true_label), function(class) {
		sample(which(data$true_label == class), num_labeled_per_class)
	}))

	data$obs_label[labeled_elements] <- factor(as.character(data$true_label[labeled_elements]))
	
	data
}

# Plots a bivariate distribution with x1 on the x-axis and x2 on the y-axis.
# y is a vector of labels for each population.
plot_bivariate <- function(data) {
	labeled_data <- subset(data, data$obs_label != "unlabeled")
	unlabeled_data <- subset(data, data$obs_label == "unlabeled")
	p <- ggplot(labeled_data, aes(x = X1, y = X2, group = obs_label))
	p <- p + geom_point(aes(color = obs_label), size = 5) #+ opts(legend.title = "Groups")
	p <- p + geom_point(size = 1.5, data = unlabeled_data)
	print(p)
}

# This function acts as a facade to query an oracle in one of several ways.
# NOTE: The only implemented query method right now is "random"
# The oracle returns a reference to the observation which should be labeled
# and the labels for those observations.
# We return both of these as well as the updated data.
active_learn <- function(data, method = "random", ...) {
	if(method != "random") stop("Only the random oracle is implemented")
	if(method == "random") {
		oracle_out <- oracle_random(data, ...)
	}
	class_labels <- c(levels(data$true_label), "unknown")
	oracle_out$labels <- factor(oracle_out$labels, levels = class_labels)
	data$obs_label[oracle_out$label_which] <- oracle_out$labels
	list(data = data, labels = oracle_out$labels, label_which = oracle_out$label_which)
}


# Randomly chooses unlabeled observations to query the true label.
# The 'oracle' is queried at random.
# Returns the number(s) of the observation to query
oracle_random <- function(data, how_many = 1) {
	# The number of observations that need labeling should be
	# greater than or equal to the number of unlabeled observations.
	stopifnot(sum(data$obs_label == "unlabeled") >= how_many)
	label_which <- sample(which(data$obs_label == "unlabeled"), how_many)
	labels <- data$true_label[label_which]
	list(label_which = label_which, labels = labels)
}

# Randomly chooses unlabeled observations to query the true label.
# The 'oracle' is queried at random and is imperfect.
# The oracle has a specified percent change of being correct.
# If the oracle is wrong, an incorrect label is selected at random.
# Returns the number(s) of the observation to query
#
# Example: Queries oracle for 10 labels with 25% chance of being wrong.
# 
# out <- oracle_imperfect_random(data, 10, imperfection_rate = 0.25)
# data$obs_label[out$label_which] <- out$labels
#
oracle_imperfect_random <- function(data, how_many = 1, imperfection_rate = 0.1) {
	# The imperfection rate is a probability and must be in [0,1].
	stopifnot(0 <= imperfection_rate && imperfection_rate <= 1)
	# The number of observations that need labeling should be
	# greater than or equal to the number of unlabeled observations.
	stopifnot(sum(data$obs_label == "unlabeled") >= how_many)
	
	label_which <- sample(which(data$obs_label == "unlabeled"), how_many)
	# Randomly choose a binary outcome for each of the observations to be labeled.
	imperfections <- rbinom(length(label_which), 1, imperfection_rate)
	class_labels <- levels(data$true_label)
	
	labels <- sapply(label_which, function(obs) {
		if(rbinom(1, 1, prob = 1 - imperfection_rate) == 1) {
			label <- data$true_label[obs]
		} else {
			false_label <- which(!(levels(x) %in% data$true_label[obs]))
			false_label <- sample(class_labels[false_label], 1)
			label <- false_label
		}
		label
	})
	
	list(label_which = label_which, labels = labels)
}

# Computes error rates with various classifiers
# Uses Linear Discriminant Analysis, 1-Nearest Neighbor,
# Support Vector Machines (with radial basis function),
# and Random Forests.
#
# All defaults are used.
#
# Need to specify the data with label 'obs_label'
# Need to specify test_data as well
#
error_rates <- function(data, test_data) {
	training <- subset(data, obs_label != "unlabeled")
	training$obs_label <- factor(training$obs_label)

	test_x <- subset(test_data, select = c("X1", "X2"))
	test_y <- test_data$true_label

	lda_out <- lda(obs_label ~ X1 + X2, data = training)
	knn_out <- knn3(obs_label ~ X1 + X2, data = training, k = 1)
	rf_out <- randomForest(obs_label ~ X1 + X2, data = training)
	svm_out <- ksvm(obs_label ~ X1 + X2, data = training)

	lda_error <- mean(predict(lda_out, test_x)$class != test_y)
	knn_error <- mean(predict(knn_out, test_x, type = "class") != test_y)
	rf_error <- mean(predict(rf_out, test_x) != test_y)
	svm_error <- mean(predict(svm_out, test_x) != test_y)

	list(LDA = lda_error, KNN = knn_error, RF = rf_error, SVM = svm_error)
}