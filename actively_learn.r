library('ProjectTemplate')
load.project()

num_labeled_per_class <- 5

set.seed(42)
data <- gen_data(n = 30, num_groups = 3)
names(data) <- c("X1", "X2", "true_label")
N <- length(data$y)
data$obs_label <- "unlabeled"

labeled_elements <- c(sapply(levels(data$true_label), function(class) {
	sample(which(data$true_label == class), num_labeled_per_class)
}))

data$obs_label[labeled_elements] <- factor(as.character(data$true_label[labeled_elements]))

active_learn(data, how_many = 5)







# Example of imperfect oracle
# out <- oracle_imperfect_random(data, 10, imperfection_rate = 0.25)
# data$obs_label[out$label_which] <- out$labels