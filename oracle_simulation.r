how_many <- 1
num_iter <- sum(data$obs_label == "unlabeled") / how_many

errors <- foreach(i = seq_len(num_iter)) %do% {
	oracle_out <- active_learn(data = data,
		method = "random",
		how_many = how_many
	)
	data <- oracle_out$data
	error_rates(data, test_data)
}
errors <- melt(errors)
names(errors) <- c('error', 'Classifier', 'iteration')

p <- ggplot(errors, aes(x = iteration, y = error, group = Classifier))
p <- p + geom_line(aes(color = Classifier)) + ylim(0, max(errors$error))
p + xlab("Iteration") + ylab("Test Error Rate")