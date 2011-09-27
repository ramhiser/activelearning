library('ProjectTemplate')
load.project()

h_gen_data <- function(h,...) {
	n <- svalue(rdo_sample_sizes)
	num_labeled_per_class <- floor(n * svalue(slid_pct_labeled) / 100)
	data <<- gen_partial_labeled_data(
		num_labeled_per_class = num_labeled_per_class,
		n = n,
		num_groups = svalue(cbo_num_groups),
		shape = svalue(rdo_shapes),
		dist = svalue(slid_group_distance)
	)
	test_data <<- gen_partial_labeled_data(
		num_labeled_per_class = 1000,
		n = 1000,
		num_groups = svalue(cbo_num_groups),
		shape = svalue(rdo_shapes),
		dist = svalue(slid_group_distance)
	)
	svalue(gNotebook) <- 1
	plot_bivariate(data)
}

h_query_oracle <- function(h, ...) {
	oracle_out <- active_learn(data = data,
		method = tolower(svalue(cbo_query_methods)),
		how_many = svalue(cbo_num_query)
	)
	data <<- oracle_out$data
	svalue(gNotebook) <- 1
	plot_bivariate(data)
}

h_query_oracle_simulation <- function(h, ...) {
	how_many <- svalue(cbo_num_query_sim)
	num_iter <- sum(data$obs_label == "unlabeled") / how_many

	errors <- foreach(i = seq_len(num_iter)) %do% {
		oracle_out <- active_learn(data = data,
			method = tolower(svalue(cbo_query_methods_sim)),
			how_many = how_many
		)
		data <- oracle_out$data
		error_rates(data, test_data)
	}
	errors <- melt(errors)
	names(errors) <- c('error', 'Classifier', 'iteration')

	p <- ggplot(errors, aes(x = iteration, y = error, group = Classifier))
	p <- p + geom_line(aes(color = Classifier, linetype = Classifier)) + ylim(0, max(errors$error))
	p <- p + xlab("Iteration") + ylab("Test Error Rate")
	#svalue(gNotebook) <- 2
	print(p)
}

# Constants to generate data.
sample_sizes <- c(50, 100, 200, 300)
num_groups <- seq.int(2, 5)
shapes <- c(
	Spherical = "Spherical",
	Low = "Low Correlation",
	High = "High Correlation")
query_methods <- c(Random="sample")
num_query <- seq.int(1, 5)


# I'm making 'data' global to make querying the oracle easier.
# Yes, this is cheating and is bad practice.
data <- NULL
test_data <- NULL

# Create the layout of the window.
options(guiToolkit = "RGtk2")
window <- gwindow("Active Learning Demo")
# The notebook creates tabs for our GUI window.
gNotebook <- gnotebook(container=window)

# Here, we add the gDataGroup to the first tab in gNotebook.
# The gDataGroup displays the data and has the controls to generate new data.
gDataGroup <- ggroup(cont=gNotebook)
gDataGenGroup <- ggroup(horizontal=FALSE, container=gDataGroup)

# Next, we add the tab that has the performance results.
gPerformanceGroup <- ggroup(cont = gNotebook)

names(gNotebook) <- c("Data", "Accuracy")

# GUI Controls for Data Generation.
slid_group_distance <- gslider(from=0.01,to=10,by=.01, value=3)
cbo_num_groups <- gcombobox(num_groups)
rdo_sample_sizes <- gradio(sample_sizes)
slid_pct_labeled <- gslider(from=1,to=100,by=1, value=10)
rdo_shapes <- gradio(shapes)
btn_gen_data <- gbutton("Generate Data", handler=h_gen_data)
cbo_query_methods <- gcombobox(names(query_methods))
cbo_num_query <- gcombobox(num_query)
btn_query_oracle <- gbutton("Query Oracle", handler=h_query_oracle)

# GUI Controls for Oracle Simulation.
cbo_query_methods_sim <- gcombobox(names(query_methods))
cbo_num_query_sim <- gcombobox(num_query)
btn_query_oracle_sim <- gbutton("Generate Results", handler=h_query_oracle_simulation)

# Adds the GUI controls to the DataGroup tab.
tmp <- gframe("Shape", container = gDataGenGroup)
add(tmp, rdo_shapes)
tmp <- gframe("Number of Groups", container = gDataGenGroup)
add(tmp, cbo_num_groups)
tmp <- gframe("Sample Size", container = gDataGenGroup)
add(tmp, rdo_sample_sizes)
tmp <- gframe("Percentage of Data to Label", container = gDataGenGroup)
add(tmp, slid_pct_labeled, expand=TRUE)
tmp <- gframe("Distance between Groups", container = gDataGenGroup)
add(tmp, slid_group_distance, expand=TRUE)
add(tmp, btn_gen_data)
tmp <- gframe("Oracle", container = gDataGenGroup)
add(tmp, cbo_query_methods)
add(tmp, cbo_num_query)
add(tmp, btn_query_oracle)
tmp <- gframe("Oracle Simulation", container = gDataGenGroup)
add(tmp, cbo_query_methods_sim)
add(tmp, cbo_num_query_sim)
add(tmp, btn_query_oracle_sim)

# Now to add a graphics device.
add(gDataGroup, ggraphics())

# This focuses the window to the first tab (i.e. "Data").
svalue(gNotebook) <- 1

results <- NULL