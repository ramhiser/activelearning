library('ProjectTemplate')
load.project()

h_gen_data <- function(h,...) {
	n <- svalue(rdo_sample_sizes)
	num_labeled_per_class <- floor(n * svalue(rdo_pct_labeled) / 100)
	data <<- gen_partial_labeled_data(
		num_labeled_per_class = num_labeled_per_class,
		n = n,
		num_groups = svalue(cbo_num_groups),
		shape = svalue(rdo_shapes),
		dist = svalue(distance_slider))
	plot_bivariate(data$X1, data$X2, data$obs_label)
}

h_query_oracle <- function(h, ...) {
	oracle_out <- active_learn(data = data,
		method = tolower(svalue(cbo_query_methods)),
		how_many = svalue(cbo_num_query)
	)
	data <<- oracle_out$data
	plot_bivariate(data$X1, data$X2, data$obs_label)
}

# Constants to generate data.
sample_sizes <- c(50, 100, 200, 300)
pct_labeled <- c(25, 50, 75)
num_groups <- seq.int(2, 5)
shapes <- c(
	Spherical = "Spherical",
	Low = "Low Correlation",
	High = "High Correlation")
query_methods <- c(Random="sample")
num_query <- seq.int(1, 5)

# GUI Controls.
distance_slider <- gslider(from=0.01,to=10,by=.01, value=3, handler=h_gen_data)
cbo_num_groups <- gcombobox(num_groups, handler=h_gen_data)
rdo_sample_sizes <- gradio(sample_sizes, handler = h_gen_data)
rdo_pct_labeled <- gradio(pct_labeled, handler = h_gen_data)
rdo_shapes <- gradio(shapes, handler=h_gen_data)	
cbo_query_methods <- gcombobox(names(query_methods), handler=h_query_oracle)
cbo_num_query <- gcombobox(num_query, handler=h_query_oracle)

# Create the layout of the window.
window <- gwindow("Active Learning Demo")
BigGroup <- ggroup(cont=window)
group <- ggroup(horizontal=FALSE, container=BigGroup)

# Adds the GUI controls to the GUI.
tmp <- gframe("Shape", container=group)
add(tmp, rdo_shapes)
tmp <- gframe("Number of Groups", container=group)
add(tmp, cbo_num_groups)
tmp <- gframe("Sample Size", container=group)
add(tmp, rdo_sample_sizes)
tmp <- gframe("Percentage of Data to Label", container=group)
add(tmp, rdo_pct_labeled)
tmp <- gframe("Distance between Groups", container=group)
add(tmp, distance_slider, expand=TRUE)
tmp <- gframe("Query Methods", container=group)
add(tmp, cbo_query_methods)
add(tmp, cbo_num_query)

# Now to add a graphics device.
add(BigGroup, ggraphics())

# I'm making 'data' global to make querying the oracle easier.
# Yes, this is cheating and is bad practice.
data <- NULL