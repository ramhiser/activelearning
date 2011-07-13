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
	plot_bivariate(data)
}

h_query_oracle <- function(h, ...) {
	oracle_out <- active_learn(data = data,
		method = tolower(svalue(cbo_query_methods)),
		how_many = svalue(cbo_num_query)
	)
	data <<- oracle_out$data
	plot_bivariate(data)
}

# Constants to generate data.
sample_sizes <- c(50, 100, 200, 300)
pct_labeled <- c(25, 50, 75, 100)
num_groups <- seq.int(2, 5)
shapes <- c(
	Spherical = "Spherical",
	Low = "Low Correlation",
	High = "High Correlation")
query_methods <- c(Random="sample")
num_query <- seq.int(1, 5)


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
PerformanceGroup <- ggroup(cont = gNotebook)

names(gNotebook) <- c("Data", "Accuracy")

# GUI Controls.
distance_slider <- gslider(from=0.01,to=10,by=.01, value=3)
cbo_num_groups <- gcombobox(num_groups)
rdo_sample_sizes <- gradio(sample_sizes)
rdo_pct_labeled <- gradio(pct_labeled)
rdo_shapes <- gradio(shapes)
btn_gen_data <- gbutton("Generate Data", handler=h_gen_data)
cbo_query_methods <- gcombobox(names(query_methods))
cbo_num_query <- gcombobox(num_query)
btn_query_oracle <- gbutton("Query Oracle", handler=h_query_oracle)

# Adds the GUI controls to the GUI.
tmp <- gframe("Shape", container = gDataGenGroup)
add(tmp, rdo_shapes)
tmp <- gframe("Number of Groups", container = gDataGenGroup)
add(tmp, cbo_num_groups)
tmp <- gframe("Sample Size", container = gDataGenGroup)
add(tmp, rdo_sample_sizes)
tmp <- gframe("Percentage of Data to Label", container = gDataGenGroup)
add(tmp, rdo_pct_labeled)
tmp <- gframe("Distance between Groups", container = gDataGenGroup)
add(tmp, distance_slider, expand=TRUE)
add(tmp, btn_gen_data)
tmp <- gframe("Oracle", container = gDataGenGroup)
add(tmp, cbo_query_methods)
add(tmp, cbo_num_query)
add(tmp, btn_query_oracle)

# Now to add a graphics device.
add(gDataGroup, ggraphics())



# I'm making 'data' global to make querying the oracle easier.
# Yes, this is cheating and is bad practice.
data <- NULL