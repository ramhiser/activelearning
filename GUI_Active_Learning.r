library('ProjectTemplate')
load.project()

updatePlot <- function(h,...) {
	data <- gen_data(
		n = svalue(rdo_sample_sizes),
		num_groups = svalue(cbo_num_groups),
		shape = svalue(rdo_shapes),
		dist = svalue(distance_slider))
	plot_bivariate(data$X1, data$X2, data$y)
}

sample_sizes <- c(50, 100, 200, 300)
num_groups <- seq.int(2, 5)
shapes <- c(
	Spherical = "Spherical",
	Low = "Low Correlation",
	High = "High Correlation")
query_methods <- c(Random="sample")

distance_slider <- gslider(from=0.01,to=10,by=.01, value=3, handler=updatePlot)
cbo_num_groups <- gcombobox(num_groups, handler=updatePlot)
rdo_sample_sizes <- gradio(sample_sizes, handler = updatePlot)
rdo_shapes <- gradio(shapes, handler=updatePlot)
	
cbo_query_methods <- gcombobox(names(query_methods), handler=updatePlot)

## now layout
window <- gwindow("Active Learning Demo")
BigGroup <- ggroup(cont=window)
group <- ggroup(horizontal=FALSE, container=BigGroup)

tmp <- gframe("Shape", container=group)
add(tmp, rdo_shapes)
tmp <- gframe("Number of Groups", container=group)
add(tmp, cbo_num_groups)
tmp <- gframe("Sample Size", container=group)
add(tmp, rdo_sample_sizes)
tmp <- gframe("Distance between Groups", container=group)
add(tmp, distance_slider, expand=TRUE)

tmp <- gframe("Query Methods", container=group)
add(tmp, cbo_query_methods)


# Now to add a graphics device.
add(BigGroup, ggraphics())
