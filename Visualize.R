##############################################################################################

plotPairs <- function(data, class = NULL, non_numeric = FALSE) {

	col_types <- getColumnsType(data)
	numeric_list <- unlist(col_types[1])
	non_numeric_list <- unlist(col_types[2])
	others_list <- unlist(col_types[3])

	for(col in others_list) {
		data[, col] <- NULL
	}

	if(!non_numeric) {
		for(col in  non_numeric_list) {
			data[, col] <- NULL
		}
	}

	if(is.null(class)) {

		plot <- ggpairs(data) 

	}else {

		plot <- ggpairs(data, mapping = aes_string(col = class, shape = class), legends = TRUE, upper = "blank")

		columns <- names(data)
		for (i in c(1:length(columns))) {

			# Address only the diagonal elements
			# Get plot out of matrix
			inner <- getPlot(plot, i, i);

			# Add any ggplot2 settings you want (blank grid here)
			inner <- inner + theme(panel.grid = element_blank()) + theme(axis.text.x = element_blank())

			# Put it back into the matrix
			plot <- putPlot(plot, inner, i, i)

			for (j in c(1:length(columns))){

				if(i == ceiling(length(columns) / 2) && j == 1){

					# Move legend right
					inner <- getPlot(plot, i, j)
					inner <- inner + scale_shape_manual(values = c(1:nlevels(factor(data[, class])))) + theme(legend.position = c(length(columns) - 0.25, 0.50))
					if(columns[i] %in% numeric_list && columns[j] %in% numeric_list) inner <- inner + geom_point(size = 3)
					plot <- putPlot(plot, inner, i, j)

				}else if(i != j) {

					# Delete legend
					inner <- getPlot(plot, i, j)
					inner <- inner + scale_shape_manual(values = c(1:nlevels(factor(data[, class])))) + theme(legend.position="none")
					if(columns[i] %in% numeric_list && columns[j] %in% numeric_list) inner <- inner + geom_point(size = 3)
					plot <- putPlot(plot, inner, i, j)

				}else {

					inner <- getPlot(plot, i, j)
					inner <- inner + theme(legend.position="none")
					plot <- putPlot(plot, inner, i, j)

				}

			}
		}

	}

	return(plot)

}

##############################################################################################

plotClusteringResult <- function(data, class = names(data)[length(names(data))], legends = TRUE) {

	plot <- ggpairs(data, mapping = aes_string(col = class, shape = class), legends = legends, upper = "blank")


	columns <- names(data)
	col_types <- getColumnsType(data)
	numeric_list <- unlist(col_types[1])
	non_numeric_list <- unlist(col_types[2])

	if(legends) {

		for (i in c(1:length(columns))) {

			# Address only the diagonal elements
			# Get plot out of matrix
			inner <- getPlot(plot, i, i);

			# Add any ggplot2 settings you want (blank grid here)
			inner <- inner + theme(panel.grid = element_blank()) + theme(axis.text.x = element_blank())

			# Put it back into the matrix
			plot <- putPlot(plot, inner, i, i)

			for (j in c(1:length(columns))){

				if(i == ceiling(length(columns) / 2) && j == 1){

					# Move legend right
					inner <- getPlot(plot, i, j)
					inner <- inner + scale_shape_manual(values = c(1:nlevels(factor(data[, class])))) + theme(legend.position = c(length(columns) - 0.25, 0.50))
					if(columns[i] %in% numeric_list && columns[j] %in% numeric_list) inner <- inner + geom_point(size = 3)
					plot <- putPlot(plot, inner, i, j)

				}else if(i != j) {

					# Delete legend
					inner <- getPlot(plot, i, j)
					inner <- inner + scale_shape_manual(values = c(1:nlevels(factor(data[, class])))) + theme(legend.position="none")
					if(columns[i] %in% numeric_list && columns[j] %in% numeric_list) inner <- inner + geom_point(size = 3)
					plot <- putPlot(plot, inner, i, j)

				}else {

					inner <- getPlot(plot, i, j)
					inner <- inner + theme(legend.position="none")
					plot <- putPlot(plot, inner, i, j)

				}

			}
		}

	}

	return(plot)

}

##############################################################################################

plotClassificationResult <- function(data, class = names(data)[length(names(data))], legends = TRUE) {

	plot <- ggpairs(data, mapping = aes_string(col = class, shape = class), legends = legends, upper = "blank")

	columns <- names(data)
	col_types <- getColumnsType(data)
	numeric_list <- unlist(col_types[1])
	non_numeric_list <- unlist(col_types[2])

	if(legends) {

		for (i in c(1:length(columns))) {

			# Address only the diagonal elements
			# Get plot out of matrix
			inner <- getPlot(plot, i, i);

			# Add any ggplot2 settings you want (blank grid here)
			inner <- inner + theme(panel.grid = element_blank()) + theme(axis.text.x = element_blank())

			# Put it back into the matrix
			plot <- putPlot(plot, inner, i, i)

			for (j in c(1:length(columns))){

				if(i == ceiling(length(columns) / 2) && j == 1){

					# Move legend right
					inner <- getPlot(plot, i, j)
					inner <- inner + scale_shape_manual(values = c(1:nlevels(factor(data[, class])))) + theme(legend.position = c(length(columns) - 0.25, 0.50))
					if(columns[i] %in% numeric_list && columns[j] %in% numeric_list) inner <- inner + geom_point(size = 3)
					plot <- putPlot(plot, inner, i, j)

				}else if(i != j) {

					# Delete legend
					inner <- getPlot(plot, i, j)
					inner <- inner + scale_shape_manual(values = c(1:nlevels(factor(data[, class])))) + theme(legend.position="none")
					if(columns[i] %in% numeric_list && columns[j] %in% numeric_list) inner <- inner + geom_point(size = 3)
					plot <- putPlot(plot, inner, i, j)

				}else {

					inner <- getPlot(plot, i, j)
					inner <- inner + theme(legend.position="none")
					plot <- putPlot(plot, inner, i, j)

				}

			}
		}

	}

	return(plot)

}

##############################################################################################


