getColumnsType <- function(data) {

	#Get datatype of each column
	header <- names(data)

	numeric_list <- NULL
	non_numeric_list <- NULL
	others_list <- NULL

	for(col in header) {

		ifelse(is.ts(data[, col]), others_list <- c(others_list, col),
			ifelse(is.factor(data[, col]), non_numeric_list <- c(non_numeric_list, col),
				ifelse(is.numeric(data[, col]), numeric_list <- c(numeric_list, col), non_numeric_list <- c(non_numeric_list, col))
			)
		)

	}

	return(list(numeric_list, non_numeric_list, others_list))

}

