getDatasetList <- function() {

	data_list <- data.frame(data()[3])
	data_list <- do.call(rbind, strsplit(as.character(data_list$results.Item), " "))[,1]

	return(data_list)

}

