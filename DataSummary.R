dataSummary_numreic <- function(data) {

    if(is.data.frame(data)) {

        #Get datatype of each column
        col_types <- getColumnsType(data)
        numeric_list <- unlist(col_types[1])

        #Numeric

        mean_list <- NULL
        sd_list <- NULL
        min_list <- NULL
        Q1_list <- NULL
        median_list <- NULL
        Q3_list <- NULL
        max_list <- NULL
        IQR_list <- NULL
        N_list <- NULL

        for(col in numeric_list) {

            mean_list <- c(mean_list, mean(data[, col], na.rm = TRUE))
            sd_list <- c(sd_list, sd(data[, col], na.rm = TRUE))

            quantile <- quantile(data[, col], na.rm = TRUE)

            min_list <- c(min_list, quantile[1])
            Q1_list <- c(Q1_list, quantile[2])
            median_list <- c(median_list, quantile[3])
            Q3_list <- c(Q3_list, quantile[4])
            max_list <- c(max_list, quantile[5])

            IQR_list <- c(IQR_list, IQR(data[, col], na.rm = TRUE))
            N_list <- c(N_list, length(data[, col]) - sum(is.na(data[, col])))

        }

        numeric_summary <- data.frame(numeric_list, mean_list, sd_list, min_list, Q1_list, median_list, Q3_list, max_list, IQR_list, N_list)
        names(numeric_summary) <- c("Attribute", "Mean", "Sd", "Min", "25%", "Median", "75%", "Max", "IQR", "N")

        correlation_matrix <- cor(data[, numeric_list], use="complete")


        return(list(numeric_summary, correlation_matrix))

    }else {

        return(NULL)

    }

}

##############################################################################################

dataSummary_non_numeric <- function(data) {

    if(is.data.frame(data)) {

        #Get datatype of each column
        col_types <- getColumnsType(data)
        non_numeric_list <- unlist(col_types[2])

        non_numeric_summary <- NULL

        for(col in non_numeric_list) {

            temp <- data.frame(table(data[, col]))
            names(temp) <- c(col, "Freq")
            non_numeric_summary <- if(is.null(non_numeric_summary)) list(temp) else list(non_numeric_summary, list(temp))

        }

        return(non_numeric_summary)

    }else {

        return(NULL)

    }

}

##############################################################################################

