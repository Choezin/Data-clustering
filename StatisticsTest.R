##############################################################################################

statisticsRegression <- function(data, response = NULL) {

	if(is.null(response)) {

		col_types <- getColumnsType(data)
		numeric_list <- unlist(col_types[1])
		response <- numeric_list[1]

	}

	formula <- paste(response, " ~ .", sep = "")
	reg <- lm(formula, data = data)

	return(reg)

}

##############################################################################################

statisticsRegression_step <- function(reg_model) {

	result <- step(reg_model, direction="both")

	return(result)

}

##############################################################################################

statisticsRegression_outlier <- function(reg_model) {

	result <- outlierTest(reg_model, n.max = 100)

	return(result)

}

##############################################################################################

statisticsTtest <- function(data, var1, var2) {

	result <- t.test(data[, var1], data[, var2])

	return(result)

}

##############################################################################################

statisticsOnewayANOVA <- function(data, group, response) {

	formula <- paste(response, " ~ ", group, sep = "")
	result <- aov(as.formula(formula), data = data)

	return(result)

}

##############################################################################################

statisticsOnewayANOVA_tukey <- function(anova) {

	result <- TukeyHSD(anova)

	return(result)

}

##############################################################################################

statisticsMANOVA <- function(data , groups, responses) {

	formula <- "cbind("

	for(response in responses) {

		if(match(response, responses) == length(responses)) formula <- paste(formula, response, sep= "") else formula <- paste(formula, response, ", ", sep= "")
	
	}

	formula <- paste(formula, ") ~ ", sep = "")

	for(group in groups) {

		if(match(group, groups) == length(groups)) formula <- paste(formula, group, sep = "") else formula <- paste(formula, group, " * ", sep = "")

	}

	result <- manova(as.formula(formula), data = data)

}

##############################################################################################

