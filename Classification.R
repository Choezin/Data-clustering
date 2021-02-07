##############################################################################################

classificationDecisionTree <- function(data, class = names(data)[length(names(data))]) {

    formula <- paste(class, " ~ .", sep = "")
    model <- rpart(as.formula(formula), data)

    return(model)

}

##############################################################################################

evaluationDecissionTree <- function(model, testing_dataset) {

    if(model$cptable[2] != 0) {

        testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")

    }

    return(testing_dataset)

}

##############################################################################################

classificationRandomForest <- function(data, class = names(data)[length(names(data))]) {

    formula <- paste(class, " ~ .", sep = "")
    model <- randomForest(as.formula(formula), data)

    return(model)

}

##############################################################################################

evaluationRandomForest <- function(model, testing_dataset) {

    testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")

    return(testing_dataset)

}

##############################################################################################

classificationKNN <- function(training_dataset, testing_dataset, class = names(training_dataset)[length(names(training_dataset))], k = 7, distance = 2) {

    formula <- paste(class, " ~ .", sep = "")
    model <- kknn(as.formula(formula), training_dataset, testing_dataset, k = k, distance = distance)

    return(model)

}

##############################################################################################

classificationSVM <- function(data, class = names(data)[length(names(data))]) {

    formula <- paste(class, " ~ .", sep = "")
    model <- svm(as.formula(formula), data = data)

    return(model)

}

##############################################################################################

evaluationSVM <- function(model, testing_dataset) {

    testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")

    return(testing_dataset)

}

##############################################################################################

classificationNaiveBayes <- function(data, class = names(data)[length(names(data))]) {

    formula <- paste(class, " ~ .", sep = "")
    model <- naiveBayes(as.formula(formula), data = data)

    return(model)

}

##############################################################################################

evaluationNaiveBayes <- function(model, testing_dataset) {

    testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")

    return(testing_dataset)

}

##############################################################################################

classificationNN <- function(data, class = names(data)[length(names(data))], size = 10) {

    formula <- paste(class, " ~ .", sep = "")
    model <- nnet(as.formula(formula), data = data, size = size)

    return(model)

}

##############################################################################################

evaluationNN <- function(model, testing_dataset) {

    testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")

    return(testing_dataset)

}

##############################################################################################

evaluationConfusionMatrix <- function(predict_result, class) {

    confusion_matrix <- confusion(factor(predict_result), factor(class))
    temp <- confusionMatrix(factor(predict_result), factor(class))
    overall <- temp$overall
    byClass <- temp$byClass
    theme <- ttheme_default(core = list(fg_params=list(cex = 2.0)), colhead = list(fg_params=list(cex = 3.0)), rowhead = list(fg_params=list(cex = 3.0)))

    df <- data.frame(x = 1:10, y = 1:10)
    plot <- ggplot(df, aes(x, y)) + geom_blank() + theme_bw()
    plot <- plot + annotation_custom(tableGrob(confusion_matrix, theme = theme), xmin = 0, xmax = Inf, ymin = -Inf, ymax = 10)
    plot <- plot + theme(
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()
    )

    theme <- ttheme_default(core = list(fg_params=list(cex = 1.2)), colhead = list(fg_params=list(cex = 1.3)), rowhead = list(fg_params=list(cex = 1.3)))

    byClass_plot <- ggplot(df, aes(x, y)) + geom_blank() + theme_bw()
    byClass_plot <- byClass_plot + annotation_custom(tableGrob(byClass, theme = theme), xmin = 0, xmax = Inf, ymin = -Inf, ymax = 10)
    byClass_plot <- byClass_plot + theme(
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()
    )

    return(list(plot, confusion_matrix, overall, byClass, byClass_plot))

}

##############################################################################################

