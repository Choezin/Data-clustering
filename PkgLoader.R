##############################################################################################
#Used to load packages. If the packages is not exist, it will download packages automatically
#
#Input: Type = character, name of package.
#
#Output: NULL

pkgLoad <- function(package) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package, dep=TRUE, repos = "http://cran.csie.ntu.edu.tw/")
        if(!require(package, character.only = TRUE)) stop("Package not found")
    }
    library(package, character.only=TRUE)
}

pkgLoad("car")

pkgLoad("stats")
pkgLoad("mclust")
pkgLoad("dbscan")
pkgLoad("kknn")
pkgLoad("dendextend")



pkgLoad("rpart")
pkgLoad("randomForest")
pkgLoad("kknn")
pkgLoad("e1071")
pkgLoad("nnet")

pkgLoad("ggplot2")
pkgLoad("gridExtra")
pkgLoad("mlearning")
pkgLoad("caret")
pkgLoad("GGally")



