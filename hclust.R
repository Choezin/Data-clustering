i <- iris
names(i)
ic <- i[, c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")]
hclust(ic)

risingstars <- dist(ic)
plot(hclust(risingstars))


