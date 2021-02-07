# define dendrogram object to play with:
hc <- hclust(dist(iris), "complete")
dend <- as.dendrogram(hc)

library(dendextend)
par(mfrow = c(1,2), mar = c(5,2,1,0))
dend <- dend %>%
  color_branches(k = 3) %>%
  set("branches_lwd", c(2,1,2)) %>%
  set("branches_lty", c(1,2,1))


depth.cutoff <- 2.5
plot(dend,horiz = TRUE)
abline(v=depth.cutoff,col="red",lty=2)



dend <- color_labels(dend, k = 3)
# The same as:
# labels_colors(dend)  <- get_leaves_branches_col(dend)
plot(dend)

