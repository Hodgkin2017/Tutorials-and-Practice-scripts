###############
## Heat maps Tutorials
################

##examples of pHeatmaps:
hmcol <- colorRampPalette(rev(brewer.pal(9, "PuOr")))(255)
hmcol

pheatmap(dists, col = rev(hmcol), clustering_distance_rows = "manhattan",
         clustering_distance_cols = "manhattan")

##############
## pHeatmap (https://www.rdocumentation.org/packages/pheatmap/versions/1.0.8/topics/pheatmap)

library(pheatmap)

# Create test matrix
test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")
test

# Draw heatmaps
pheatmap(test)
pheatmap(test, kmeans_k = 2)
pheatmap(test, scale = "row", clustering_distance_rows = "correlation")
pheatmap(test, color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
pheatmap(test, cluster_row = FALSE)
pheatmap(test, legend = FALSE)

# Show text within cells
pheatmap(test, display_numbers = TRUE)
pheatmap(test, display_numbers = TRUE, number_format = "\%.1e")
pheatmap(test, display_numbers = matrix(ifelse(test > 5, "*", ""), nrow(test)))
pheatmap(test, cluster_row = FALSE, legend_breaks = -1:4, legend_labels = c("0",
                                                                            "1e-4", "1e-3", "1e-2", "1e-1", "1"))

# Fix cell sizes and save to file with correct size
pheatmap(test, cellwidth = 15, cellheight = 12, main = "Example heatmap")
pheatmap(test, cellwidth = 15, cellheight = 12, fontsize = 8, filename = "test.pdf")

# Generate annotations for rows and columns
annotation_col = data.frame(
  CellType = factor(rep(c("CT1", "CT2"), 5)),
  Time = 1:5
)
rownames(annotation_col) = paste("Test", 1:10, sep = "")

annotation_row = data.frame(
  GeneClass = factor(rep(c("Path1", "Path2", "Path3"), c(10, 4, 6)))
)
rownames(annotation_row) = paste("Gene", 1:20, sep = "")

annotation_row

# Display row and color annotations
pheatmap(test, annotation_col = annotation_col)
pheatmap(test, annotation_col = annotation_col, annotation_legend = FALSE)
pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row)


# Specify colors
ann_colors = list(
  Time = c("white", "firebrick"),
  CellType = c(CT1 = "#1B9E77", CT2 = "#D95F02"),
  GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
)

pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors, main = "Title")
pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row,
         annotation_colors = ann_colors)
pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors[2])

# Gaps in heatmaps
pheatmap(test, annotation_col = annotation_col, cluster_rows = FALSE, gaps_row = c(10, 14))
pheatmap(test, annotation_col = annotation_col, cluster_rows = FALSE, gaps_row = c(10, 14),
         cutree_col = 2)

# Show custom strings as row/col names
labels_row = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
               "", "", "Il10", "Il15", "Il1b")

pheatmap(test, annotation_col = annotation_col, labels_row = labels_row)

# Specifying clustering from distance matrix
drows = dist(test, method = "minkowski")
dcols = dist(t(test), method = "minkowski")
pheatmap(test, clustering_distance_rows = drows, clustering_distance_cols = dcols)

# Modify ordering of the clusters using clustering callback option
callback = function(hc, mat){
  sv = svd(t(mat))$v[,1]
  dend = reorder(as.dendrogram(hc), wts = sv)
  as.hclust(dend)
}

pheatmap(test, clustering_callback = callback)

############
## Heatmaps in R (http://sahirbhatnagar.com/heatmap)

library(RColorBrewer)
library(MASS)

##Simulate gene expression data function

sim.expr.data <- function(n, n0, p, rho.0, rho.1){
  # Initiate Simulation parameters
  # n: total number of subjects
  # n0: number of subjects with X=0
  # n1: number of subjects with X=1
  # p: number of genes
  # rho.0: rho between Z_i and Z_j when X=0
  # rho.1: rho between Z_i and Z_j when X=1
  
  # Simulate gene expression values according to exposure X=0, X=1, according to a centered multivariate normal distribution with covariance between Z_i and Z_j being rho^|i-j|
  times = 1:p # used for creating covariance matrix
  H <- abs(outer(times, times, "-"))
  V0 <- rho.0^H
  V1 <- rho.1^H
  
  # rows are people, columns are genes
  genes0 <- MASS::mvrnorm(n = n0, mu = rep(0,p), Sigma = V0)
  genes1 <- MASS::mvrnorm(n = n1, mu = rep(0,p), Sigma = V1)
  genes <- rbind(genes0,genes1)
  return(genes)
}


## Create simulated gene expression data

n = 100 ; n0 = 50 ; n1 = 50; p = 100
genes <- sim.expr.data(n = 100, n0 = 50, p = 100, 
                       rho.0 = 0.01, rho.1 = 0.95)
genes

colnames(genes) <- paste0("Gene", 1:p)
rownames(genes) <- paste0("Subject", 1:n)
genes[1:5, 1:5]

##Pick colour scheme

RColorBrewer::display.brewer.all()

col.pal <- RColorBrewer::brewer.pal(9, "Reds")
(col.pal <- RColorBrewer::brewer.pal(9, "Reds"))

##Create a dataframe with details of the samples in the 
#heatmap e.g. control v's treated. This will be used for contrasting

annotation_col<- data.frame(
  Exposure = factor(c(rep("X=0", n0), c(rep("X=1", n1)))),
  Type = factor(sample(c("T-cell", "B-cell"), n, replace = T))
)
annotation_col

rownames(annotation_col) <- paste0("Subject", 1:n)
head(annotation_col)

annotation_row<- data.frame(
  Pathway = factor(rep(1:4, each = 25))
)

annotation_row

rownames(annotation_row)<- paste0("Gene", 1:n)
head(annotation_row)


##Clustering heatmap with pHeatmap

head(genes)

pheatmap(t(genes),
         cluster_rows = T,
         cluster_cols = F,
         annotation_col = annotation_col,
         annotation_row = annotation_row,
         color = col.pal,
         fontsize = 6.5,
         fontsize_row = 6,
         fontsize_col = 6,
         gaps_col = 050
         )

##Interactive heatmaps using d3heatmaps

library(d3heatmap)
library(webshot)

?d3heatmap
d3heatmap(t(genes), colors = "Reds", Colv = FALSE)

##Interactive heatmaps using plotly

expression<- t(genes)
plotly::plot_ly(z= expression, colorscale = col.pal, type = "heatmap")

##Heatmap with baseR





##Heatmap with lattice




##Heatmap with ggplot2






