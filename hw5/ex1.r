library(RUnit)
errMsg <- function(err) print(err)
load('ex1-tests.rda')

# Implement the function sigComponents. Your function should take the
# following arguments:
#
# <pca>: an object of class prcomp (i.e. the entire output of the function
#   prcomp)
# <var.level>: a numeric value indicating the variance cutoff for which a
#   principal component is considered "significant"
#
# Your function should return a single value indicating the number of principal
# components in <pca> whose associated variance exceeds <var.level>

sigComponents <- function(pca, var.level) {

    # your code here
    return(sum((pca$sdev)^2>=var.level))

    
}

tryCatch(checkEquals(2, sigComponents(iris.pca, 0.9)), error=function(err)
         errMsg(err)) 


# Implement the function varProportion. Your function should take the following
# arguments:
#
# <pca>: an object of class prcomp
#
# Your function should return a numeric vector giving the cumulative
# proportion of variance explained by each principal component (i.e the
# first element of the vector gives the proportion of variance explained by
# pc1, the second the proportion explained by pc1 and pc2 combined, etc.)

varProportion <- function(pca) {

    # your code here
    x=pca$sdev
    x=x^2
    y=1:length(x)
    for(i in 1:length(x)){
    	y[i]=sum(x[1:i])/sum(x)
    }

	return(y)
}

tryCatch(checkEquals(var.proportion.t, varProportion(iris.pca)),
         error=function(err) errMsg(err))


# Implement the funciton reduceData. Your function should take the
# following arguments:
#
# <data>: a numeric matrix or dataframe whose rows represent observations
#   and columns variables
# <var.level>: a numeric value indicating the variance for which a
#   principal component will be considered significant.
# <scale>: a logical indicating whether variables should be scaled in prcomp
# <center>: a logical indicating whether variables should be centered in
#   prcomp
#
# Your function should run a principal component analysis on data (scaling
# and centering when indicated) and determine the number of principal
# components whose variance exceeds <var.level>. Your function should
# reduce the dimension of the dataset using this number of principal
# components. 
# ***If none of the component variances exceed <var.level>, your function
# should return integer(0)***

reduceData <- function(data, var.level, scale=T, center=T) {

    # your code here
    data.pca=prcomp(data,scale=scale,center=center)
    idcs=(data.pca$sdev)^2>=var.level
    if(sum(idcs)==0){
    	return(integer(0))
    } else{
    	return(data.pca$x[,idcs])
    }
    

}


tryCatch(checkTrue(!length(reduceData(iris.data, 5))), error=function(err)
         errMsg(err))

tryCatch(checkEquals(reduce.data.t, reduceData(iris.data, 0.9)),
         error=function(err) errMsg(err))

# In this folder you will find the file "wines.csv". The data contains
# measurements of the concentrations of 13 different chemicals in wines
# various wines produced by 3 different types of grapes. The first column
# of this dataset indicated the type of grape used to produce each wine,
# while the remaining columns give the chemical concentrations.
# Use your previously defined functions along with R's built in functions
# to do the following:
# 1) load the wines.csv dataset
# 2) run a principal component analysis. Use the output to transform your
#   data, and reduce the dimension to include only the PCs whose associated
#   variance exceeds 1 (this should reduce the data to 3 variables). Call
#   this reduced dataset <wine.reduced>.
# 3) run k-means clustering on your reduced data with k=3 and
#   max.iters=10. ***Make sure to set your seed to 47 before doing
#   this***. Store the resulting cluster labels from k-means as the
#   variable <cluster.labels.k>
# 4) run hierarchical clustering on your reduced data. Plot the resulting
#   dendrogram. Cut the tree so that you have 3 clusters and store the
#   resulting cluster labels as the variable <cluster.labels.h>


wine.data=read.csv('wines.csv')
# wine.reduced <- your code here
wine.reduced=reduceData(wine.data,1)

# cluster.labels.k <- your code here, set seed to 47 before running the
# algorithm
set.seed(47)
k.means=kmeans(wine.reduced,center=3,iter.max=10)
cluster.labels.k=k.means$cluster

# cluster.labels.h <- your code here
plot(hclust(dist(wine.reduced)))
cluster.labels.h=cutree(hclust(dist(wine.reduced)),k=3)


# Implement the helper function plotClusters. Your function should take the
# following arguments:
#
# <data>: a numeric matrix or dataframe whose observations belong to
#   certain clusters
# <varibles>: a length 2 numeric vector indicating the two variables of
#   <data> that should be plotted against one another
# <cluster.labels>: a numeric vector, whose length is equal to the number of
#   rows of data, giving the cluster label for each observation
# <...>: any additional plotting parameters
#
# Your function should plot the two variables of <data> given by <variables>
# against one another. The points in your plot should be colored using the
# "Set1" palette in RColorBrewer (you may assume that the number of
# clusters is less than the total number of colors in this
# palette).
#***Your function will need to check that the conditions on the length of
#<variables> and <cluster.labels> hold. If they do not, your function
#should run the command "stop", printing the error messages:
# "len variables > 2" and "incompatible dimensions" respectively.


plotClusters <- function(data, variables, cluster.labels, ...) {
    
    # your code here
	if(length(variables)!=2) stop('len variables > 2')
	if(length(cluster.labels)!=nrow(data)) stop('incompatible dimensions')
	plot(data[,variables],col=brewer.pal(max(cluster.labels),"Set1")[cluster.labels], ...)

}



tryCatch(checkException(plotClusters(iris.data, 1:2, 1:10), silent=T),
         error=function(err) errMsg(err))
tryCatch(checkException(plotClusters(iris.data, 1:3, 1:nrow(iris.data)),
                        silent=T), error=function(err) errMsg(err))



# Use your plotClusters function to create a plot containing 9 plots
# (3x3). The first row (of 3 plots) whould plot PC1 against PC2, PC2
# against PC3, and PC1 against PC3 and be colored based on the cluster
# labels from k-means. Set the title of all first row plots should be
# "k-means" and pch to 20. The second and third rows should plot the same
# variables but color the points by the hierarchical clustering labels and
# the true labels (from col 1 of wines.csv) respectively. Change the titles
# of each of these rows accordingly but keep the pch as 20.

par(mfrow=c(3,3))
library(RColorBrewer)

plotClusters(wine.reduced,c(1,2),cluster.labels.k,main="k-means",pch=20)
plotClusters(wine.reduced,c(2,3),cluster.labels.k,main="k-means",pch=20)
plotClusters(wine.reduced,c(1,3),cluster.labels.k,main="k-means",pch=20)

plotClusters(wine.reduced,c(1,2),cluster.labels.h,main="hcluster",pch=20)
plotClusters(wine.reduced,c(2,3),cluster.labels.h,main="hcluster",pch=20)
plotClusters(wine.reduced,c(1,3),cluster.labels.h,main="hcluster",pch=20)

plotClusters(wine.reduced,c(1,2),wine.data[,1],main="truelabel",pch=20)
plotClusters(wine.reduced,c(2,3),wine.data[,1],main="truelabel",pch=20)
plotClusters(wine.reduced,c(1,3),wine.data[,1],main="truelabel",pch=20)

