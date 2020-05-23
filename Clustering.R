#CLUSTERING
#----------------------------------------------------------------------------------
#CREATE A RANDOM DATA SET:
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

plot(x)#random 2-D data set
#----------------------------------------------------------------------------------
#K-MEANS CLUSTERNG:
#------------------
km.out=kmeans(x,        #data
              2,        #k-values
              nstart=20)#number of random start points

km.out$cluster
plot(x, 
     col=(km.out$cluster+1), 
     main="K-Means Clustering Results with K=2", 
     xlab="",
     ylab="",
     pch=20,
     cex=2)
#----------------------------------------------------------------------------------

set.seed(4)
km.out=kmeans(x,        #data
              3,        #k-values
              nstart=20)#number of random start points

km.out

plot(x, 
     col=(km.out$cluster+1), 
     main="K-Means Clustering Results with K=3", 
     xlab="",
     ylab="",
     pch=20,
     cex=2)
#----------------------------------------------------------------------------------
#STARTING WITH A SINGLE VS MULTIPLE:

set.seed(3)
km.out=kmeans(x,        #data
              3,        #k-values
              nstart=1)#number of random start points
km.out$tot.withinss
# km.out$tot.withinss is the total within-cluster sum of squares,
# which we seek to minimize by performing K-means clustering

km.out=kmeans(x,        #data
              3,        #k-values
              nstart=20)#number of random start points
km.out$tot.withinss
#----------------------------------------------------------------------------------
#HIGHER K VALUES:

km.out=kmeans(x,        #data
              10,        #k-values
              nstart=50)#number of random start points
km.out$tot.withinss
plot(x, 
     col=(km.out$cluster+1), 
     main="K-Means Clustering Results with K=10", 
     xlab="",
     ylab="",
     pch=20,
     cex=2)

#----------------------------------------------------------------------------------
#HIERARCHICAL CLUSTERNG:
#-----------------------

hc.complete = hclust(dist(x),            #Euclidean distance as a dissimilarity measure
                     method = "complete")#Complete linkage method

hc.average = hclust(dist(x),            #Euclidean distance as a dissimilarity measure
                     method = "average")#average linkage method

hc.single = hclust(dist(x),            #Euclidean distance as a dissimilarity measure
                     method = "single")#single linkage method

#PLOT THE DENDOGRAMS
plot(hc.complete,
     main="Complete Linkage", 
     xlab="",
     sub="",
     cex=.9)
plot(hc.average,
     main="Average Linkage",
     xlab="",
     sub="",
     cex=.9)
plot(hc.single,
     main="Single Linkage",
     xlab="",
     sub="",
     cex=.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), 
     main="Hierarchical Clustering with Scaled Features")

#clustering on a 3-D data set
x=matrix(rnorm(30*3), ncol=3)

dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), 
     main="Complete Linkage with Correlation-Based Distance",
     xlab="",
     sub="")

#---------------------------------------------------------------------------------
library(ISLR)
#---------------------------------------------------------------------------------
# Description
# NCI microarray data. The data contains expression levels on 6830 genes from 64 cancer cell lines. Cancer type is also recorded.
# 
# Usage
# NCI60
# Format
# The format is a list containing two elements: data and labs.
# 
# data is a 64 by 6830 matrix of the expression values while labs is a vector listing the cancer types for the 64 cell lines.
# 
# Source
# The data come from Ross et al. (Nat Genet., 2000). More information can be obtained at http://genome-www.stanford.edu/nci60/
#   
#   References
# James, G., Witten, D., Hastie, T., and Tibshirani, R. (2013) An Introduction to Statistical Learning with applications in R, www.StatLearning.com, Springer-Verlag, New York
# 
# Examples
# table(NCI60$labs)
#---------------------------------------------------------------------------------

nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
dim(nci.labs)
#---------------------------------------------------------------------------------
#PRINCIPAL COMPONENET ANALYSIS ON NCI:
#---------------------------------------------------------------------------------

pr.out = prcomp(nci.data, 
               scale=TRUE)
Cols = function(vec) {
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

plot(pr.out)

#proportion of variance
PVE = pr.out$sdev^2/sum(pr.out$sdev^2)
plot(PVE, 
     xlab = "Princpal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1),
     type = 'b')

#cumulative PVE
plot(cumsum(PVE), 
     xlab = "Princpal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1),
     type = 'b')

#---------------------------------------------------------------------------------
#CLUSTERING ON NCI:
#---------------------------------------------------------------------------------
#HEIRARCHIAL CLUSTERING:
sd.data=scale(nci.data) # SCALE THE DATA

data.dist=dist(sd.data) #USING THE EUCLIDEAN DISTANCE AS THE DISSIMILARITY MEASURE

#DENDOGRAMS FOR THE H CLUST:
plot(hclust(data.dist), 
     labels=nci.labs, 
     main="Complete Linkage", 
     xlab="", 
     sub="",
     ylab="")

plot(hclust(data.dist, method="average"),
     labels=nci.labs,
     main="Average Linkage",
     xlab="",
     sub="",
     ylab="")

plot(hclust(data.dist, method="single"),
     labels=nci.labs,
     main="Single Linkage",
     xlab="",
     sub="",
     ylab="")

hc.out = hclust(dist(sd.data)) #COMPLETE LINKAGE
hc.clusters = cutree(hc.out,4) #REVISED CLUSTERING WITH A LIMITED HEIGHT OF 4
table(hc.clusters,nci.labs)

plot(hc.out, labels=nci.labs)
abline(h=139, col="red")
hc.out

#---------------------------------------------------------------------------------
#K-MEANS CLUSTERING:
set.seed(2)
km.out=kmeans(sd.data,
              4,
              nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)

#---------------------------------------------------------------------------------
#CLUSTERING AND PCA:
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out,
     labels=nci.labs,
     main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs)
