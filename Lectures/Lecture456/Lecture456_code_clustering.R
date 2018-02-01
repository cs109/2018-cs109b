# load libraries
library(ggplot2)
library(ggfortify)
library(factoextra)
library(cluster)
library(corrplot)
library(NbClust)
library(mclust)
library(dbscan)
library(MASS) 
library(TeachingDemos)  # to load "faces2"

# Principal components
states=row.names(USArrests)
pr.out=prcomp(USArrests, scale=TRUE)
pr.out$rotation=-pr.out$rotation  # unneeded, but makes biplot arrows point to the right
postscript(file="usarrests-biplot.ps",hori=T)
par(pty='s')
pr.out$x=-pr.out$x
biplot(pr.out, scale=0,cex=c(0.5, 1),
       main="PCA biplot for scaled USArrests data\n")
dev.off()
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var # can see the variances of each PC
pve=pr.var/sum(pr.var)
pve  # proportion variance explained by each PC
postscript(file="pve-plots.ps",hori=T)
par(pty='s',mfrow=c(1,2),mar=c(5,4,4,2) + 0.1,oma = c(0, 0, 2, 0))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
mtext("USArrests - proportion variance explained",outer=T, cex=1.5)
par(pty='m',mfrow=c(1,1))
dev.off()

# Old Faithful
ggplot(faithful, aes(x=eruptions, y=waiting)) +
  geom_point() +  
  ggtitle("Old Faithful geyser eruptions")

ggplot(faithful, aes(x=eruptions, y=waiting)) +
  geom_point() +  
  geom_density2d() + # Add 2d density estimation
  ggtitle("Old Faithful geyser eruptions")

# Load the dataset
data(USArrests)
# Subset of the data
set.seed(123)
ss = sample(1:50, 10) # Take 10 random rows
arrests = USArrests[ss, ] # Subset the 10 rows
# View the firt 6 rows of the data
head(arrests, n = 6)

arrests.scaled = scale(arrests)
head(round(arrests.scaled, 2))

# principal components plot
autoplot(prcomp(scale(USArrests))) + 
  ggtitle("First two principal components of violent crimes data")

# Compute Euclidean pairwise distances
dist.eucl = daisy(arrests.scaled, metric = "euclidean")
# View a subset of the distance matrices
round(as.matrix(dist.eucl)[1:6, 1:6], 1)

# Euclidean distance
corrplot(as.matrix(dist.eucl), is.corr = FALSE, method = "color",
order="hclust", type = "lower")

res.dist = daisy(USArrests, metric="euclidean",stand=T)
fviz_dist(res.dist,
   gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

set.seed(123)
arrests.km = kmeans(scale(USArrests), 4, nstart = 25)
print(arrests.km)
print(aggregate(USArrests, by=list(cluster=arrests.km$cluster), mean))

fviz_cluster(arrests.km, data = scale(USArrests),
  main="K-means clustering of violent crime data")

arrests.pam = pam(scale(USArrests), k=4)
print(arrests.pam)

fviz_cluster(arrests.pam, # don't need to specify data for pam
  main="PAM clustering of violent crime data")

# silhouette plot
fviz_silhouette(silhouette(arrests.pam),
  main="Silhouette plot for PAM clustering")

# Compute silhouette
sil = silhouette(arrests.pam)[, 1:3]
# Objects with negative silhouette
neg_sil_index = which(sil[, 'sil_width'] < 0)
print(sil[neg_sil_index, , drop = FALSE])

# example hierarchical cluster - just for show
plot(hclust(dist(scale(as.matrix(mtcars))[c(3,8,10,15,20),])),
  xlab="Cars",main="Hierarchical clustering example",sub='')

# agnes and diana for arrests data
arrests.agnes = agnes(USArrests,method="ward",stand=T)
pltree(arrests.agnes, cex=0.5, hang= -1,
  main="AGNES fit (Ward's method) of violent crimes data",
  xlab="State",sub="")
arrests.diana = diana(USArrests,stand=T)
pltree(arrests.diana, cex=0.5, hang= -1,
  main="DIANA fit of violent crimes data",
  xlab="State",sub="")
pltree(arrests.agnes, cex=0.5, hang= -1,
  main="AGNES fit (Ward's method) of violent crimes data",
  xlab="State",sub="")
rect.hclust(arrests.agnes,k=4,border=2:5)

# principal components plot with groupings
grp.agnes = cutree(arrests.agnes, k=4)
fviz_cluster(list(data = scale(USArrests), cluster = grp.agnes),
  main="AGNES fit - 4 clusters")
grp.diana = cutree(arrests.diana, k=4)
fviz_cluster(list(data = scale(USArrests), cluster = grp.diana),
  main="DIANA fit - 4 clusters")

# elbow method for selecting clusters from kmeans, PAM
fviz_nbclust(scale(USArrests), kmeans, method="wss") +
  ggtitle("K-means clustering for violent crimes - optimal number of clusters")
fviz_nbclust(scale(USArrests), kmeans, method="wss") + 
  ggtitle("K-means clustering for violent crimes - optimal number of clusters") +
  geom_vline(xintercept=4,linetype=2)
fviz_nbclust(scale(USArrests), pam, method="wss") + 
  ggtitle("PAM clustering for violent crimes - optimal number of clusters") +
  geom_vline(xintercept=4,linetype=2)

# silhouette method for selecting clusters from kmeans, PAM
fviz_nbclust(scale(USArrests),kmeans,method="silhouette") +
  ggtitle("K-means clustering for violent crimes - optimal number of clusters") 

fviz_nbclust(scale(USArrests),pam,method="silhouette") +
  ggtitle("PAM clustering for violent crimes - optimal number of clusters") 

# Gap statistic
gapstat = clusGap(scale(USArrests),FUN=kmeans,nstart=25,d.power=2,K.max=10,B=500)
print(gapstat, method="Tibs2001SEmax")
fviz_gap_stat(gapstat, 
  maxSE=list(method="Tibs2001SEmax",SE.factor=1)) + 
  ggtitle("K-means clustering for violent crimes - optimal number of clusters") 

gapstat = clusGap(scale(USArrests),FUN=pam,d.power=2,K.max=10,B=500)
print(gapstat, method="Tibs2001SEmax")
fviz_gap_stat(gapstat,
  maxSE=list(method="Tibs2001SEmax",SE.factor=1)) + 
  ggtitle("PAM clustering for violent crimes - optimal number of clusters") 

# histogram of optimal number of clusters based on 30 measures
nb.arrests = NbClust(scale(USArrests),distance="euclidean",
  min.nc=2, max.nc=9, method="ward.D2",index="all")
print(nb.arrests)
fviz_nbclust(nb.arrests) + theme_minimal()

# Fuzzy clustering - fanny (4 clusters)
arrests.fanny = fanny(scale(USArrests),k=4)
print(head(round(arrests.fanny$membership,3),15))
fviz_cluster(arrests.fanny,
  main="FANNY fit - 4 clusters")
 
corrplot(arrests.fanny$membership[1:15,], is.corr=F)

# model-based clustering
faithful.mc = Mclust(faithful)
print(summary(faithful.mc))
# optimal number of clusters
print(faithful.mc$G)
# estimated probability for an observation to be in each cluster
print(head(round(faithful.mc$z,3)))
fviz_cluster(faithful.mc, frame.type="norm", geom="point") +
  ggtitle("Old Faithful - Bivariate normal mixture model G=3")

# plot multishapes data
data(multishapes)  # in factoextra
plot(multishapes[,1], multishapes[, 2], pch = 19, 
  xlab="X",ylab="Y",main="Multishapes data")
# k-means clustering of these data with K=5
ms = multishapes[, 1:2]
set.seed(123)
ms.kmeans = kmeans(ms, 5, nstart = 25)
fviz_cluster(ms.kmeans, ms, ellipse = FALSE, geom = "point") +
  ggtitle("K-means clustering with K=5 of multishape data")

set.seed(123)
kNNdistplot(ms, k=5)  
abline(0.15,0,lty=2,lwd=2,col="red") # added after seeing kNN plot
title(sub="Knee at around y=0.15",main="Knee plot for multishapes data")
ms.db = dbscan(ms, eps=0.15, minPts = 5)
fviz_cluster(ms.db, ms, stand = FALSE, ellipse = FALSE, geom = "point") +
  ggtitle("DBSCAN clustering of multishape data with eps=0.15 and minPts=5")

kNNdistplot(scale(USArrests),k=4)
abline(1.4,0,lty=2,lwd=2,col="red") # added after seeing kNN plot
title(sub="Knee at around y=1.4",main="Knee plot for violent crimes data")
arrests.db = dbscan(scale(USArrests),eps=1.4, minPts=4)
fviz_cluster(arrests.db, scale(USArrests), 
  stand = FALSE, ellipse.type = "convex", geom = "point") +
  ggtitle("DBSCAN clustering of violent crime data with eps=1.4 and minPts=4")

# Chernoff faces
car.mat = as.matrix(Cars93[,
c("Weight","MPG.city","MPG.highway",
  "Horsepower","EngineSize","Min.Price","Price","Max.Price",
  "RPM","Rev.per.mile","Fuel.tank.capacity","Passengers",
  "Length","Wheelbase","Width","Turn.circle")])
dimnames(car.mat)[[1]] = as.character(Cars93$Make)
print(summary(car.mat))
faces2(car.mat[1:40,], nrows=4, ncols=10)
