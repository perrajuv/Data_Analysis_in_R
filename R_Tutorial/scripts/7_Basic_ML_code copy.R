#---------------------------------------------#
#--------Data Analysis & Graphics in R--------#
#----------------Basic ML in R----------------#
#---K Means, KNN, PCA, LDA, SVM, NaiveBayes---#
#---------------------------------------------#

# First we need to load up some packages to support ML.
# Please install package if not already installed
#install.packages('MASS')
#install.packages('ggplot2')
#install.packages('scales')
#install.packages('gridExtra')
#install.packages('class')
#install.packages('e1071')

library(MASS)
library(ggplot2)
library(scales)
library(gridExtra)
library(class)
library(e1071)


#load data
data(iris)
View(iris)
summary(iris)

# Prepare training and testing data
testidx <- which(1:length(iris[,1])%%5 == 0)
iristrain <- iris[-testidx,]
iristest <- iris[testidx,]


# OK. Lets get to it

# K-Means

# kmeans(), built into the R base package, is an unsupervised
# learning technique. The goal is to cluster the observed data
# into groups. This is achieved by assuming a Euclidean distance
# metric, and finding points which lie at local centroids. All
# points are then assigned to their closest centroid and are
# thus clustered. The algorithmic approach to finding these
# centroids is to pick k points at random then assign all other 
# points to the centroids. The algorithm then chooses new
# centroids based on the mean point of the resulting clusters.
# Then with these new centroids, the remaining N-k points are
# reclustered. This repeats until some stopping condition is
# reached.
# Here we know, a priori, that there are 3 clusters, so we set 
# k = 3.

newiris <- iris
newiris$Species <- NULL
(kc <- kmeans(newiris, 3))
table(iris$Species, kc$cluster)
plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)

# K Nearest Neighbor

# In KNN we classify an unknown point by looking at k nearest
# neighbors of known classification. In the case of k=1, we
# find the closest point in our training set to a new point
# from our test set. We then assume that the new point has
# the same class as its closest neighbor in the test set.
# For k>1, we typically apply a voting mechanism to pick the
# modal class from the neighborhood of points in the training
# set. 

train_input <- as.matrix(iristrain[,-5])
train_output <- as.vector(iristrain[,5])
test_input <- as.matrix(iristest[,-5])
prediction <- knn(train_input, test_input, 
                    train_output, k=5)
table(prediction, iristest$Species)


# PCA - We use principle component analysis (PCA) to transform our space.
# PCA transforms the space by looking at the vectors along which the
# bulk of the variance in the data occur. The vector that embodies
# the greatest variance becomes the first principle component axis
# in the transformed space. The second axis then is formed along the
# vector that is orthogonal to the first but with the second most 
# variance in the data. And so on.

# LDA - In linear discriminant analysis (LDA) we no longer look for recursive 
# partitions, but rather for lines that go between the clusters.
# In some ways, this is similar to KNN. LDA approaches
# the problem by applying a transform that applies the inverse of 
# the estimated covariance matrix. In this transformed space,
# classification is finding the closest cluster mean. 

# Load data for PCA
pca <- prcomp(iris[,-5],
              center = TRUE,
              scale. = TRUE) 
# Scaled PCA
prop.pca = pca$sdev^2/sum(pca$sdev^2)

# Load data for LDAA
lda <- lda(Species ~ ., 
           iris, 
           prior = c(1,1,1)/3)
# Scaled LDA
prop.lda = lda$svd^2/sum(lda$svd^2)

# Predict LDA
plda <- predict(object = lda,
                newdata = iris)

# generate dataset to compare PCA and LDA
dataset = data.frame(species = iris[,"Species"],
                     pca = pca$x, lda = plda$x)

# generate plot for LDA
p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species, shape = species), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

# generate plot for PCA
p2 <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, colour = species, shape = species), size = 2.5) +
  labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

grid.arrange(p1, p2)



# NaiveBayes#

# Computes the conditional a-posterior probabilities
# of a categorical class variable given independent
# predictor variables using the Bayes rule.

# Load data
data(iris)
(m <- naiveBayes(iris[,-5], iris[,5]))
table(predict(m, iris[,-5]), iris[,5])




# SVM

# Support vector machines take the next step from LDA/QDA. However
# instead of making linear voronoi boundaries between the cluster
# means, we concern ourselves primarily with the points on the 
# boundaries between the clusters. These boundary points define
# the 'support vector'. Between two completely separable clusters
# there are two support vectors and a margin of empty space
# between them. The SVM optimization technique seeks to maximize
# the margin by choosing a hyperplane between the support vectors
# of the opposing clusters. For non-separable clusters, a slack
# constraint is added to allow for a small number of points to
# lie inside the margin space.

# Load data
data(cats)
m1 <- svm(Sex~., data = cats)
plot(m1, cats)
cplus<-cats
m2 <- svm(Sex~., data = cplus)
plot(m2, cplus, Bwt~Hwt)


