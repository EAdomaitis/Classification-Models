# This model was created for a homework assignment using the well known iris dataset.
# The goal is to use a an unsupervised clustering model to classify the type of flower given a set of characteristics.



#Function to install packages and load them if they are not already installed
PackagesPrep <- function(x){
  for( i in x ){
    #  require loads the package and returns false if it was unable to do so
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

#  Run function
PackagesPrep( c("ggplot2" ) )


# Clear environment
rm(list = ls())

# Reading in the data
data(iris)

#
# check to make sure the data is read correctly
#

head(iris)

# Console output for head(data)
##Num Sepal.Length Sepal.Width Petal.Length Petal.Width Species
##1   1          5.1         3.5          1.4         0.2  setosa
##2   2          4.9         3.0          1.4         0.2  setosa
##3   3          4.7         3.2          1.3         0.2  setosa
##4   4          4.6         3.1          1.5         0.2  setosa
##5   5          5.0         3.6          1.4         0.2  setosa
##6   6          5.4         3.9          1.7         0.4  setosa
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT
#

#Let us see how many of each plant species we have for reference in k-means clustering results

table(iris[,5], iris$Species)

#Output from table(data[,5], data$Species)
##             setosa   versicolor virginica
##setosa         50          0         0
##versicolor      0         50         0
##virginica       0          0        50


# Setting the random number generator seed
set.seed(1)


############################### Exploring the data ############################### 


# Data is small enough to explore as a whole
iris

# It appears that sepal width and length tend to be fairly constant throughout each species, but petal width and length vary a bit more.

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
# We can visually see that petal length and width create pretty great separation between species

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()
# On the other hand, sepal length and width have a lot more overlap, so petal length and width will likely be the only variables we need.


############################### Performing the kmeans clustering ############################### 


# Logically speaking, we know that there are 3 species involved, so we know 3 clusters will likely be the best solution, but let's look at multiple solutions
# To begin, let's start with all predictor variables and then we will simply use petal length and width and compare results.
# Additionally, this is an unsupervised program, but in this case, we already know the results. That gives us a chance to check the accuracy that we otherwise would not be able to see.

#Cluster on unscaled data
irisClusterALL2 <- kmeans(iris[,1:4], 2, nstart = 20) # Try 2 clusters 20 times
irisClusterALL3 <- kmeans(iris[,1:4], 3, nstart = 20) # Try 3 clusters 20 times
irisClusterALL4 <- kmeans(iris[,1:4], 4, nstart = 20) # Try 4 clusters 20 times
irisClusterALL5 <- kmeans(iris[,1:4], 5, nstart = 20) # Try 5 clusters 20 times

# Scale the data:
scdata <- iris # initialize value/size of sdata
for (i in 1:4) { scdata[,i] <- (iris[,i]-min(iris[,i]))/(max(iris[,i])-min(iris[,i])) }

#Cluster on scaled data
irisClusterALLsc2 <- kmeans(scdata[,1:4], 2, nstart = 20) # Try 2 clusters 20 times
irisClusterALLsc3 <- kmeans(scdata[,1:4], 3, nstart = 20) # Try 3 clusters 20 times
irisClusterALLsc4 <- kmeans(scdata[,1:4], 4, nstart = 20) # Try 4 clusters 20 times
irisClusterALLsc5 <- kmeans(scdata[,1:4], 5, nstart = 20) # Try 5 clusters 20 times

# Calculate the total distance between data points and cluster centers:
csum = 0

# for each data point
for (i in 1:nrow(data)) {
# add the distance between its point and its cluster center
    csum = csum + dist(rbind(iris[i,1:4],irisClusterALL2$centers[irisClusterALL2$cluster[i],]))
}

# Check Total
csum[1] #128.3367

#Now, let's compare the clusters with the species.
#We can only do this because we happen to know the species of each data point.

table(irisClusterALL2$cluster, data$Species)
table(irisClusterALLsc2$cluster, data$Species)
table(irisClusterALL3$cluster, data$Species)
table(irisClusterALLsc3$cluster, data$Species)
table(irisClusterALL4$cluster, data$Species)
table(irisClusterALLsc4$cluster, data$Species)
table(irisClusterALL5$cluster, data$Species)
table(irisClusterALLsc5$cluster, data$Species)

#We can see that 3 clusters did a fairly good job. Let's see if just petal variables give better results

############################### Using Petal Length and Petal Width ############################### 

#Because of the graphical analysis above, let's try just using petal length and petal width

irisClusterPET2 <- kmeans(data[,3:4], 2, nstart = 20)
irisClusterPET3 <- kmeans(data[,3:4], 3, nstart = 20)
irisClusterPET4 <- kmeans(data[,3:4], 4, nstart = 20)
irisClusterPET5 <- kmeans(data[,3:4], 5, nstart = 20)

irisClusterPETsc2 <- kmeans(scdata[,3:4], 2, nstart = 20)
irisClusterPETsc3 <- kmeans(scdata[,3:4], 3, nstart = 20)
irisClusterPETsc4 <- kmeans(scdata[,3:4], 4, nstart = 20)
irisClusterPETsc5 <- kmeans(scdata[,3:4], 5, nstart = 20)

# Calculate the total distance between data points and cluster centers:
csum2 = 0

# for each data point...
for (i in 1:nrow(data)) {
  # add the distance between its point and its cluster center
  csum2= csum2 + dist(rbind(iris[i,3:4],irisClusterPET2$centers[irisClusterPET2$cluster[i],]))
}

# Check Total
csum2[1] #87.58802 - much lower than before

#Let us compare the clusters with the species.

table(irisClusterPET2$cluster, data$Species)
table(irisClusterPETsc2$cluster, data$Species)
table(irisClusterPET3$cluster, data$Species)
table(irisClusterPETsc3$cluster, data$Species)
table(irisClusterPET4$cluster, data$Species)
table(irisClusterPETsc4$cluster, data$Species)
table(irisClusterPET5$cluster, data$Species)
table(irisClusterPETsc5$cluster, data$Species)

#Clustering with 3 centroids only resulted in 6 misclassifications this time around. Overall, it appears 3 clusters and using petal length and width is our optimal model.

#Here is a summary of various results from the selected model:
irisClusterPETsc3
   
  # K-means clustering with 3 clusters of sizes 48, 52, 50
  # 
  # Cluster means:
  #   Petal.Length Petal.Width
  # 1   0.77401130  0.81510417
  # 2   0.55867014  0.51041667
  # 3   0.07830508  0.06083333
  # 
  # Clustering vector:
  #   [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
  # [98] 2 2 2 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  # 
  # Within cluster sum of squares by cluster:
  #   [1] 0.8858123 0.6791299 0.1369325
  # (between_SS / total_SS =  94.0 %)
  # 
  # Available components:
  #   
  #   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault"

# Plot solution
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisClusterPETsc3$cluster)) + geom_point() + scale_color_gradient(low="blue", high="green")



