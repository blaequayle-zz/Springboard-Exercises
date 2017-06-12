# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))
install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
library(rattle)
data(wine, package="rattle")
head(wine)
summary(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

df.wine <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data, 2, var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df.wine)

# Exercise 2:
#   * How many clusters does this method suggest?
# The plot shows a significant change in gradient of the line for within groups sum of squares when 3 clusters is reached.

#   * Why does this method work? What's the intuition behind it?
#K means requires that the number of clusters is slected before the algorithm is run.
#This method involves observing a set of possible numbers of clusters relative to how they minimise the within cluster sum of squares. 
#For loop allows multiple cluster numbers to be tested rapidly.

#   * Look at the code for wssplot() and figure out how it works
#It sets up a function using the input dataframe, max number of clusters (nc = 15) and a random seed.
#Num. of rows in the dataframe minus 1 multiplied by sum of variance of each column
#For loop with looping index i - for(i in 2:nc) : begins at 2 as cannot have 1 cluster
#Calculates sum of kmeans$withinss i.e. vector of within cluster sum of squares, for 2 - 15 clusters

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df.wine, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
#15 of 24 criteria provided by the package suggest a three cluster solution.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df.wine, centers = 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

fit.km$size
fit.km$centers
compare <- table(wine$Type, fit.km$cluster)
View(compare)

#NB. Var1, Var 2 in table
#The clustering looks like it has done a good job. Only 6 have been incorrectly classified.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

library(cluster)
clusplot(df.wine, fit.km$cluster)

#clusplot(x - matrix or data frame, clus - a vector of length n representing a clustering of x)
#Yes - I would consider this to be good clustering.
