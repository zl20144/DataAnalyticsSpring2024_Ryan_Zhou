# Exercise 1: Heatmap(), image() and hierarchical clustering example
# creating a matrix data with random numbers
# and plotting the matrix using the image() function
# you will see there, it does not have a real pattern in the plot.
set.seed(12345)
help(par)
# par can be used to set or query graphical parameters.
# Parameters can be set by specifying them as arguments
# to par in tag = value form, or by passing them as a list of tagged values.
par(mar = rep(0.2,4))
data_Matrix <-matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
help("heatmap")
help("rep")
par(mar = rep(0.2,4))
heatmap(data_Matrix)
# When we run the heatmap() here, we get the dendrograms printed on the
# both columns and the rows and still there is no real immerging pattern that is
# interesting to us,
# it is because there is no real interesting pattern underlying in the data we
# generated.
help(rbinom)
set.seed(678910)
# Looped through all the
# rows and, on a random row, I flipped a coin.
# during the coin flip, if it turns out to be 1
# (true), then, just added a pattern to my data in a
# way that the five of the columns have a mean
# of zero and others have mean of three.
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}
# Now we will plot the data
# Now we can see that the right hand five columns have more yellow in them,
# which means they have a higher value and the left hand five columns that are little
# bit more in red color which means they have a lower value.
# it is because some of the rows have a mean of three in the right hand side, and
# some of the rows have mean of zero. Now we have introduced some pattern to it.
par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])
# now we will run the heatmap() function on the data, we can see that, two
# sets of columns are easily separated.
par(mar=rep(0.2, 4))
heatmap(data_Matrix)
# Let's take a closer look at the patterns in rows and columns by
# looking at the marginal
# means of the rows and columns.
# ten different columns means and forty different rows means
hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab="The Row Mean", ylab = "Row", pch = 19)
plot(colMeans(data_Matrix_Ordered), xlab = "Column", ylab = "The Column Mean", pch = 19)



#Exercise 2: Classification
#• Retrieve the abalone.csv dataset
#• Predicting the age of abalone from physical
#measurements.
#• Perform knn classification to get predictors for Age
#(Rings). Interpretation not required.
# abalone dataset from UCI repository
# reading the dataset from UCI repository URL
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,
                    sep = ",")
# Column names
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight',
                       'rings' )
# summary on abalone
summary(abalone)
# structure of the abalone data
str(abalone)
# summary of the abalone rings column
summary(abalone$rings)
# As shown above, the “rings” variable has a range between 1-29.
# This is the variable that we want to predict, and predicting this many levels
# might not give us the insight we’re looking for.
# For now, we’ll break the rings variable
# into 3 levels" “young” for abalones less than 8, “adult” for abalones between 8-11,
# and “old” for abalones older than 11.
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)
# remove the "sex" variable in abalone, because KNN requires all numeric variables for prediction
# z <- abalone
aba <- abalone
aba$sex <- NULL
# normalize the data using min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
# After Normalization, each variable has a min of 0 and a max of 1.
# in other words, values are in the range from 0 to 1.
# We’ll now split the data into training and testing sets.
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)
# make k equal to the square root of 2918, the number of observations in the training set.
# sqrt(2918) ~= 54.01852 round it to 55 and use k = 55 # We usually take an Odd number for k value,
# knn model
# knn() is in the "class" library. Make sure to install it first on your RStudio.
library(class)
help("knn") # Read the knn documentation on RStudio.
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)


#Exercise 3: Clustering
#• The Iris dataset (in R use data(“iris”) to load it)
#• The 5th column is the species, and you want
#to find how many clusters without using that
#information
#• Create a new data frame and remove the fifth
#column
#• Apply kmeans (you choose k) with 1000
#iterations
#• Use table(iris[,5],<your clustering>) to assess
#your results
# iris dataset is from UCI ML repository.
library(ggplot2)
data(iris)
head(iris) # first 6 rows of the
str(iris) # take a look at the structure of the iris data using str() function in R.
# dataset has 150 observations equally distributed observations among
# the three species: Setosa, Versicolor and Verginica.
summary(iris) # summary statistics of all the 4 variables Sepal.Length,Sepal.Width,
# Petal.Length and Petal.Width
help("sapply")
sapply(iris[,-5], var)
summary(iris)
# plot Sepal.Length Vs Sepal.Width using ggplot
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
# plot Petal.Length Vs Sepal.Width using ggplot
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()
# kmeans clustering
# Read the documentation for kmeans() function
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html
set.seed(300)
k.max <- 12
# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 1000)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris[,5])