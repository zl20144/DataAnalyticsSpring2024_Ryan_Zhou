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
#• The age of abalone is determined by cutting the shell
#through the cone, staining it, and counting the number
#of rings through a microscope: a boring and timeconsuming task.
#• Other measurements, which are easier to obtain, are
#used to predict the age.
#• Perform knn classification to get predictors for Age
#(Rings). Interpretation not required.
# <insert code here>



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
# <insert code here>