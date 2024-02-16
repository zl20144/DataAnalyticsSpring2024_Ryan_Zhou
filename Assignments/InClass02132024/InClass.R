abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")
colnames(abalone) <- c("sex","length","diameter","height","whole_weight","shucked_weight","viscera_weight","shell_weight","rings")
summary(abalone)
str(abalone)
summary(abalone$rings)

# the "rings" has a range between 1~29, so we are dividing them into 3 groups:
# -1 < x < 8, 8 <= x <= 11, x > 11 for young, adult, old

# given that rings are characters, a convertion in place can be done before further analysis
abalone$rings <- as.numeric(abalone$rings)
# now cut the "rings" w.r.t. the actual age difference
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels-c("young","adult","old"))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)

# k-NN Algorithm doesn't function with non-numeric variables, so remove those variables
#
aba <- abalone
aba$sex <- NULL

# to put different variables on the same scale(0~1), normalization is required
normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}

aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_weight)

index <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7,0.3))

KNNtrain <- aba[index==1,]
KNNtest <- aba[index==2,]
sqrt(2918) # 2918 = number of observations in the training set
#    ^^^^ there are better ways to get this number
# k = sqrt(2918) ~ 55 (pick odd number)
library(class)
help("knn")

KNNprediction <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNprediction
table(KNNprediction)

# TO-DO: In-Class Exercise K-Means Iris dataset
library(ggplot2)
head(iris)
str(iris)
# There are 3 species: setosa, versicolor and verginica
summary(iris)

help("sapply")
sapply(iris[,-5], var)
summary(iris)
#plot Sepal.length VS sEPAL.width using ggplot
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + geom_point()
#plot Petal.length VS Petal.width using ggplot
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col = Species)) + geom_point()

# kmeans clustering
set.seed(300)
k.max <- 12
# wss =  within sum of squares
# tot.withinss = total within cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen
wss <- sapply(1:k.max,function(k){
  kmeans(iris[3:4],k,nstart = 20, iter.max = 20)$tot.withinss
})
wss
plot(1:k.max, wss, type="b", xlab="Number of clusters(k)", ylab = "WSS")
icluster <- kmeans(iris[,3:4], 3, nstart = 20)
table(icluster$cluster, iris$Species)

library(rpart)
library(rpart.plot)
dim(iris)
sample_iris <- sample(150,100)
sample_iris

iris_train <- iris[sample_iris,]
iris_test <- iris[-sample_iris,]
dim(iris_train)
dim(iris_test)
# generate the decision tree model
decisionTreeModel <- rpart(Species~., iris_train, method = "class")
decisionTreeModel
#plotting the decision tree model using rpart.plot() function
rpart.plot(decisionTreeModel)