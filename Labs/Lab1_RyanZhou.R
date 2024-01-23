
# TO-DO: Repeat this on multiple files and multiple distributions

data_2010EPI <- read.csv("2010EPI_data.csv")
# How to change the first row to be the header in R?
names(data_2010EPI) <- as.matrix(data_2010EPI[1, ])
data_2010EPI <- data_2010EPI[-1, ]
data_2010EPI[] <- lapply(data_2010EPI, function(x)
  type.convert(as.character(x)))
data_2010EPI
View(data_2010EPI)
attach(data_2010EPI) # sets default object to attached object, if you call EPI,it's really calling [default obj$]EPI
fix(data_2010EPI)
EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array
summary(EPI) # stats
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(EPI)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

boxplot(EPI, DALY)
qqplot(EPI,DALY)

help(distributions)
