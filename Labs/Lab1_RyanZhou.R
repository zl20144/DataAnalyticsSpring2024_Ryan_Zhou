# Lab 1
# Part I: 2010EPI_data.csv

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
EP <- EPI[!tf] # filters out NA values, new array
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


WATER_H
wtf <- is.na(WATER_H)
W <- WATER_H[!wtf]
summary(W)
summary(WATER_H)
fivenum(WATER_H, na.rm = TRUE)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0., 101., 2.0), prob=TRUE)
lines(density(WATER_H,na.rm=TRUE,bw=1.))
rug(WATER_H)
plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(WATER_H); qqline(WATER_H)
boxplot(WATER_H, DALY)
qqplot(EPI,WATER_H)


ENVHEALTH

entf <- is.na(ENVHEALTH)
EN <- ENVHEALTH[!entf]
summary(EN)
fivenum(EN, na.rm = TRUE)
stem(EN)
hist(EN)
hist(EN, seq(0., 96., 2.0), prob=TRUE)
lines(density(EN,na.rm=TRUE,bw=1.))
rug(ENVHEALTH)
plot(ecdf(EN), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EN); qqline(EN)
boxplot(EN, EPI)
boxplot(EN, WATER_H)
qqplot(W, EN)


ECOSYSTEM

etf <- is.na(ECOSYSTEM)
E <- ECOSYSTEM[!etf]
summary(E)
summary(ECOSYSTEM)
fivenum(ECOSYSTEM, na.rm = TRUE)
fivenum(ECOSYSTEM, na.rm = FALSE)
stem(ECOSYSTEM)
hist(ECOSYSTEM)
hist(ECOSYSTEM, seq(0., 98., 2.0), prob=TRUE)
lines(density(ECOSYSTEM,na.rm=TRUE,bw=1.))
rug(E)
plot(ecdf(E), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(E); qqline(E)
boxplot(E,EPI)
qqplot(E,W)


DALY

dtf <- is.na(DALY)
D <- DALY[!dtf]
summary(D)
fivenum(D, na.rm = TRUE)
stem(DALY)
hist(DALY)
hist(D, seq(0., 74., 2.0), prob=TRUE)
lines(density(D,na.rm=TRUE,bw=1.))
rug(DALY)
plot(ecdf(D), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(D); qqline(DALY)
boxplot(DALY, WATER_H)
boxplot(D,EN)
qqplot(D,W)
qqplot(EPI, D)

AIR_H

atf <- is.na(AIR_H)
A <- AIR_H[!atf]
summary(A)
fivenum(A, na.rm = TRUE)
stem(A)
hist(A)
hist(A, seq(0., 91., 2.0), prob=TRUE)
lines(density(A,na.rm=TRUE,bw=1.))
rug(AIR_H)
plot(ecdf(A), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(A); qqline(A)
boxplot(A, EP)
boxplot(A, W)
qqplot(AIR_H, ENVHEALTH)
qqplot(A, D)


help(distributions)
EPILand <- EPI[!Landlock]
EPILand
ELand <- EPILand[!is.na(EPILand)]
ELand
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
lines(density(ELand,na.rm=TRUE,bw=1.))
EPIPop <- EPI[!High_Population_Density]
EPIPop
summary(EPIPop)
fivenum(EPIPop)
hist(EPIPop, seq(30., 100., 1.0), prob=TRUE)
boxplot(ELand, EPIPop)
EPI_South_Asia = EPI[EPI_regions == 'South Asia']
View(EPI_South_Asia)
EPI_SouthAsia = EPI[GEO_subregion == 'South Asia']
View(EPI_SouthAsia)
WATERH_South_Asia = WATER_H[EPI_regions == 'South Asia']
View(WATERH_South_Asia)



# Part II: 2010GRUMP_data.xls
data_2010GRUMP = read_xls('2010GRUMP_data.xls', sheet = "Summary Information")
View(data_2010GRUMP)
attach(data_2010GRUMP) # sets default object to attached object
fix(data_2010GRUMP)

PopulationPerUnit
ptf <- is.na(PopulationPerUnit)
PP <- PopulationPerUnit[!ptf]
summary(PP)
fivenum(PopulationPerUnit)
stem(PP)
hist(PP)
hist(PP, seq(0., 3000., 150))
lines(density(PP, na.rm=TRUE, bw=1.))
rug(PP)
plot(ecdf(PP), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(PP); qqline(PP)


`P95E ('000)`
petf = is.na(`P95E ('000)`)
PE = `P95E ('000)`[!petf]
summary(PE)
fivenum(PE)
stem(PP)
hist(PP)
rug(PE)
plot(ecdf(PE), do.points=FALSE, verticals=TRUE)
qqnorm(PE)
qqline(PE)

boxplot(PP,PE)
qqplot(PE,PP)
