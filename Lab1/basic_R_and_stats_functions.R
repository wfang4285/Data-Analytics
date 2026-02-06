library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("C:/Users/fangw2/Desktop/DATA_ANALYTICS/Lab1")

# read data
data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(data)

# print values in variable
data$ECO.new

# print summary of variable
summary(data$ECO.new)


### Explore Variable ###

## take a copy of a variable from a dataframe into a separate variable
ECO <- data$ECO.new

# find NAs in variable: function outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(ECO)

# print
NAs

# function "which" returns row numbers of rows with NAs
rownums <- which(NAs)

# print rows with NAs
ECO[rownums]

ECO.complete <- ECO[!NAs]

ECO.complete

## create copy of new variable

MKP <- data$MKP.new

# print values in variable
MKP

# find NAs inv variavle - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(MKP)

rownums <- which(NAs)

# print NAs
MKP[rownums]

# take subset of NOT NAs from variable
MKP.complete <- MKP[!NAs]

MKP.complete
  
# stats
summary(MKP.complete)

# boxplot of variable(s)
boxplot(ECO.complete, MKP.complete, names = c("ECO","MKP"))


### Histograms ###

# range of values
x<-seq(20,90,5)

# histogram (frequency distribution) over range
hist(ECO.complete, x, prob=TRUE) 

avg_value <- mean(ECO.complete)
sd_value <- sd(ECO.complete)

x1<-seq(20,90,1)
# generate probability density values for a normal distribution with given mean and sd
d1 <- dnorm(x1,mean = avg_value, sd = sd_value,log=FALSE)

# print density values
lines(x1,d1)

# Same again for MKP
x<-seq(0,100,5)
hist(MKP.complete, x, prob=TRUE) 

avg_value <- mean(MKP.complete)
sd_value <- sd(MKP.complete)

x1<-seq(0,100,5)
d1 <- dnorm(x1,mean = avg_value, sd = sd_value,log=FALSE)

lines(x1,d1)


### Empirical Cumulative Distribution Function ###

# plot ecdfs
plot(ecdf(ECO.complete), do.points=FALSE, verticals=TRUE) 

plot(ecdf(MKP.complete), do.points=FALSE, verticals=TRUE) 


### Quantile-quantile Plots ###

# print quantile-quantile plot for variable with theoretical normal distribuion
qqnorm(ECO.complete); qqline(ECO.complete)

qqnorm(MKP.complete); qqline(MKP.complete)

# print quantile-quantile plot of two variables

qqplot(ECO.complete, MKP.complete, xlab = "Q-Q plot for ECO & MKP") 

## Statistical Tests

hist(ECO.complete)
hist(MKP.complete)

shapiro.test(ECO.complete)
shapiro.test(MKP.complete)

ad.test(ECO.complete)
ad.test(MKP.complete)

ks.test(ECO.complete,MKP.complete)

wilcox.test(ECO.complete,MKP.complete)

var.test(ECO.complete,MKP.complete)
t.test(ECO.complete,MKP.complete)

hist(ECO.complete, col='lightsteelblue')
hist(MKP.complete, col='lightgreen', add=TRUE)


### THE END ###