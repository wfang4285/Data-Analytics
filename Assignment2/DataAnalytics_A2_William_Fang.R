library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)
library(readr)
library(class)
library(caret)

#Read dataset in, one var for question 1
dataset <- read_csv("epi_results_2024_pop_gdp.csv")
ECO <- dataset$ECO.new

#Box plot and histo. of ECO
x<-seq(20,90,5)
hist(ECO, x, prob=TRUE) 
lines(density(ECO), col = "blue")

boxplot(ECO ~ region, dataset, name = c("ECO"), las = 2)

#Question 2 Start

#Using global west + asia-pacific region data
west <- subset(dataset, region == "Global West")
asia <- subset(dataset, region == "Asia-Pacific")
west.ECO <- west$ECO.new
asia.ECO <- asia$ECO.new

#Histographs of each region
x<-seq(20,90,5)
hist(west.ECO, x, prob=TRUE) 
lines(density(west.ECO), col = "blue")

x<-seq(20,90,5)
hist(asia.ECO, x, prob=TRUE) 
lines(density(asia.ECO), col = "blue")

#QQ plot
qqplot(west.ECO, asia.ECO) 
abline(0, 1, col = "red", lwd = 2)

#Question 3 start

#All other regionsregions
east <- subset(dataset, region == "Eastern Europe")
soviet <- subset(dataset, region == "Former Soviet States")
me <- subset(dataset, region == "Greater Middle East")
latin <- subset(dataset, region == "Latin America & Caribbean")
sasia <- subset(dataset, region == "Southern Asia")
africa <- subset(dataset, region == "Sub-Saharan Africa")

#Plots with linear model attached
plot(ECO.new~log(population), data = west, main = "Global West")
abline(lm(ECO.new~log(population), data = west))

plot(ECO.new~log(gdp), data = west, main = "Global West")
abline(lm(ECO.new~log(gdp), data = west))


plot(ECO.new~log(population), data = asia, main = "Asia-Pacific")
abline(lm(ECO.new~log(population), data = asia))

plot(ECO.new~log(gdp), data = asia, main = "Asia-Pacific")
abline(lm(ECO.new~log(gdp), data = asia))


plot(ECO.new~log(population), data = east, main = "Eastern Europe")
abline(lm(ECO.new~log(population), data = east))

plot(ECO.new~log(gdp), data = east, main = "Eastern Europe")
abline(lm(ECO.new~log(gdp), data = east))


plot(ECO.new~log(population), data = soviet, main = "Former Soviet States")
abline(lm(ECO.new~log(population), data = soviet))

plot(ECO.new~log(gdp), data = soviet, main = "Former Soviet States")
abline(lm(ECO.new~log(gdp), data = soviet))


plot(ECO.new~log(population), data = me, main = "Greater Middle East")
abline(lm(ECO.new~log(population), data = me))

plot(ECO.new~log(gdp), data = me, main = "Greater Middle East")
abline(lm(ECO.new~log(gdp), data = me))


plot(ECO.new~log(population), data = latin, main = "Latin America & Caribbean")
abline(lm(ECO.new~log(population), data = latin))

plot(ECO.new~log(gdp), data = latin, main = "Latin America & Caribbean")
abline(lm(ECO.new~log(gdp), data = latin))


plot(ECO.new~log(population), data = sasia, main = "Southern Asia")
abline(lm(ECO.new~log(population), data = sasia))

plot(ECO.new~log(gdp), data = sasia, main = "Southern Asia")
abline(lm(ECO.new~log(gdp), data = sasia))


plot(ECO.new~log(population), data = africa, main = "Sub-Saharan Africa")
abline(lm(ECO.new~log(population), data = africa))

plot(ECO.new~log(gdp), data = africa, main = "Sub-Saharan Africa")
abline(lm(ECO.new~log(gdp), data = africa))

#3.2 two models compared- asia pacific is superior by far
west_gdp <- lm(ECO.new ~ log(gdp), data = west)
summary(west_gdp)
plot(west_gdp$residuals, main = "Residuals: Global West", ylab = "Residuals")

asia_gdp <- lm(ECO.new ~ log(gdp), data = asia)
summary(asia_gdp)
plot(asia_gdp$residuals, main = "Residuals: Asian-Pacific", ylab = "Residuals")

#Question 4 Start

#Get 3 chosen regions and 3 varaiables of them
regions <- c("Global West", "Asia-Pacific", "Eastern Europe")
knn_data <- dataset[dataset$region %in% regions, ]
knn_data <- knn_data[, c("region", "population", "gdp", "ECO.new")]

#FACTOR, log of population + gdp, and scale
knn_data$region <- as.factor(knn_data$region)
knn_data$population <- log(knn_data$population)
knn_data$gdp <- log(knn_data$gdp)
knn_data[, 2:4] <- scale(knn_data[, 2:4])

#Best k fit below using training and testing
set.seed(15)
train_regions <- sample(nrow(knn_data), 0.7 * nrow(knn_data))
train <- knn_data[train_regions, ]
test <- knn_data[-train_regions, ]

#Guessing best k
k_range <- 1:46
results <- numeric(46)

#Loop through and find best result 
for(k in k_range){
  knn_result <- knn(train = train[, 2:4], test = test[, 2:4], cl = train$region, k = k)
  results[k] <- sum(knn_result == test$region) / nrow(test)
}

k_result <- which.max(results)
print(paste("Best K:", k_result, "Accuracy:", results[k_result]))

#4.2 Training another model with same thing as above, only using EPI instead
knn_data <- dataset[dataset$region %in% regions, ]
knn_data <- knn_data[, c("region", "population", "gdp", "EPI.new")]

#FACTOR, log of population + gdp, and scale
knn_data$region <- as.factor(knn_data$region)
knn_data$population <- log(knn_data$population)
knn_data$gdp <- log(knn_data$gdp)
knn_data[, 2:4] <- scale(knn_data[, 2:4])

#Best k fit below using training and testing
set.seed(15)
train_regions <- sample(nrow(knn_data), 0.7 * nrow(knn_data))
train <- knn_data[train_regions, ]
test <- knn_data[-train_regions, ]

#Guessing best k
k_range <- 1:46
results <- numeric(46)

#Loop through and find best result 
for(k in k_range){
  knn_result <- knn(train = train[, 2:4], test = test[, 2:4], cl = train$region, k = k)
  results[k] <- sum(knn_result == test$region) / nrow(test)
}

k_result <- which.max(results)
print(paste("Best K:", k_result, "Accuracy:", results[k_result]))







