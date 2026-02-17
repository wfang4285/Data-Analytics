################################################
#### Evaluating Classification & CLustering ####
################################################

library("caret")
library(GGally)
library(psych)
library(class)
library(cluster)
library(factoextra)

## read data
abalone <- read.csv("C:/Users/fangw2/Desktop/DATA_ANALYTICS/Lab3/abalone/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]
abalone.dimensions <- abalone[,c(2:4,10)]
abalone.weights <- abalone[,c(5:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)
abalone.dimensions$age.group <- as.character(abalone.dimensions$age.group)
abalone.weights$age.group <- as.character(abalone.weights$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)
abalone.dimensions$age.group <- as.factor(abalone.dimensions$age.group)
abalone.weights$age.group <- as.factor(abalone.weights$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

train.dim <- abalone.dimensions[train.indexes,]
test.dim <- abalone.dimensions[-train.indexes,]

train.weights <- abalone.weights[train.indexes,]
test.weights <- abalone.weights[-train.indexes,]

## separate x (features) & y (class labels)
X <- train[,1:7] 
Y <- train[,8]

X.dim <- train.dim[,1:3]
Y.dim <- train.dim[,4]

X.weights <- train.weights[,1:4]
Y.weights <- train.weights[,5]

## kNN Model
k.range <- 1:100
results.dim <- numeric(100)
results.weights <- numeric(100)
results.sub <- numeric(100)

for (k in k.range){
  knn.sub <- knn(train[,1:7], test[,1:7], train[,8], k = k)
  results.sub[k] <- sum(knn.sub == test[,8]) / nrow(test)
  
  knn.dim <- knn(train.dim[,1:3], test.dim[,1:3], train.dim[,4], k = k)
  results.dim[k] <- sum(knn.dim == test.dim[,4]) / nrow(test.dim)
  
  knn.weights <- knn(train.weights[,1:4], test.weights[,1:4], train.weights[,5], k = k)
  results.weights[k] <- sum(knn.weights == test.weights[,5]) / nrow(test.weights)
}

k.sub <- which.max(results.sub)
k.dim <- which.max(results.dim)
k.weights <- which.max(results.weights)
print(paste("BEST K SUB:", k.sub))
print(paste("BEST K DIM:", k.dim))
print(paste("BEST K WEIGHTS:", k.weights))

knn.dim <- knn(train.dim[,1:3], test.dim[,1:3], train.dim[,4], k = k.dim)
knn.weights <- knn(train.weights[,1:4], test.weights[,1:4], train.weights[,5], k = k.weights)

## Contingency Tables
table.dim <- table(Predicted = knn.dim, Actual = test.dim[,4])
table.weights <- table(Predicted = knn.weights, Actual = test.weights[,5])
print(table.dim)
print(table.weights)

## EXCERISE 2
X.dim.scaled <- scale(X.dim)
k.list <- 2:10
si.km.list <- c()
si.pam.list <- c()

for (k in k.list){
  set.seed(6)
  km.temp <- kmeans(X.dim.scaled, centers = k, nstart = 25)
  sil.km <- silhouette(km.temp$cluster, dist(X.dim.scaled))
  avg.si.km <- mean(sil.km[, 3])
  si.km.list <- c(si.km.list, avg.si.km)
  
  pam.temp <- pam(X.dim.scaled, k = k)
  si.pam.list <- c(si.pam.list, pam.temp$silinfo$avg.width)
}

best.k.km <- k.list[which.max(si.km.list)]
best.k.pam <- k.list[which.max(si.pam.list)]

print(paste("K-MEANS BEAT:", best.k.km))
print(paste("PAM BEST:", best.k.pam))

plot(k.list, si.km.list, type = "b", main = "K-Means", xlab = "k", ylab = "Width")
plot(k.list, si.pam.list, type = "b", main = "PAM", xlab = "k", ylab = "Width")

final.km <- kmeans(X.dim.scaled, centers = best.k.km, nstart = 25)
sil.final.km <- silhouette(final.km$cluster, dist(X.dim.scaled))
fviz_silhouette(sil.final.km) + labs(title = "k-MEANS")

final.pam <- pam(X.dim.scaled, k = best.k.pam)
fviz_silhouette(final.pam) + labs(title = "PAM")

## feature boxplots
boxplot(X, main="abalone features")

## class label distributions
plot(Y)


## feature-class plots
featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## psych scatterplot matrix
pairs.panels(X,gap = 0,bg = c("pink", "green", "blue")[Y],pch=21)

## GGally 
ggpairs(train, ggplot2::aes(colour = Y))



## EOF ##

