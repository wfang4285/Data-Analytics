##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####
wine$Type <- as.factor(wine$Type)

## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

#Y type (1-3), X all others
X <- wine[,-1]
Y <- wine$Type

#PCA to identify principal components 
Xmat <- as.matrix(X)

Xc <- scale(Xmat, center = T, scale = T)

principal_components <- princomp(Xc)

summary(principal_components)

# Plots PC1 + 2
autoplot(principal_components, data = wine, colour = 'Type') +
  ggtitle("PCA of Wine Dataset (PC 1 + 2)") 

#Variables for the 1st PC printed in sorted order
variables <- principal_components$loadings[, 1]
sorted_variables <- sort(abs(variables))
print(sorted_variables)

#kNN training using 3 vars
data.c <- wine[, c("Type", "Alcohol", "Ash", "Magnesium")]

data.c <- na.omit(data.c)
data.c$Type <- as.factor(data.c$Type)
data.c$`Alcohol` <- scale(data.c$`Alcohol`)
data.c$`Ash` <- scale(data.c$`Ash`)
data.c$`Magnesium` <- scale(data.c$`Magnesium`)

colnames(data.c) <- c("Type", "Alcohol", "Ash", "Magnesium")

set.seed(70)
split.c <- sample(nrow(data.c), 0.7 * nrow(data.c))
train.c <- data.c[split.c, ]
test.c  <- data.c[-split.c, ]

knn.predicted <- knn(train = train.c[, 2:4], test = test.c[, 2:4], cl = train.c$Type, k = 3)
confusionMatrix(knn.predicted, test.c$Type, mode = "prec_recall")

#Prediction using first two PCs
data.pca <- data.frame(
  Type = wine$Type,
  PC1 = principal_components$scores[, 1],
  PC2 = principal_components$scores[, 2]
)

head(data.pca)
data.c <- na.omit(data.c)
data.pca$Type <- as.factor(data.pca$Type)

set.seed(70)
split.pca <- sample(nrow(data.pca), 0.7 * nrow(data.pca))
train.pca <- data.pca[split.pca, ]
test.pca  <- data.pca[-split.pca, ]

knn.predicted <- knn(train = train.pca[, 2:3], test = test.pca[, 2:3], cl = train.c$Type, k = 3)
confusionMatrix(knn.predicted, test.c$Type, mode = "prec_recall")

#2 kNNs comparsion with tables again
knn.predicted <- knn(train = train.c[, 2:4], test = test.c[, 2:4], cl = train.c$Type, k = 3)
confusionMatrix(knn.predicted, test.c$Type, mode = "prec_recall")

knn.predicted <- knn(train = train.pca[, 2:3], test = test.pca[, 2:3], cl = train.c$Type, k = 3)
confusionMatrix(knn.predicted, test.c$Type, mode = "prec_recall")


