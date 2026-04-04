library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

## read dataset, set column names
wine <- read_csv("wine.data", col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

# factor + split + subset of vars
wine$Type <- as.factor(wine$Type)
X <- wine[,-1]
Y <- wine$Type
data.c <- wine[, c("Type", "Alcohol", "Ash", "Magnesium")]

# Train + split prep
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

# SVM linear kernal
svm.mod0 <- svm(Type ~ ., data = train.c, kernel = 'linear')
summary(svm.mod0)

svm.pred <- predict(svm.mod0, test.c)
confusionMatrix(svm.pred, test.c$Type)

# Tuned polynominal kernal
gamma.range <- seq(0.5, 5, 0.5)
C.range <- seq(1, 10, 1)

tuned.svm <- tune.svm(Type ~ ., data = train.c, kernel = 'polynomial', gamma = gamma.range, cost = C.range)
svm.mod1 <- tuned.svm$best.model
summary(svm.mod1)

svm.pred1 <- predict(svm.mod1, test.c)
confusionMatrix(svm.pred1, test.c$Type)

# kNN model 
knn.predicted <- knn(train = train.c[, 2:4], test = test.c[, 2:4], cl = train.c$Type, k = 3)
confusionMatrix(knn.predicted, test.c$Type, mode = "prec_recall")

# Comparsion of stats with confusion matrix of 3 models 
print("kNN Model:")
confusionMatrix(knn.predicted, test.c$Type, mode = "prec_recall")
print("Linear Kernal SVM:")
confusionMatrix(svm.pred, test.c$Type)
print("Tuned Polynominal SVM:")
confusionMatrix(svm.pred1, test.c$Type)

