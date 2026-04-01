library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)
library(readr)
library(class)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

#Read dataset, narrow down to one borough, clear NAs
NY_House_Dataset <- read_csv("data/NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")
Queens <- NY_House_Dataset[NY_House_Dataset$BOROUGH == 4, ]
Queens <- Queens[!is.na(Queens$`SALE PRICE`) & !is.na(Queens$`GROSS SQUARE FEET`), ]

#Histogram and box plot to show outliers EDA
summary(Queens)
summary(Queens$'SALE PRICE')
hist(log10(Queens$`SALE PRICE`), 
     main = "Log10 of Queens Sale Prices", 
     xlab = "Sale Price (Log10)",
     breaks = 50)

Above_Zero <- Queens[Queens$`SALE PRICE` > 0, ]
boxplot(log10(Above_Zero$'SALE PRICE'), 
        main="Log10 Of Queens Sale Prices", 
        ylab="Sale Price (Log10)")

Above_Zero$`GROSS SQUARE FEET` <- as.numeric(Above_Zero$`GROSS SQUARE FEET`)
Above_Zero <- Above_Zero[Above_Zero$`GROSS SQUARE FEET` > 0, ]
ggplot(Above_Zero, aes(x = log10(`GROSS SQUARE FEET`), y = log10(`SALE PRICE`))) +
  geom_point() +
  stat_smooth(method = "lm", col="red") + 
  xlab("Log10(Gross Square Feet)") +
  ylab("Log10(Sale Price)")

# Units and year built summaries EDA
summary(Queens$`RESIDENTIAL UNITS`)
hist(log10(Queens$`RESIDENTIAL UNITS`), main="Distribution of Residential Units", xlab="Number of Units")
summary(Queens$`YEAR BUILT`)
Queens <- Queens[Queens$`YEAR BUILT` > 0, ]
hist(Queens$`YEAR BUILT`, main="Distribution of Year Built", xlab="Year")

#Linear regression

#Further Data Cleaning
data <- Above_Zero[Above_Zero$`YEAR BUILT` > 1800, ]
data$`RESIDENTIAL UNITS` <- as.numeric(data$`RESIDENTIAL UNITS`)

#One Round Cross Validation set up 
set.seed(15)
train_model <- sample(nrow(data), 0.7 * nrow(data))
train <- data[train_model, ]
test <- data[-train_model, ]

#Variable combinations
model1 <- lm(log10(`SALE PRICE`) ~ log10(`GROSS SQUARE FEET`), data = train)
model2 <- lm(log10(`SALE PRICE`) ~ `RESIDENTIAL UNITS`, data = train)
model3 <- lm(log10(`SALE PRICE`) ~ log10(`GROSS SQUARE FEET`) + `RESIDENTIAL UNITS`, data = train)
model4 <- lm(log10(`SALE PRICE`) ~ log10(`GROSS SQUARE FEET`) + `RESIDENTIAL UNITS` + `YEAR BUILT`, data = train)

#Adjusted R squared/ summaries
summary(model1)
summary(model2)
summary(model3)
summary(model4)

#Classification

targets <- c("ASTORIA", "FLUSHING-NORTH", "BAYSIDE", "WOODSIDE")
data.c <- data[data$NEIGHBORHOOD %in% targets, ]
data.c <- data.c[, c("NEIGHBORHOOD", "SALE PRICE", "GROSS SQUARE FEET", "RESIDENTIAL UNITS")]

data.c <- na.omit(data.c)
data.c$NEIGHBORHOOD <- as.factor(data.c$NEIGHBORHOOD)
data.c$`SALE PRICE` <- scale(data.c$`SALE PRICE`)
data.c$`GROSS SQUARE FEET` <- scale(data.c$`GROSS SQUARE FEET`)
data.c$`RESIDENTIAL UNITS` <- scale(data.c$`RESIDENTIAL UNITS`)

colnames(data.c) <- c("NEIGHBORHOOD", "SALE_PRICE", "GROSS_SQUARE_FEET", "RESIDENTIAL_UNITS")

set.seed(70)
split.c <- sample(nrow(data.c), 0.7 * nrow(data.c))
train.c <- data.c[split.c, ]
test.c  <- data.c[-split.c, ]

#kNN
knn.predicted <- knn(train = train.c[, 2:4], test = test.c[, 2:4], cl = train.c$NEIGHBORHOOD, k = 7)
confusionMatrix(knn.predicted, test.c$NEIGHBORHOOD, mode = "prec_recall")

#Decision Trees
tree.model <- rpart(NEIGHBORHOOD ~ ., data = train.c, method = "class")
rpart.plot(tree.model, main="Decision Tree for Queens Neighborhoods")

tree.predicted <- predict(tree.model, test.c, type = "class")
confusionMatrix(tree.predicted, test.c$NEIGHBORHOOD, mode = "prec_recall")

#Random Forest
rf.model <- randomForest(NEIGHBORHOOD ~ ., data = train.c, ntree = 100)

rf.predicted <- predict(rf.model, test.c)
confusionMatrix(rf.predicted, test.c$NEIGHBORHOOD, mode = "prec_recall")



