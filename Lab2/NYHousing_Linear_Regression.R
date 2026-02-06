library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("C:/Users/fangw2/Desktop/DATA_ANALYTICS/Lab2/NY-House-Dataset.csv")

dataset <- NY_House_Dataset


ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()


## filter data
dataset <- dataset[dataset$PRICE<195000000,]

## column names
names(dataset)

## fit linear model
lmod0 <- lm(PRICE~PROPERTYSQFT, data = dataset)
lmod1 <- lm(PRICE~BEDS, data = dataset)
lmod2 <- lm(PRICE~BATH, data = dataset)
lmod_best <- lm(PRICE~PROPERTYSQFT+BEDS+BATH, data = dataset)

## print model output
summary(lmod0)
summary(lmod1)
summary(lmod2)

## R squared of this which has all three factors is most significant
summary(lmod_best)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod0)

plot(PRICE~BEDS, data = dataset)
abline(lmod1)

plot(PRICE~BATH, data = dataset)
abline(lmod2)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

lmod0 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
lmod1 <- lm(log10(PRICE)~log10(BEDS), data = dataset)
lmod2 <- lm(log10(PRICE)~BATH, data = dataset)

## print model output
summary(lmod0)
summary(lmod1)
summary(lmod2)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod0)

plot(log10(PRICE)~log10(BEDS), data = dataset)
abline(lmod1)

plot(log10(PRICE)~BATH, data = dataset)
abline(lmod2)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(lmod0, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


ggplot(dataset, aes(x = BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## filter data

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

lmod0 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
lmod1 <- lm(log10(PRICE)~log10(BEDS), data = dataset)
lmod2 <- lm(log10(PRICE)~BATH, data = dataset)

## print model output
summary(lmod0)
summary(lmod1)
summary(lmod2)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod0)

plot(log10(PRICE)~log10(BEDS), data = dataset)
abline(lmod1)

plot(log10(PRICE)~BATH, data = dataset)
abline(lmod2)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

### THE END ###

