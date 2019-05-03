####################################### 
# BEER LEARNING v1
#######################################

#######################################
# Packages and Data
#######################################
setwd("~/R/Beer Learning")
library(caTools)
library(randomForest)
library(mlr)
library(cluster)
library(caret)
beer <- read.csv('Beer Ratings.csv')

str(beer)

#######################################
# Linear Analysis
#######################################

cor(beer$SamRate, beer$Alco) # nothing linear
cor(beer$SamRate, beer$IBU)

plot(SamRate~Alco, data = beer)
plot(SamRate~IBU, data = beer) # don't bother with linear regression, not even close
# look into multi var regression/correlation
plot(beer$SamRate) # decent distribution of rankings


#######################################
# K-means Clustering
#######################################

# for only alco to samrate

ms <- cbind(beer$Alco, beer$SamRate)

# Elbow Method
set.seed(6)
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(ms, i)$withins)
plot(1:10, wcss, type = 'b') # 3

# Cluster - Kmeans
set.seed(7)
kmeans1 <- kmeans(ms, 3, iter.max = 500, nstart = 10)
kmeans1$cluster 
plot(kmeans1$cluster) # car_speed sd variation against the average speed decreases when the sun is down (~1900 to ~0400 next day)

# Visualise kmeans1
clusplot(ms,
         kmeans1$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         plotchar = FALSE,
         span = TRUE, 
         main = 'SamRate Against Alcohol Content',
         xlab = 'Scaled SamRate',
         ylab = 'Scaled Alco'
)

#######################################
# Random Forest Classification for Type 
#######################################

# Prepping dataset
dataset = beer[c(3,6:8)]
levels(dataset$Type)
# Encoding the target feature as factor
dataset$Type = factor(dataset$Type, labels = c(0,1), levels = c('Ale', 'Lager'))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Type, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling

## Random Forest is not based on Euclidean distances, but scale it so we don't break the code
### this is because of the 0.01 increments in grid vectors
training_set[-1] = scale(training_set[-1])
test_set[-1] = scale(test_set[-1])

# Fitting classifier to the Training set
library(randomForest)
classifier = randomForest(x = training_set[-1],
                          y = training_set$Type,
                          ntree = 10)


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-1])
y_pred2 = predict(classifier, newdata = training_set[-1])

# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred)
cm2 = table(training_set[, 1], y_pred2) #actually really accurate...

#######################################
# Random Forest Regression
#######################################

dataset <- beer[c(2:8)]

# Split
split <- sample.split(dataset$SamRate, SplitRatio = 0.75)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

# Create Regressor
library(randomForest)
set.seed(1234)
regressor <- randomForest(x = training_set[-8], 
                         y = training_set$SamRate,
                         ntree = 500) # no difference beyond this number of trees (thus far) # minimise trees for CPU?
getTree(regressor)

# Predict with Rand Forest
y_pred <- predict(regressor, test_set)

plot(test_set$SamRate, y_pred) # decent...
abline(1:100, 1:100, col = 'red', lwd = 2)  # like threshold is 70+? maybe 60 to 70 and -60 is dislike
summary(lm(test_set$SamRate~y_pred)) # a stat sig linear relation between pred and original

###
# Importance and feature weighting
###

set.seed(1234)
reg_imp <- randomForest(x = training_set[-8],
                         y = training_set$SamRate,
                         importance = TRUE,
                         ntree = 500)

y_pred_imp <- predict(reg_imp, test_set)

plot(test_set$SamRate, y_pred_imp)
abline(1:100,1:100, col = 'light blue', lwd = 10)
summary(lm(test_set$SamRate~y_pred_imp))

reg_class <- randomForest(x = training_set[-8],
                          y = training_set$SamRate,
                          classwt = 100)

y_pred_class <- predict(reg_class, test_set)

plot(test_set$SamRate, y_pred_class)
abline(1:100,1:100, col = 'light green', lwd = 10)
summary(lm(test_set$SamRate~y_pred_class))  # with class at 100, p-value decreases (but both still stat sig) 
                                            # also R square increased 1.5% in explainable variability 
                                            # the HIGHEST R squared is in the original default algorithm


#######################################
# Logistic Regression
#######################################

dataset <- beer[,6:8]

# Split
library(caTools)
set.seed(123)
split = sample.split(dataset$SamRate, SplitRatio = 0.75)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)

# Feature Scaling
training_set[1:3] = scale(training_set[1:3])
test_set[1:3] = scale(test_set[1:3]) # how to scale with values 0<x<1????

# Fitting Logist Regression to the Training Set
classifier = glm(formula = SamRate ~ ., 
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
# Test with a new vector 
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix (to test predictor)
cm = table(test_set[, 3], y_pred)


#######################################
# DIY
#######################################

head(beer)

# starting from 0

beerlearn1.0 <- function(Type, SubType, Source, IBU, Alco) {
  x = 0
  if (Type == 'Ale') {
    x = x + 19
  } 
  else if (Type != 'Ale') {
    x = x + 12
  } 
  if (SubType == 'Pale') {
    x = x + 17
  } 
  else if (SubType == 'Amber') {
    x = x + 13 
  } 
  else {
    x = x + 8
  } 
  if (Source == 'Tap') {
    x = x + 16
  } 
  else {
    x = x + 8
  } 
  if (IBU < 10 & Type == 'Ale') {
    x = x - 10
  } 
  else if (IBU < 10 & Type == 'Lager' & Alco < 4.8) {
    x = x + 20
  }
  else if (IBU > 9 & IBU < 15) {
    x = x + 9
  } 
  else if (IBU > 14 & IBU < 21) {
    x = x + 14
  } 
  else {
    x = x + 2
  } 
  if (Alco < 4.0) {
    x = x - 7
  } 
  else if (Alco > 3.9 & Alco < 5.0) {
    x = x + 9 
  }
  else if (Alco > 4.9 & Alco < 6.0) {
    x = x + 12
  } 
  else if (Alco > 5.9) {
    x = x + 8
  } 
  return(x)
}

plot(beer$SamRate)
View(beer)
head(beer)
str(beer)
levels(beer$SubType)

# try starting from 50 and adding or subtracting from the score given the input parameters
# start at ranking between -3 and 3

beerlearn2.0 <- function(Type, SubType, Source, IBU, Alco) {
  x = 50
  if (Type == 'Ale') { # Types #
    x = x + 7
  } 
  else if (Type == 'Lager') {
    x = x - 2
  } 
  else if (Type == 'Mead') {
    x = x + 12
  }
  else {
    x = x
  }  # Subtypes #
  if (SubType == 'Pale' | SubType == 'Golden' | SubType == 'Summer' | SubType == 'Pacific') {
    x = x + 5
  } 
  else if (SubType == 'Kolsch' | SubType == 'Amber' | SubType == 'Dubbel') {
    x = x + 3
  }
  else if (SubType == 'Witbier' | SubType == 'Pilsner') {
    x = x + 1
  }
  else if (SubType == 'Scotch' | SubType == 'Lager') {
    x = x - 1
  }
  else if (SubType == 'Light' | SubType == 'IPA') {
    x = x - 3
  }
  else if (SubType == 'Porter' | SubType == 'Guinness' | SubType == 'Stout') {
    x = x - 5
  }
  else {
    x = x 
  } # Source #
  if (Source == 'Bottle') {
    x = x - 3
  } 
  else if (Source == 'Tap') {
    x = x + 6
  }
  else {
    x = x
  } # IBU #
  if (IBU >= 0 & IBU < 10) {
    x = x + 5
  } 
  else if (IBU >= 10 & IBU <= 20) {
    x = x + 3
  }
  else if (IBU > 20 & IBU < 30) {
    x = x - 1
  }
  else if (IBU >= 30 & IBU < 45) {
    x = x - 3
  } 
  else if (IBU >= 45) {
    x = x - 5
  } # Alco #
  if ((Alco >= 4.0 & Alco <= 4.5) | Alco > 6.5) {
    x = x + 5
  }
  else if (Alco > 4.5 & Alco < 5.0) {
    x = x + 3
  }
  else if ((Alco >= 5.0 & Alco <= 5.5) | (Alco < 4.0 & Alco >= 3.5)) {
    x = x + 1
  }
  else if (Alco > 5.5 & Alco <= 6.5) {
    x = x - 1
  }
  else if (Alco < 3.5) {
    x = x - 5
  } # Combination exceptions
  if (SubType == 'IPA' & Alco > 5.9) {
    x = x + 3
  }
  else if (SubType == 'IPA' & IBU < 30) {
    x = x + 3 
  }
  else if (SubType == 'Dubbel' & IBU < 40 & Alco > 5.5) {
    x = x + 5
  }
  else if (SubType == 'Pacific' & IBU < 25 & Alco < 5.5) {
    x = x + 3
  }
  else if (SubType == Type) {
    x = x - 1
  }
  else if (SubType == 'Lager' & IBU < 10) {
    x = x + 3
  }
  else if ((SubType == 'Golden' | SubType == 'Summer') & IBU < 20) {
    x = x + 3
  }
  else if (SubType == 'Cider' & IBU < 5) {
    x = x + 10
  }
  else if (SubType == 'Mead' & IBU < 5) {
    x = x + 12
  }
  else if (SubType == 'Kolsch' & IBU < 30) {
    x = x + 5
  }
  else if (Type == 'Lager' & IBU > 25) {
    x = x - 2
  }
  else if ((IBU > 5 & IBU < 15) & (Alco > 4.0 & Alco < 5.0)) {
    x = x + 5
  }
  else if (SubType == 'Witbier' & IBU < 20) {
    x = x + 5
  }
  return(x)
}
head(beer)
beerlearn2.0('Ale','Pale','Tap', 20 , 4.7)
beerlearn2.0('Ale','Porter','Bottle', 35, 6.0)



beerlearn2.1 <- function(x) {
  y <- vector()
    for (i in 1:nrow(x)) {
    y[i] <- beerlearn2.0(x[i,1], x[i,2], x[i,3], x[i,4], x[i,5])
    }
  return(y)
}

library(dplyr)

test <- beer[,3:7]
str(test)

test$Type <- test$Type %>%  as.character()
test$SubType <- test$SubType %>%  as.character()
test$Source <- test$Source %>%  as.character()
Predicted <- beerlearn2.1(test)

beer_pred <- cbind(beer, Predicted)

plot(Predicted~SamRate, data = beer_pred) # sort of...

cor(beer_pred$Predicted, beer_pred$SamRate) # 0.57 is okay...
summary(lm(Predicted~SamRate, data = beer_pred)) # stat sig, but only 33% explainable variability


test[1,2]
sapply(test, beerlearn2.0)
