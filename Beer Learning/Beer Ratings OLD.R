#Learn my Taste Beer App
 
setwd("~/R")

dataset <- read.csv('Beer Ratings.csv')

str(dataset)  


### Factorisation of categorical variables
dataset$Source = factor(dataset$Source,
                        levels = c('Bottle', 'Tap'),
                        labels = c(0,1))

dataset$Type = factor(dataset$Type,
                      levels = c('Ale', 'Lager'),
                      labels = c(0,1))

str(dataset$SubType)
SubType <- as.character(unique(dataset$SubType))
dataset$SubType <- factor(dataset$SubType,
                         levels = SubType,
                         labels = 1:length(SubType))
str(dataset$SubType)
Region <- as.character(unique(dataset$Region))
dataset$Region <- factor(dataset$Region,
                         levels = Region,
                         labels = 1:length(Region))
ID <- 1:nrow(dataset)
dataset$ID <- ID

Sam <- dataset$SamRate
Sally <- dataset$SalRate
Name <- dataset$Name
Region <- dataset$Region
Alco <- dataset$Alco
Type <- dataset$Type
SubType <- dataset$SubType
IBU <- dataset$IBU
Source <- dataset$Source
cor(Sam, Sally)
#22 Dec 17 = 0.675 obs 30
#27 Dec 17 = 0.768 obs 37
#4 Jan 18 = 0.761 obs 42
#13 Jan 18 = 0.767 obs 51
#28 Feb 18 = 0.763 obs 55
plot(Sam, Sally)
abline(lm(formula = Sam ~ Sally), col = 'red')
SSlm <- lm(Sam ~ Sally)
summary(SSlm)

cov(Sam, Sally)
#22 Dec 17 = 143.2 obs 30
#27 Dec 17 = 212.1 obs 37
#4 Jan 18 = 191.2 obs 42
#13 Jan 18 = 173.5 obs 51
#28 Feb = 164.5 obs 55
cor(IBU, Alco)
cor(Alco, Sam)
cor(Alco, Sally)
cor(IBU, Sam)
cor(IBU, Sally)
plot(IBU, Alco)

min(Sam)
min(Sally)
max(Sam)
max(Sally)
mean(Sam)
#22 Dec 17 = 71.33 BOTH obs 30
#27 Dec 17 = 68.51 obs 37
#4 Jan 18 = 69.29 obs 42
#13 Jan 18 = 70.55 obs 51
#28 Feb 18 = 70.96 obs 55
mean(Sally)
#27 Dec 17 = 68.92 obs 37
#4 Jan 18 = 69.64 obs 42
#13 Jan 18 = 71.14 obs 51
#28 Feb 18 = 71.85 obs 55

#with labels
plot(Sam, Sally, text(Name, x = Sam, y = Sally+1, cex = 0.6, col = 'brown'),
     col = 'blue', lwd = 1)
abline(line(1:100, 1:100), lwd = 1, col = 'orange')

#compare bothpar(mar = c(5,4,4,5) + 0.1)
plot(ID, Sam, col = 'blue', main = 'Sam and Sally', pch = 7, type = 'b')
par(new=T)
plot(ID, Sally, col = 'purple', axes = F, ylab = '', pch = 5, type = 'b')
axis(side = 4)
mtext(side = 4, text = 'Sally', line = 3)



library(ggplot2)
#graph of ranks, IBU and Alco
ggplot(data=dataset, aes(x=Sam, y=Sally, color=IBU, size=Alco)) + 
  geom_point() +
  geom_vline(xintercept = mean(Sam),
             color = 'red',
             linetype = 'longdash') +
  geom_hline(yintercept = mean(Sally),
                               color = 'purple',
                               linetype = 'longdash') +
  scale_color_gradientn(colours = c('yellow', 'brown')) +
  labs(title = "Sam and Sally's Beer Rankings",
       colour = 'IBU',
       size = 'Alcohol Content') + 
  xlab('Sam Rank') + 
  ylab('Sally Rank')
  


#insights into residuals of linear reg relationship between Sam and Sally and 'Cook's distance'
plot(lm(formula = Sally ~ Sam, data = dataset))


head(dataset)


 
# SPLIT and PREPRO for Regression machine learning
require(caTools)
set.seed(123)
split = sample.split(dataset$Source, SplitRatio = 22/33)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set = training_set[2:9] 
test_set = test_set[2:9]
TestID <- 1:nrow(test_set)
head(test_set)


#CREATE REGRESSOR for Multi Linear
str(training_set)
regressor = lm(formula = SamRate ~ 
                 SalRate + IBU,
               data = training_set)
summary(regressor)
y_pred = predict(regressor, newdata = test_set)
y_pred
test_set$Pred <- round(y_pred, 2)
test_set$Diff <- test_set$Pred - test_set$SamRate
test_set
mean(abs(test_set$Diff))


#plot accuracy of multi-linear (look into removing scaling of data)
x <- 1:100
par(mar=c(5,4,4,5) + 0.1)
plot(TestID, test_set$SamRate, col = 'red', lwd = 3,
     ylab = 'Original Sam Rate',
     main = 'Prediction of SamRate using MultiLinear')
par(new=T)
plot(TestID, test_set$Pred, col = 'green', 
     pch = 2, axes = F, ylab = '', bty = 'n', lwd = 3)
axis(side = 4, at = x)
mtext(side = 4, text = 'Predicted', line = 3)
### WRONG, THE TWO VARIABLES AGAINST Y ARE ON DIFFERENT SCALES
class(TestID)
TestID <- as.character(TestID)
TestID



#OR use SVR seemingly NOT better than straight linear reg
library(e1071)
regressor = svm(formula = SamRate ~ SalRate + IBU + Alco, 
                data = training_set,
                type = 'eps-regression')
y_pred = predict(regressor, newdata = test_set)
y_pred
test_set$Pred <- round(y_pred, 2)
test_set$Diff <- test_set$Pred - test_set$SamRate
test_set$RateDiff <- test_set$SalRate - test_set$SamRate
test_set$TestDiff <- test_set$Diff - test_set$RateDiff
test_set
mean(abs(test_set$Diff))



#plot accuracy of SVR (look into removing scaling of data)
x <- 1:100
par(mar=c(5,4,4,5) + 0.1)
plot(TestID, test_set$SamRate, col = 'red', lwd = 3,
     ylab = 'Original Sam Rate',
     main = 'Prediction of SamRate using SVR with SalRate and IBU')
par(new=T)
plot(TestID, test_set$Pred, col = 'green', 
     pch = 2, axes = F, ylab = '', bty = 'n', lwd = 3)
axis(side = 4, at = x)
mtext(side = 4, text = 'Predicted', line = 3)



#Re-Pre-Pro
head(training_set)
test_set <- test_set[-(9:10)]
head(test_set)

#Try Kernel SVM (Support Vector Machines) for SOURCE
# require(e1071)
# 3 Type I error, 1 Type II   #
classifier = svm(formula = Source ~ SamRate + SalRate + IBU,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
# 4 Type I error, 0 Type II   ##
classifier = svm(formula = Source ~ SamRate + SalRate + IBU,
                     data = training_set,
                     type = 'C-classification',
                     kernel = 'polynomial')
# 2 Type I error, 0 Type II   #### (within .8 refined!)
classifier = svm(formula = Source ~ SamRate + SalRate + IBU,
                     data = training_set,
                     type = 'C-classification',
                     kernel = 'radial')
# 5 Type I error, o Type II (did not predict any negative results!)
classifier = svm(formula = Source ~ SamRate + SalRate + IBU,
                     data = training_set,
                     type = 'C-classification',
                     kernel = 'sigmoid')


#Try SVM again for TYPE (within .8 refined!)
classifier = svm(formula = Type ~ SamRate + SalRate,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')
test_set <- test_set[-9:-12]
test_set
summary(classifier)
y_pred = predict(classifier, newdata = test_set[-1:-3])
y_pred
test_set$Pred <- y_pred
head(test_set)

#Confusion matrix
cm <- table(test_set[, 4], y_pred)
cm
#still at only 2 incorrect predictions
#NOW at 3 incorrect
#Now at 6...

cm_perc <- 100*(cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
cm_perc
#27 Dec 17, obs 37 84.6%
#4 Jan 18, obs 42 85.7%
#13 Jan 18, obs 51 82.35%
#28 Feb 18, obs 55 66.67%

#Scale Independent variables so you don't crash your computer
#and remove variables you are not calculating
training_set$SamRate <- scale(training_set$SamRate)
training_set$SalRate <- scale(training_set$SalRate)
training_set <- training_set[-1]
training_set <- training_set[-2:-3]
training_set$Source <- factor(training_set$Source,
                              levels = c('Tap','Bottle'),
                              labels = c(0,1))
#Visualise 
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
X2 = seq(min(set[, 3]) - 1, max(set[, 3]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('SamRate', 'SalRate')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -1],
     main = 'Kernel SVM (Training Set)',
     xlab = 'Sam', ylab = 'Sally',
     xlim = range(X1), ylim = range(X2))
##### ALL Z VALUES ARE EQUAL ERROR!
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1 , 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 1] == 1, 'green4', 'red3'))

#Try KNN?
library(class)
y_pred = knn(train = training_set[, -1], 
             test = test_set[, -1],
             cl = training_set[, 1],
             k = 5) 
y_pred
#Confusion matrix
cm <- table(test_set[, 1], y_pred)
cm
cm_perc <- 100*(cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
cm_perc

#Decision Trees?

# Random Forest??? <<< look into next



### 3D Visuals
x = Sam
y = Sally
z = Alco
# install.packages('rgl')
require(rgl)
plot3d(x,y,z, xlab = "Sam", ylab = 'Sally', zlab = 'Alcohol Content')


