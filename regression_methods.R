### Lab – Chapter 6 Forecasting Numeric Data – Regression Methods Lab Part 1 -----------------------------------------------------------------
### The purpose of this lab is to se the linear regression methods
### to identify any correlation between the target feature “meditation” and other features

### read data
mydata <- read.csv("IDRecord-Meditation.csv")
str(mydata)

### produce a scatterplot matrix 
pairs.panels(mydata[c("attention","meditation", "eegRawValue", "blinkStrength")]


### train a simple linear regression model
m <- lm(meditation~attention, data = mydata)

### summary of the trained model
summary(m)

### add a squared term of “attention” to the model
mydata$attention2 <- mydata$attention^2
m2 <- lm(meditation ~ attention + attention2, data = mydata)
summary(m2)

### multiple linear regression analysis using “attention”, “eegRawValue”, and “blinkStrength” as independent variables 
m3 <- lm(meditation ~ attention + eegRawValue + blinkStrength, data = mydata)

### show summary
summary(m3)


### multiple linear regression analysis using all of the variables
m4 <- lm(meditation ~ . , data = mydata)
summary(m4)


### Lab – Chapter 6 Forecasting Numeric Data – Regression Methods Lab Part 2 -------------------------------------------------------------------
### The purpose of this lab is to use the regression tree methods to develop a model that
### distinguishes between meditation and talking states.
library(rpart)
library(gmodels)

### load data
meditation <- read.csv("DiscMeditating.csv")
talking <- read.csv("DiscTalking.csv")

### add columns for labels
lbl <- rep("M", 23920)
meditation <- cbind(meditation, lbl)
str(meditation)

lbl <- rep("T", 23000)
talking <- cbind(talking, lbl)
str(talking)

### randomly take 10% of the data to be test dataset
set.seed(123)

test_meditation_sample <- sample(23920, 2392)
meditation_train <- meditation[-test_meditation_sample,]
meditation_test <- meditation[test_meditation_sample,]

test_talking_sample <- sample(23000, 2300)
talking_train <- talking[-test_talking_sample,]
talking_test <- talking[test_talking_sample,]

### combine two datasets
train <- rbind(meditation_train,talking_train)
test <- rbind(meditation_test,talking_test)

train_lbl <- train[,9]
test_lbl <- test[,9]

train <- train [,-9]
test <- test [,-9]

### use the rpart() function to train the regression tree model, using the train dataset
m.rpart <- rpart(train_lbl ~. , data = train)
m.rpart

### use the predict() function to test the model, using the test dataset
p.rpart <- predict(m.rpart, test, type = "vector")

### compare the predicted values and the true values
CrossTable(test_lbl, p.rpart, dnn = c('actual','predicted'))


### repeat using approximate entropy data in datasets
Norm_meditation <- read.csv("NormMeditatingDApEn.csv")
Norm_talking <- read.csv("NormTalkingDApEn.csv")

str(Norm_meditation)
str(Norm_talking)

lbl <- rep("M", 176)
Norm_meditation <- cbind(Norm_meditation, lbl)
str(Norm_meditation)

lbl <- rep("T", 169)
Norm_talking <- cbind(Norm_talking, lbl)
str(Norm_talking)

set.seed(123)

test_norm_meditation_sample <- sample(176, 17)
Norm_meditation_train <- Norm_meditation[-test_norm_meditation_sample,]
Norm_meditation_test <- Norm_meditation[test_norm_meditation_sample,]


test_norm_talking_sample <- sample(169, 16)
Norm_talking_train <- Norm_talking[-test_norm_talking_sample,]
Norm_talking_test <- Norm_talking[test_norm_talking_sample,]

### Combine two datasets
train2 <- rbind(Norm_meditation_train,Norm_talking_train)
test2 <- rbind(Norm_meditation_test,Norm_talking_test)

train_lbl2 <- train2[,9]
test_lbl2 <- test2[,9]

train2 <- train2 [,-9]
test2 <- test2 [,-9]

m.rpart <- rpart(train_lbl2 ~. , data = train2)
m.rpart

p.rpart <- predict(m.rpart, test2, type = "vector")

CrossTable(test_lbl2, p.rpart, dnn = c('actual','predicted'))



