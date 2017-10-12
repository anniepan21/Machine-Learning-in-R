### Lab – Chapter 5 Divide and Conquer – Classification Using Decision Trees and Rules

disease <- read.csv("U.S._Ch
ronic_Disease_Indicators__CDI_processed.csv")
str(disease)

set.seed(123)
test_sample <- sample(12979,2979)
str(test_sample)

train <- disease[-test_sample,]
test <- disease[test_sample,]

train_lbl <- train[,7]
test_lbl <- test[,7]

prop.table(table(train$Stratification))
prop.table(table(test$Stratification))

train <- train[,-7]
test <- test[,-7]

model <- C5.0(train, train_lbl)
summary(model)

pred <- predict(model,test)
library(gmodels)
CrossTable(test_lbl, pred, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c('actual default','predicted default'))

model_2 <- C5.0(train, train_lbl, trails = 2)
summary(model_2)

pred_2 <- predict(model_2,test)
CrossTable(test_lbl, pred_2, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c('actual default','predicted default'))

model_10 <- C5.0(train, train_lbl, trails = 10)
summary(model_10)

pred_10 <- predict(model_10,test)
CrossTable(test_lbl, pred_10, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c('actual default','predicted default'))
