### Lab – Chapter 4 Probabilistic Learning – Classification Using Naïve Bayes

### Read data
eegIDRecord_raw <- read.csv("eegIDRecord.csv")

str(eegIDRecord_raw)

### Normalize all 12 columns
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
eegIDRecord_n <- as.data.frame(lapply(eegIDRecord_raw[1:12],normalize))

### Discretize values in each column
discretize <- function(x){ifelse(x<0.5,0,1)}
eegIDRecord_n_d <- as.data.frame(lapply(eegIDRecord_n[1:12],discretize))

### Create training and test data
test_data <- runif(518,min=1,max=2518)
eegIDRecord_test <- eegIDRecord_n_d [ test_data,]
eegIDRecord_train <- eegIDRecord_n_d [-test_data,]

### Save the labels
eegIDRecord_train$meditation <- factor(eegIDRecord_train$meditation)
eegIDRecord_test$meditation <- factor(eegIDRecord_test$meditation)

train_label <- eegIDRecord_train$meditation
test_label <- eegIDRecord_test$meditation

eegIDRecord_test2 <- eegIDRecord_test [,-3]
eegIDRecord_train2 <- eegIDRecord_train [,-3]


### Training model on the data
library(e1071)
library(gmodels)
classifier <- naiveBayes(eegIDRecord_train2,train_label,laplace = 1)
p <- predict(classifier,eegIDRecord_test2,type="class")

### CrossTable
CrossTable(p, test_label,
          prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
          dnn = c('predicted', 'actual'))
