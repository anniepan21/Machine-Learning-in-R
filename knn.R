### Machine-Learning-in-R
### Lab – Chapter 3 Lazy Learning – Classification Using Nearest Neighbors

### Two datasets, viz., eegData1.csv and eegData2.csv, are given to be the train dataset and the
### test dataset, respectively. Use kNN classification method to train and test the model, and report
### the accuracy of the classification by comparing the values of the target feature of the test
### dataset to the predicted values.

### read data
eegData1 <- read.csv("eegData1.csv")
eegData2 <- read.csv("eegData2.csv")

### inspect data
str(eegData1)
str(eegData2)

### transform attention values to 0 or 1
norm_att <- function(x){ifelse(x<50,0,1)}
att_n1 <- lapply(eegData1$att,norm_att)
att_n2 <- lapply(eegData2$att,norm_att)

### normalize data
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
eegData1_n <- as.data.frame(lapply(eegData1[2:10], normalize))
eegData2_n <- as.data.frame(lapply(eegData2[2:10], normalize))

str(eegData1_n)
str(eegData2_n)

### Extract the “attention” column from eegData1.csv dataset to be the target feature
### Extract the “attention” column from eegData2.csv dataset to be the labels to be compared to the predicted values
train <- eegData1_n
test <- eegData2_n

train_labels <- as.vector(att_n1)
test_labels <- as.vector(att_n2)

### Set k to be an odd integer that is closest to the square root of the number of examples in the dataset
sqrt(295)
#k=17

### Train the knn model
install.packages("class")
library(class)
eegData_test_pred <- knn(train=train, test=test, cl=train_labels, k=17)
