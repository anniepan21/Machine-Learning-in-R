### Lab – Chapter 5 Divide and Conquer – Classification Using Decision Trees and Rules Lab 2
library(Rweka)
library(gmodels)

### Load data
mydata <- read.csv("wisc_bc_data.csv")
str(mydata)

### Extract label column
lbl <- mydata[,2]
mydata$id <- NULL
mydata$diagnosis <- NULL
str(mydata)

### Train and test the OneR() model
m <- OneR(lbl~.,data=mydata)
pred <- predict(m, mydata)
CrossTable(lbl, pred, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c('actual default','predicted default'))

### Train and test the JRip() model
data_JRip <- JRip(lbl~., data=mydata)
pred <- predict(data_JRip, mydata)
CrossTable(lbl, pred, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c('actual default','predicted default'))


