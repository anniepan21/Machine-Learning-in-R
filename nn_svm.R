### Lab – Chapter 7 Black Box Methods – Neural Networks and Support Vector Machines Lab Part 1 -------------------------------------------------------------------
### The purpose of this lab is to use the neural network methods to develop a model that
### distinguishes meditation, reading, and watching video states

### Load data 
Meditation <- read.csv("eegIDRecord-Meditation.csv")
Reading <- read.csv("eegIDRecord-Reading.csv")
Video <- read.csv("eegIDRecord-Video.csv")

### Standardize datasets
Meditation_n <- scale(Meditation)
Reading_n <- scale(Reading)
Video_n <- scale(Video)

### Discretize datasets
M_n2 <- Meditation_n [-1,]
M_n3 <- Meditation_n [-80917,]
M_n4 <- M_n2 - M_n3

med_disc <- matrix(NA,ncol=13,nrow=80916)
for(j in 1:13){
  for(i in 1:80916){
    if(M_n4[i,j]>3) med_disc[i,j] <-3
    else if(M_n4[i,j]>2) med_disc[i,j] <-2
    else if(M_n4[i,j]>1) med_disc[i,j] <-1
    else if(M_n4[i,j]>-1) med_disc[i,j] <-0
    else if(M_n4[i,j]>-2) med_disc[i,j] <- -1
    else if(M_n4[i,j]>-3) med_disc[i,j] <- -2
    else med_disc[i,j] <- -3
  }
}

med_n_disc <- data.frame(med_disc)


R_n2 <- Reading_n [-1,]
R_n3 <- Reading_n [-72098,]
R_n4 <- R_n2 - R_n3

read_disc <- matrix(NA,ncol=13,nrow=72097)
for(j in 1:13){
  for(i in 1:72097){
    if(R_n4[i,j]>3) read_disc[i,j] <-3
    else if(R_n4[i,j]>2) read_disc[i,j] <-2
    else if(R_n4[i,j]>1) read_disc[i,j] <-1
    else if(R_n4[i,j]>-1) read_disc[i,j] <-0
    else if(R_n4[i,j]>-2) read_disc[i,j] <- -1
    else if(R_n4[i,j]>-3) read_disc[i,j] <- -2
    else read_disc[i,j] <- -3
  }
}

read_n_disc <- data.frame(read_disc)


V_n2 <- Video_n [-1,]
V_n3 <- Video_n [-92360,]
V_n4 <- V_n2 - V_n3


video_disc <- matrix(NA,ncol=13,nrow=92359)
for(j in 1:13){
  for(i in 1:92359){
    if(V_n4[i,j]>3) video_disc[i,j] <-3
    else if(V_n4[i,j]>2) video_disc[i,j] <-2
    else if(V_n4[i,j]>1) video_disc[i,j] <-1
    else if(V_n4[i,j]>-1) video_disc[i,j] <-0
    else if(V_n4[i,j]>-2) video_disc[i,j] <- -1
    else if(V_n4[i,j]>-3) video_disc[i,j] <- -2
    else video_disc[i,j] <- -3
  }
}

video_n_disc <- data.frame(video_disc)


### Add a column for labels

med_n_disc$lbl <- "meditating"
read_n_disc$lbl <- "reading"
video_n_disc$lbl <- "video"


str(med_n_disc)
str(read_n_disc)
str(video_n_disc)


### Split data into test and train dataset

set.seed(123)

test_med_sample <- sample(80916, 80916*.25)
med_train <- med_n_disc[-test_med_sample,]
med_test <- med_n_disc[test_med_sample,]


test_read_sample <- sample(72097, 72097*.25)
read_train <- read_n_disc[-test_read_sample,]
read_test <- read_n_disc[test_read_sample,]


test_video_sample <- sample(92359, 92359*.25)
video_train <- video_n_disc[-test_video_sample,]
video_test <- video_n_disc[test_video_sample,]


### Combine three test datasets into one single test dataset, and three train datasets into a  single train dataset

train <- rbind(med_train,read_train, video_train)
test <- rbind(med_test,read_test, video_test)

train_lbl = as.factor(train$lbl)
train$lbl = NULL

test_lbl = as.factor(test$lbl)
test$lbl = NULL
tail(train)


### Train the model

library(gmodels)
library(nnet)

nn <- nnet(train_lbl ~., data = train, size =1)
p <- predict(nn, test, type ="class")

### Evaluate Results
CrossTable(test_lbl , p)




### Lab – Chapter 7 Black Box Methods – Neural Networks and Support Vector Machines Lab Part 2 ----------------------------------------------------------------------
### The purpose of this lab is to use neural net and SVM to
### build brain state models and compare the performance of neural net and SVM for the given
### problem.

### Load data 
Meditation_raw <- read.csv("eegIDRecord-Meditation-2.csv")
Reading_raw <- read.csv("eegIDRecord-Reading-2.csv")
Video_raw <- read.csv("eegIDRecord-Video-2.csv")

### Randomly select 30% data
set.seed(123)

med_sample <- sample(80917, 24275)
Meditation <- Meditation_raw[med_sample,]

read_sample <- sample(72098, 21629)
Reading <- Reading_raw[read_sample,]

video_sample <- sample(92360, 27708)
Video <- Video_raw[video_sample,]

### Standardize datasets
Meditation_n <- as.data.frame(scale(Meditation))
Reading_n <- as.data.frame(scale(Reading))
Video_n <- as.data.frame(scale(Video))

str(Meditation_n)
str(Reading_n)
str(Video_n)

### Discretize datasets

M_n2 <- Meditation_n [-1,]
M_n3 <- Meditation_n [-24275,]
M_n4 <- M_n2 - M_n3

med_disc <- matrix(NA,ncol=5,nrow=24274)
for(j in 1:5){
  for(i in 1:24274){
    if(M_n4[i,j]>3) med_disc[i,j] <-3
    else if(M_n4[i,j]>2) med_disc[i,j] <-2
    else if(M_n4[i,j]>1) med_disc[i,j] <-1
    else if(M_n4[i,j]>-1) med_disc[i,j] <-0
    else if(M_n4[i,j]>-2) med_disc[i,j] <- -1
    else if(M_n4[i,j]>-3) med_disc[i,j] <- -2
    else med_disc[i,j] <- -3
  }
}

med_n_disc <- data.frame(med_disc)


R_n2 <- Reading_n [-1,]
R_n3 <- Reading_n [-21629,]
R_n4 <- R_n2 - R_n3

read_disc <- matrix(NA,ncol=5,nrow=21628)
for(j in 1:5){
  for(i in 1:21628){
    if(R_n4[i,j]>3) read_disc[i,j] <-3
    else if(R_n4[i,j]>2) read_disc[i,j] <-2
    else if(R_n4[i,j]>1) read_disc[i,j] <-1
    else if(R_n4[i,j]>-1) read_disc[i,j] <-0
    else if(R_n4[i,j]>-2) read_disc[i,j] <- -1
    else if(R_n4[i,j]>-3) read_disc[i,j] <- -2
    else read_disc[i,j] <- -3
  }
}

read_n_disc <- data.frame(read_disc)


V_n2 <- Video_n [-1,]
V_n3 <- Video_n [-27708,]
V_n4 <- V_n2 - V_n3


video_disc <- matrix(NA,ncol=5,nrow=27707)
for(j in 1:5){
  for(i in 1:27707){
    if(V_n4[i,j]>3) video_disc[i,j] <-3
    else if(V_n4[i,j]>2) video_disc[i,j] <-2
    else if(V_n4[i,j]>1) video_disc[i,j] <-1
    else if(V_n4[i,j]>-1) video_disc[i,j] <-0
    else if(V_n4[i,j]>-2) video_disc[i,j] <- -1
    else if(V_n4[i,j]>-3) video_disc[i,j] <- -2
    else video_disc[i,j] <- -3
  }
}

video_n_disc <- data.frame(video_disc)


str(med_n_disc)
str(read_n_disc)
str(video_n_disc)


### Approximate Entropy

library(pracma)  

### Meditation

varnum = dim(med_n_disc)[2]  
length = as.numeric(dim(med_n_disc)[1])  
mxlengh = floor(length/512-6) 
   
AproxEntrp <- matrix( rep(NA, varnum * mxlengh), ncol = varnum )  
  
for (i in 1:mxlengh)
{
	start = (i-1)*512+1    
	end = start + 512*6   
	for (j in 1:varnum)
	{      
	AproxEntrp[i,j] <- approx_entropy(med_n_disc[start:end, j], edim = 2,                                         
	r = 0.2*sd(med_n_disc[start:end, j]), elag = 4)   
	}  
} 


med_n_disc <- data.frame(AproxEntrp)
str(med_n_disc)
head(med_n_disc)


### Reading 

varnum = dim(read_n_disc)[2]  
length = as.numeric(dim(read_n_disc)[1])  
mxlengh = floor(length/512-6) 
AproxEntrp <- matrix( rep(NA, varnum * mxlengh), ncol = varnum )  
  
for (i in 1:mxlengh)
{
	start = (i-1)*512+1    
	end = start + 512*6   
	for (j in 1:varnum)
	{      
	AproxEntrp[i,j] <- approx_entropy(read_n_disc[start:end, j], edim = 2,                                         
	r = 0.2*sd(read_n_disc[start:end, j]), elag = 4)   
	}  
} 


read_n_disc <- data.frame(AproxEntrp)
str(read_n_disc)
head(read_n_disc)


### Video

varnum = dim(video_n_disc)[2]  
length = as.numeric(dim(video_n_disc)[1])  
mxlengh = floor(length/512-6) 
AproxEntrp <- matrix( rep(NA, varnum * mxlengh), ncol = varnum )  
  
for (i in 1:mxlengh)
{
	start = (i-1)*512+1    
	end = start + 512*6   
	for (j in 1:varnum)
	{      
	AproxEntrp[i,j] <- approx_entropy(video_n_disc[start:end, j], edim = 2,                                         
	r = 0.2*sd(video_n_disc[start:end, j]), elag = 4)   
	}  
} 


video_n_disc <- data.frame(AproxEntrp)
str(video_n_disc)
head(video_n_disc)


### Add a column for labels

med_n_disc$lbl <- "meditating"
read_n_disc$lbl <- "reading"
video_n_disc$lbl <- "video"


str(med_n_disc)
str(read_n_disc)
str(video_n_disc)


### Split data into test and train dataset

set.seed(12345)

test_med_sample <- sample(41, 10)
med_train <- med_n_disc[-test_med_sample,]
med_test <- med_n_disc[test_med_sample,]


test_read_sample <- sample(36, 9)
read_train <- read_n_disc[-test_read_sample,]
read_test <- read_n_disc[test_read_sample,]


test_video_sample <- sample(48, 12)
video_train <- video_n_disc[-test_video_sample,]
video_test <- video_n_disc[test_video_sample,]


### Combine three test datasets into one single test dataset, and three train datasets into a  single train dataset

train <- rbind(med_train,read_train, video_train)
test <- rbind(med_test,read_test, video_test)

train_lbl = as.factor(train$lbl)
train$lbl = NULL

test_lbl = as.factor(test$lbl)
test$lbl = NULL


### Train the model

library(gmodels)
library(nnet)

nn <- nnet(train_lbl ~., data = train, size =1)
p <- predict(nn, test, type ="class")
CrossTable(test_lbl , p)

### Increase number of hidden nodes
nn <- nnet(train_lbl ~., data = train, size =3)
p <- predict(nn, test, type ="class")
CrossTable(test_lbl , p)


### Using SVM Algorithm

install.packages("kernlab")
library(e1071)
library(kernlab)

m.svm <- ksvm(train_lbl ~., data = train, kernel="vanilladot", C=1)
p <- predict(m.svm , test, type ="response")
CrossTable(test_lbl , p)


### Using Gaussian RBF kernel

m.svm <- ksvm(train_lbl ~., data = train, kernel="rbfdot", C=1)
p <- predict(m.svm , test, type ="response")
CrossTable(test_lbl , p)








