### Lab – Chapter 9 Finding Groups of Data – Clustering with k-means ---------------------------------------------------------------
### The purpose of this lab is to use k-means algorithm
### to create clusters which are intended to match the known brain states
### the brain waves data are associated to

### Load data 
Meditating <- read.csv("Meditating.csv")
Talk <- read.csv("Talking.csv")
Idle <- read.csv("Idle.csv")

### Add a column for labels
Meditating$lbl <- as.factor("Meditating")
Talk$lbl <- as.factor("Talking")
Idle$lbl <- as.factor("Idle")

### Join three data sets
joint_dataset <- rbind(Meditating, Talk, Idle)

### Standardize and discretize all columns
joint_dataset_n <- as.data.frame(scale(joint_dataset[1:7]))

### Discretize datasets
Meditating_n <- joint_dataset_n[1:23920,]
Talk_n <- joint_dataset_n[23921:46920,]
Idle_n <- joint_dataset_n[46921:69920,]

M_n2 <- Meditating_n[-1,]
M_n3 <- Meditating_n[-23920,]
M_n4 <- M_n2 - M_n3

T_n2 <- Talk_n[-1,]
T_n3 <- Talk_n[-23000,]
T_n4 <- T_n2 - T_n3

I_n2 <- Idle_n[-1,]
I_n3 <- Idle_n[-23000,]
I_n4 <- I_n2 - I_n3


med_disc <- matrix(NA,ncol=7,nrow=23919)
for(j in 1:7){
  for(i in 1:23919){
    if(M_n4[i,j] > (0.1*2^i)) med_disc[i,j] <- M_n4[i,j]
    else if(M_n4[i,j] < (0.1*2^(i+1))) med_disc[i,j] <- M_n4[i,j]
    else if(M_n4[i,j] > -0.1 && M_n4[i,j] < -0.1) med_disc[i,j] <- 0
    else if(M_n4[i,j] > (-0.1*2^(i+1))) med_disc[i,j] <- -M_n4[i,j]
    else if(M_n4[i,j] < (-0.1*2^i)) med_disc[i,j] <- -M_n4[i,j]
  }
}

med_n_disc <- data.frame(med_disc)



talk_disc <- matrix(NA,ncol=7,nrow=22999)
for(j in 1:7){
  for(i in 1:22999){
  if(T_n4[i,j] > (0.1*2^i)) talk_disc[i,j] <- T_n4[i,j]
    else if(T_n4[i,j] < (0.1*2^(i+1))) talk_disc[i,j] <- T_n4[i,j]
    else if(T_n4[i,j] > -0.1 && T_n4[i,j] < -0.1) talk_disc[i,j] <- 0
    else if(T_n4[i,j] > (-0.1*2^(i+1))) talk_disc[i,j] <- -T_n4[i,j]
    else if(T_n4[i,j] < (-0.1*2^i)) talk_disc[i,j] <- -T_n4[i,j]
  }
}

talk_n_disc <- data.frame(talk_disc)

idle_disc <- matrix(NA,ncol=7,nrow=22999)
for(j in 1:7){
  for(i in 1:22999){
    if(I_n4[i,j] > (0.1*2^i)) idle_disc[i,j] <- I_n4[i,j]
    else if(I_n4[i,j] < (0.1*2^(i+1))) idle_disc[i,j] <- I_n4[i,j]
    else if(I_n4[i,j] > -0.1 && I_n4[i,j] < -0.1) idle_disc[i,j] <- 0
    else if(I_n4[i,j] > (-0.1*2^(i+1))) idle_disc[i,j] <- -I_n4[i,j]
    else if(I_n4[i,j] < (-0.1*2^i)) idle_disc[i,j] <- -I_n4[i,j]
  }
}

idle_n_disc <- data.frame(idle_disc)


str(med_n_disc)
str(talk_n_disc)
str(idle_n_disc)

### Approximate Entropy

library(pracma)  

### Meditation

varnum = dim(med_n_disc)[2]  
length = as.numeric(dim(med_n_disc)[1])  
mxlengh = floor(length/128-5) 
   
AproxEntrp <- matrix( rep(NA, varnum * mxlengh), ncol = varnum )  
  
for (i in 1:mxlengh)
{
	start = (i-1)*128+1    
	end = start + 128*5  
	for (j in 1:varnum)
	{      
	AproxEntrp[i,j] <- approx_entropy(med_n_disc[start:end, j], edim = 2,                                         
	r = 0.2*sd(med_n_disc[start:end, j]), elag = 4)   
	}  
} 


med_n_disc <- data.frame(AproxEntrp)
str(med_n_disc)
head(med_n_disc)


### Talking

varnum = dim(talk_n_disc)[2]  
length = as.numeric(dim(talk_n_disc)[1])  
mxlengh = floor(length/128-5) 
AproxEntrp <- matrix( rep(NA, varnum * mxlengh), ncol = varnum )  
  
for (i in 1:mxlengh)
{
	start = (i-1)*128+1    
	end = start + 128*5   
	for (j in 1:varnum)
	{      
	AproxEntrp[i,j] <- approx_entropy(talk_n_disc[start:end, j], edim = 2,                                         
	r = 0.2*sd(talk_n_disc[start:end, j]), elag = 4)   
	}  
} 



talk_n_disc <- data.frame(AproxEntrp)
str(talk_n_disc)
head(talk_n_disc)


### Idle

varnum = dim(idle_n_disc)[2]  
length = as.numeric(dim(idle_n_disc)[1])  
mxlengh = floor(length/128-5) 
AproxEntrp <- matrix( rep(NA, varnum * mxlengh), ncol = varnum )  
  
for (i in 1:mxlengh)
{
	start = (i-1)*128+1    
	end = start + 128*5   
	for (j in 1:varnum)
	{      
	AproxEntrp[i,j] <- approx_entropy(idle_n_disc[start:end, j], edim = 2,                                         
	r = 0.2*sd(idle_n_disc[start:end, j]), elag = 4)   
	}  
} 


idle_n_disc <- data.frame(AproxEntrp)
str(idle_n_disc)
head(idle_n_disc)


### Add a column for labels

med_n_disc$lbl <- "meditating"
talk_n_disc$lbl <- "talking"
idle_n_disc$lbl <- "idle"

str(med_n_disc)
str(talk_n_disc)
str(idle_n_disc)


### Join three data sets
mydata <- rbind(med_n_disc,talk_n_disc,idle_n_disc)
mydata$lbl <- as.factor(mydata$lbl)

lbl <- mydata$lbl
mydata <- mydata[-8]

### Clustering
install.packages("stats")
library(stats)

myclusters <- kmeans(mydata,3)
table(myclusters$cluster)

### Mapping
vec <- as.factor(myclusters$cluster)
levels(vec) <- c("meditating","talking","idle")

### Crosstable for 6 permutations
library(gmodels)
CrossTable(lbl , vec)

levels(vec) <- c("meditating","idle","talking")
CrossTable(lbl , vec)

levels(vec) <- c("idle","talking","meditating")
CrossTable(lbl , vec)

levels(vec) <- c("idle","meditating","talking")
CrossTable(lbl , vec)

levels(vec) <- c("talking","idle","meditating")
CrossTable(lbl , vec)

levels(vec) <- c("talking","meditating","idle")
CrossTable(lbl , vec)

### Display centers
myclusters$centers
