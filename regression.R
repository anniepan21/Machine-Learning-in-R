### Lab – Chapter 6 Forecasting Numeric Data – Regression Methods Lab 1

### Load data
mydata <- read.csv("IDRecord-Meditation.csv")
str(mydata)

### Produce scatterplot matrix 
pairs.panels(mydata[c("attention","meditation", "eegRawValue", "blinkStrength")]


### Train a simple linear regression model
m <- lm(meditation~attention, data = mydata)
summary(m)

### Add a squared term of “attention” to the model
mydata$attention2 <- mydata$attention^2
m2 <- lm(meditation ~ attention + attention2, data = mydata)
summary(m2)

### Use “attention”, “eegRawValue”, and “blinkStrength” as independent variables and do a multiple linear regression analysis
m3 <- lm(meditation ~ attention + eegRawValue + blinkStrength, data = mydata)
summary(m3)


### Using all of the variables as predictors
m4 <- lm(meditation ~ . , data = mydata)
summary(m4)
