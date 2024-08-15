# For Python:

#To clean and reset the work space
rm(list=ls())

#Importing the necessary packages required to perform 
library(TSA)
library(fUnitRoots)
library(lmtest)
library(tseries)
library(forecast)
library(FitAR)
library(dplyr)

#Changing the work directory as per the file location
setwd("C:/Users/vaibh/Desktop/Master of Data Science/Semester - 4/Time Series Analysis/Project")

#Importing the dataset for the analysis
pythondata <- read.csv("TotalQuestions.csv")
pythondata <- tail(pythondata,-3)

#Checking the initial class of the dataset
class(pythondata)

#Converting the data into a time series object
pythondatats <- ts(pythondata$Python,start=c(2009, 1),end = c(2024, 2),frequency=12)
pythondatats

#Checking the class after conversion 
class(pythondatats)

#Initial Plot of the Time Series dataset
plot(pythondatats,type='o',xlab="Year",ylab="Total Questions", main='Time series plot of Total Questions in Python')

#Summary of the dataset, to check min and max values
summary(pythondatats)

par(mfrow=c(1,2))
acf(pythondatats,lag.max = 60,main ="ACF plot of total Questions in Python series")
pacf(pythondatats, main ="PACF plot of total Questions in Python series",lag.max = 60)
par(mfrow=c(1,1))

#ADF Test to check the nature of the data
adf.test(pythondatats)

#To check normality, we plot the qqplot and qqline
qqnorm(pythondatats, ylab="Total Questions", xlab="Normal Scores")
qqline(pythondatats)

#Wilk-Shapiro Test to check normality
shapiro.test(pythondatats) 

y = pythondatats 
x = zlag(pythondatats) 
index = 2:length(x)
cor(y[index],x[index])

#scatter plot to see the correlation
plot(y=pythondatats,x=zlag(pythondatats),ylab='Total Questions', xlab='Previous Year Total Questions', main = "Scatter plot of total questions in Python in consequtive months")

#Box-Cox Transformation, adding the constant variance since we have negative and zero values
BC = BoxCox.ar(pythondatats)
BC$ci
lambda <- BC$lambda[which(max(BC$loglike) == BC$loglike)]
lambda

bcxt = log(pythondatats)

#Plotting the time series transformed data
par(mfrow=c(1,1))
plot(bcxt,type='o', ylab ="Total Questions", main="Time series plot of Box Cox Transformed Data", xlab = "Year")

#Checking the normality of the transformed data
qqnorm(y=bcxt, main = "QQ plot of the transformed dataset")
qqline(y=bcxt, col = 2, lwd = 1, lty = 2)

#Wilk-Shaipro Test on the transformed data
shapiro.test(bcxt)

##ADF Test on the transformed data
adf.test(bcxt) 

diff_pythondata = diff(pythondatats)

par(mfrow=c(1,1))
plot(diff_pythondata,type='o',ylab='Total Questions', main = "Time series plot of the first differenced list of questions", xlab = "Year")

#First Differenceing ADF test
adf.test(diff_pythondata)

#First Differenceing PP test
pp.test(diff_pythondata)

#Wilk-Shaipro Test on the transformed data
shapiro.test(diff_pythondata)

#ACF-PACF of the First Differenceing time series data
par(mfrow=c(1,2))
acf(diff_pythondata, main='ACF plot of first differenced
    questions series in Python')
pacf(diff_pythondata, main='PACF plot of first differenced
    questions series in Python')
par(mfrow=c(1,1))
#Possible ARIMA Model: {ARIMA(1,1,1), ARIMA(1,1,2), ARIMA(2,1,1), ARIMA(2,1,2), ARIMA(3,1,1), ARIMA(3,1,2)}

eacf(diff_pythondata)
#Possible ARIMA Model: {ARIMA(0,1,1), ARIMA(1,1,1), ARIMA(0,1,2), ARIMA(1,1,2)}

#BIC Plot of the First Differenceing time series data
par(mfrow=c(1,1))
restempano = armasubsets(y=diff_pythondata,nar=5,nma=5
                         ,y.name='pOrder',ar.method='ols')
plot(restempano)
#Possible ARIMA Model: {ARIMA(1,1,1), ARIMA(2,1,1)}

# ARIMA(0,1,1)
pydata_011_css = Arima(pythondatats,order=c(0,1,1),method='CSS')
coeftest(pydata_011_css)

pydata_011_ml = Arima(pythondatats,order=c(0,1,1),method='ML')
coeftest(pydata_011_ml)

# ARIMA(1,1,1)
pydata_111_css = Arima(pythondatats,order=c(1,1,1),method='CSS')
coeftest(pydata_111_css)

pydata_111_ml = Arima(pythondatats,order=c(1,1,1),method='ML')
coeftest(pydata_111_ml)

# ARIMA(0,1,2)
pydata_012_css = Arima(pythondatats,order=c(0,1,2),method='CSS')
coeftest(pydata_012_css)

pydata_012_ml = Arima(pythondatats,order=c(0,1,2),method='ML')
coeftest(pydata_012_ml)

# ARIMA(1,1,2)
pydata_112_css = Arima(pythondatats,order=c(1,1,2),method='CSS')
coeftest(pydata_112_css)

pydata_112_ml = Arima(pythondatats,order=c(1,1,2),method='ML')
coeftest(pydata_112_ml)

# ARIMA(2,1,1)
pydata_211_css = Arima(pythondatats,order=c(2,1,1),method='CSS')
coeftest(pydata_211_css)

pydata_211_ml = Arima(pythondatats,order=c(2,1,1),method='ML')
coeftest(pydata_211_ml)

# ARIMA(2,1,2)
pydata_212_css = Arima(pythondatats,order=c(2,1,2),method='CSS')
coeftest(pydata_212_css)

pydata_212_ml = Arima(pythondatats,order=c(2,1,2),method='ML')
coeftest(pydata_212_ml)

# ARIMA(3,1,1)
pydata_311_css = Arima(pythondatats,order=c(3,1,1),method='CSS')
coeftest(pydata_311_css)

pydata_311_ml = Arima(pythondatats,order=c(3,1,1),method='ML')
coeftest(pydata_311_ml)

# ARIMA(3,1,2)
pydata_312_css = Arima(pythondatats,order=c(3,1,2),method='CSS')
coeftest(pydata_312_css)

pydata_312_ml = Arima(pythondatats,order=c(3,1,2),method='ML')
coeftest(pydata_312_ml)

#Function to sort the AIC BIC value with the lowerst value at the top
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

#Sorting & calculating the AIC Values
sort.score(AIC(pydata_011_ml, pydata_111_ml, pydata_012_ml, pydata_112_ml, pydata_211_ml, pydata_212_ml, pydata_311_ml,pydata_312_ml), score = "aic")

#Sorting & calculating the BIC Values
sort.score(BIC(pydata_011_ml, pydata_111_ml, pydata_012_ml, pydata_112_ml, pydata_211_ml, pydata_212_ml, pydata_311_ml,pydata_312_ml), score = "bic" )

#Calculating the accuracy for the fisrt seven mertics
Spydata_011_css <- accuracy(pydata_011_css)[1:7]
Spydata_111_css <- accuracy(pydata_111_css)[1:7]
Spydata_012_css <- accuracy(pydata_012_css)[1:7]
Spydata_112_css <- accuracy(pydata_112_css)[1:7]
Spydata_212_css <- accuracy(pydata_212_css)[1:7]
Spydata_211_css <- accuracy(pydata_211_css)[1:7]
Spydata_311_css <- accuracy(pydata_311_css)[1:7]
Spydata_312_css <- accuracy(pydata_312_css)[1:7]

#creating a dataframe by merging the above results
df.Spydata <- data.frame(
  rbind(Spydata_011_css,Spydata_111_css,Spydata_012_css,
        Spydata_112_css,Spydata_212_css,Spydata_211_css,Spydata_311_css,Spydata_312_css)
)

#Columns as the Evulation Metric
colnames(df.Spydata) <- c("ME", "RMSE", "MAE", "MPE", "MAPE", 
                          "MASE", "ACF1")

#Rows as the ARIMA Models
rownames(df.Spydata) <- c("ARIMA(0,1,1)", "ARIMA(1,1,1)", "ARIMA(0,1,2)", 
                          "ARIMA(1,1,2)", "ARIMA(2,1,2)", "ARIMA(2,1,1)","ARIMA(3,1,1)","ARIMA(3,1,2)")

#Rounding off all the values to three decimals
round(df.Spydata,  digits = 3)


# ARIMA(2,1,3)
pydata_213_css = Arima(pythondatats,order=c(2,1,3),method='CSS')
coeftest(pydata_213_css)

pydata_213_ml = Arima(pythondatats,order=c(2,1,3),method='ML')
coeftest(pydata_213_ml)

#Sorting & calculating the AIC Values including the extra model
sort.score(AIC(pydata_011_ml, pydata_111_ml, pydata_012_ml, pydata_112_ml, pydata_211_ml, pydata_212_ml, pydata_311_ml,pydata_312_ml,pydata_213_ml), score = "aic")

#Sorting & calculating the BIC Values including the extra model
sort.score(BIC(pydata_011_ml, pydata_111_ml, pydata_012_ml, pydata_112_ml, pydata_211_ml, pydata_212_ml, pydata_311_ml,pydata_312_ml,pydata_213_ml), score = "bic" )

#Residual Analysis Function
residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH", "fGARCH")[1]){
  library(TSA)
  library(FitAR)
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else if (class == "fGARCH"){
    res.model = model@residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals for Python")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals for Python")
  qqnorm(res.model,main="QQ plot of standardised residuals for Python")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals for Python")
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  par(mfrow=c(1,1))
}

#Residual Analysis on the Final Selected Models
residual.analysis(model = pydata_212_ml)
residual.analysis(model = pydata_213_ml)

#Forecasting for the next 10 months for the final selected model
frc = forecast::forecast(pydata_213_css,h=10) 
plot(frc,main ="Forecast with ARIMA(2,1,3) for the total Questions in Python in next 10 months")
lines(stats::lag(fitted(pydata_213_css),-1), col= "blue")
legend("bottomleft", lty=1, pch=1, col=c("blue","black"), text.width = 11, c("Data", "Fitted "))
frc





# For R:
rm(list=ls()) 
library(TSA)
library(fUnitRoots)
library(lmtest)
library(tseries)
library(forecast)
library(FitAR)

sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH", "fGARCH")[1]){
  library(TSA)
  library(FitAR)
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else if (class == "fGARCH"){
    res.model = model@residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  par(mfrow=c(1,1))
}

path <- "C:/DataScience/Sem4/TimeSeries/Assignment3/"
setwd(path)
assign3 <- read.csv("TotalQuestions.csv", header=TRUE)$R
class(assign3) 
assign3 <- tail(assign3,-3)
assign3

r.data = ts(assign3,start=c(2009,1),end=c(2024,2),frequency=12) 
r.data
class(r.data)
summary(r.data)


plot(r.data,type='o',ylab='Total Questions', main = "Time series plot of Total Questions in R .")

### creating zlag for getting the correlation 

y = r.data # Read the weekly returns series into y
x = zlag(r.data) # Generate first lag of the returns series
head(y)

head(x)

index = 2:length(x) # Create an index to get rid of the first NA value in x
cor(y[index],x[index])

x=zlag(zlag(r.data))
index= 3:length(x)
cor(y[index],x[index])

plot(y[index],x[index],ylab='Questions series', xlab='Previous month count', main = "Scatter plot of total questions in R for consecutive months ")


par(mfrow=c(1,2))
acf(r.data,lag.max = 60,main ="ACF plot of total Questions in R")
pacf(r.data, main ="PACF plot of total Questions in R",lag.max = 60)
par(mfrow=c(1,1))

#perform adf test to check the data stationary 

adf.test(r.data)

qqnorm(r.data,main ="QQ plot to check the normality on the Total Questions in R series")
qqline(r.data, col = 2)
shapiro.test(r.data)   # no normality as it is less that 0.05 

##apply boxcox transformation 

BC = BoxCox.ar(r.data)

BC$ci
lambda <- BC$lambda[which(max(BC$loglike) == BC$loglike)]
lambda

# lambda is >1 so its implies no transformation 
BC.r.data = (r.data^lambda-1)/lambda


plot(BC.r.data,type='o',ylab='Total Questions ', main = "Time series plot of BC transformed total questions  
     in R series.")

adf.test(BC.r.data)
qqnorm(BC.r.data, main = "QQ plot to check the normality on the BC transformed total Questions in R series ")
qqline(BC.r.data, col = 2)
shapiro.test(BC.r.data)

par(mfrow=c(1,2))
acf(BC.r.data,lag.max = 60,main ="ACF plot of BC transformed total Questions in R series.")
pacf(BC.r.data, main ="PACF plot of BC transformed total Questions in R series.",lag.max = 60)
par(mfrow=c(1,1))

# apply first level- differentiation 
r.data
diff.r.data = diff(r.data)
plot(diff.r.data,type='o',ylab='Total Questions',main=" Time series plot of the first differenced total Questions in  R")

adf.test(diff.r.data)

pp.test(diff.r.data)



par(mfrow=c(1,2))
acf(diff.r.data, main ="a. ACF plot of the first differenced
    total R questions series.")
pacf(diff.r.data, main ="b. PACF of the first differenced 
     total R questions series.")
par(mfrow=c(1,1))

qqnorm(diff.r.data,main="Fig 10.QQ plot to check the normality on the first differenced questions series")
qqline(diff.r.data, col = 2)
shapiro.test(diff.r.data)


# Set of possible models from ACF and PACF : 
#p = 1,2,3
#q= 1,2,3,4

#set of possible models from ACF/PACF :{ARIMA(1,1,1),ARIMA(1,1,2),ARIMA(1,1,3),ARIMA(1,1,4),
                                          #ARIMA(2,1,1),ARIMA(2,1,2),ARIMA(2,1,3),ARIMA(2,1,4),ARIMA(3,1,1),ARIMA(3,1,2),ARIMA(3,1,3),ARIMA(3,1,4)}

eacf(diff.r.data , ar.max=10 , ma.max=10)
# Set of possible models from EACF : {ARIMA(0,1,1), ARIMA(0,1,2),ARIMA(1,1,1),ARIMA(1,1,2)}

# getting the BIC tables 
res3 = armasubsets(y=diff.r.data,nar=5,nma=5
                   ,y.name='pOrder',ar.method='ols')
plot(res3)#,title="Fig 12 BIC table for model specification on the fist difference series")
# Set of possible models from BIC : {ARIMA(2,1,2) ARIMA(3,1,2)}


# set of all possible models = {ARIMA(0,1,1), ARIMA(0,1,2),ARIMA(1,1,1), ARIMA(1,1,2),ARIMA(1,1,3), ARIMA(1,1,4),ARIMA(2,1,1),ARIMA(2,1,2),ARIMA(2,1,3),ARIMA(2,1,4),ARIMA(3,1,1),ARIMA(3,1,2),ARIMA(3,1,3),ARIMA(3,1,4)}

# ARIMA(0,1,1)
model_011_css = Arima(r.data,order=c(0,1,1),method='CSS')
coeftest(model_011_css)

model_011_ml = Arima(r.data,order=c(0,1,1),method='ML')
coeftest(model_011_ml)

# ARIMA(0,1,2)
model_012_css = Arima(r.data,order=c(0,1,2),method='CSS')
coeftest(model_012_css)

model_012_ml = Arima(r.data,order=c(0,1,2),method='ML')
coeftest(model_012_ml)

# ARIMA(1,1,1)
model_111_css = Arima(r.data,order=c(1,1,1),method='CSS')
coeftest(model_111_css)

model_111_ml = Arima(r.data,order=c(1,1,1),method='ML')
coeftest(model_111_ml)

# ARIMA(1,1,2)
model_112_css = Arima(r.data,order=c(1,1,2),method='CSS')
lmtest::coeftest(model_112_css)

model_112_ml = Arima(r.data,order=c(1,1,2),method='ML')
coeftest(model_112_ml)

# ARIMA(1,1,3)
model_113_css = Arima(r.data,order=c(1,1,3),method='CSS')
coeftest(model_113_css)

model_113_ml = Arima(r.data,order=c(1,1,3),method='ML')
coeftest(model_113_ml)

# ARIMA(1,1,4)
model_114_css = Arima(r.data,order=c(1,1,4),method='CSS')
coeftest(model_114_css)

model_114_ml = Arima(r.data,order=c(1,1,4),method='ML')
coeftest(model_114_ml)


# ARIMA(2,1,1)
model_211_css = Arima(r.data,order=c(2,1,1),method='CSS')
coeftest(model_211_css)

model_211_ml = Arima(r.data,order=c(2,1,1),method='ML')
coeftest(model_211_ml)

# ARIMA(2,1,2)
model_212_css = Arima(r.data,order=c(2,1,2),method='CSS')
coeftest(model_212_css)

model_212_ml = Arima(r.data,order=c(2,1,2),method='ML')
coeftest(model_212_ml)

# ARIMA(2,1,3)
model_213_css = Arima(r.data,order=c(2,1,3),method='CSS')
coeftest(model_213_css)

model_213_ml = Arima(r.data,order=c(2,1,3),method='ML')
coeftest(model_213_ml)

# ARIMA(2,1,4)
model_214_css = Arima(r.data,order=c(2,1,4),method='CSS')
coeftest(model_214_css)

model_214_ml = Arima(r.data,order=c(2,1,4),method='ML')
coeftest(model_214_ml)

model_214_ml = Arima(r.data,order=c(2,1,4),method='CSS-ML')
coeftest(model_214_ml)

# ARIMA(3,1,1)
model_311_css = Arima(r.data,order=c(3,1,1),method='CSS')
coeftest(model_311_css)

model_311_ml = Arima(r.data,order=c(3,1,1),method='ML')
coeftest(model_311_ml)

# ARIMA(3,1,2)
model_312_css = Arima(r.data,order=c(3,1,2),method='CSS')
coeftest(model_312_css)

model_312_ml = Arima(r.data,order=c(3,1,2),method='ML')
coeftest(model_312_ml)

# ARIMA(3,1,3)
model_313_css = Arima(r.data,order=c(3,1,3),method='CSS')
coeftest(model_313_css)

model_313_ml = Arima(r.data,order=c(3,1,3),method='ML')
coeftest(model_313_ml)

# ARIMA(3,1,4)
model_314_css = Arima(r.data,order=c(3,1,4),method='CSS')
coeftest(model_314_css)

model_314_ml = Arima(r.data,order=c(3,1,4),method='ML')
coeftest(model_314_ml)

model_314_ml = Arima(r.data,order=c(3,1,4),method='CSS-ML')
coeftest(model_314_ml)

model_215_ml = Arima(r.data,order=c(2,1,5),method='ML')
coeftest(model_215_ml)

# AIC and BIC values

sort.score(AIC(model_011_ml,model_012_ml,model_111_ml,model_112_ml,model_113_ml,model_114_ml,model_211_ml,model_212_ml,model_213_ml,model_214_ml,model_311_ml,model_312_ml,model_313_ml,model_314_ml), score = "aic")
sort.score(BIC(model_011_ml,model_012_ml,model_111_ml,model_112_ml,model_113_ml,model_114_ml,model_211_ml,model_212_ml,model_213_ml,model_214_ml,model_311_ml,model_312_ml,model_313_ml,model_314_ml), score = "bic" )

# AIC model - ARIMA(2,1,3)
#BIC model - ARIMA(2,1,3)

Smodel_011_css <- accuracy(model_011_css)[1:7]
Smodel_012_css <- accuracy(model_012_css)[1:7]
Smodel_111_css <- accuracy(model_111_css)[1:7]
Smodel_112_css<- accuracy(model_112_css)[1:7]
Smodel_113_css <- accuracy(model_113_css)[1:7]
Smodel_114_css <- accuracy(model_114_css)[1:7]
Smodel_211_css <- accuracy(model_211_css)[1:7]
Smodel_212_css <- accuracy(model_212_css)[1:7]
Smodel_213_css <- accuracy(model_213_css)[1:7]
Smodel_214_css <- accuracy(model_214_css)[1:7]
Smodel_311_css <- accuracy(model_311_css)[1:7]
Smodel_312_css <- accuracy(model_312_css)[1:7]
Smodel_313_css <- accuracy(model_313_css)[1:7]
Smodel_314_css <- accuracy(model_314_css)[1:7]
df.Smodels <- data.frame(
  rbind(Smodel_011_css,Smodel_012_css,
        Smodel_111_css,Smodel_112_css,Smodel_113_css,Smodel_114_css,
        Smodel_211_css,Smodel_212_css,Smodel_213_css,Smodel_214_css,
        Smodel_311_css,Smodel_312_css,Smodel_313_css,Smodel_314_css)
)
colnames(df.Smodels) <- c("ME", "RMSE", "MAE", "MPE", "MAPE", 
                          "MASE", "ACF1")
rownames(df.Smodels) <- c("ARIMA(0,1,1)", "ARIMA(0,1,2)","ARIMA(1,1,1)", "ARIMA(1,1,2)", "ARIMA(1,1,3)","ARIMA(1,1,4)",
                          "ARIMA(2,1,1)","ARIMA(2,1,2)","ARIMA(2,1,3)","ARIMA(2,1,4)",
                          "ARIMA(3,1,1)","ARIMA(3,1,2)","ARIMA(3,1,3)","ARIMA(3,1,4)")

round(df.Smodels,  digits = 3)


#AIC and BIC supports ARIMA(2,1,3).Error measures support ARIMA(3,1,3) 

# Overparameterised models for ARIMA(2,1,3) are ARIMA(3,1,3) and ARIMA(2,1,4) and is already calculated 


residual.analysis(model = model_213_css)
residual.analysis(model = model_313_css)
par(mfrow=c(1,1))


frc = forecast::forecast(model_213_css,h=10) 
plot(frc,main ="Forecast with ARIMA(2,1,3) for the total Questions in R in next 10 months")
lines(lag(fitted(model_213_css),-1), col= "blue")
legend("bottomleft", lty=1, pch=1, col=c("blue","black"), text.width = 11, c("Data", "Fitted "))
frc
