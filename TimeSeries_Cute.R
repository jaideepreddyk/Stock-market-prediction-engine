
dataCSV = read.csv("data11.csv")
length(dataCSV)
class(dataCSV)

################################## Time Series for Y1 varaible ##############################
pruneData = dataCSV[,c("timestamp","y1")]
summary(pruneData)
str(pruneData)

dateF = seq(as.Date("2014/1/14"), by = "day", length.out = 1769)
head(dateF, n=60)

pruneData = cbind(pruneData,Datetime = dateF)
pruneData$timestamp = NULL
head(pruneData)
str(pruneData)
tail(pruneData)
plot(pruneData$y1,type='l', main='log returns plot')

#Conduct ADF test on log returns series
#  Testing for stationarity - We test for stationarity using the 
# Augmented Dickey-Fuller unit root test. The p-value resulting from the ADF test has to be less 
# than 0.05 or 5% for a time series to be stationary. If the p-value is greater than 0.05 or 5%, you 
# conclude that the time series has a unit root which means that it is a non-stationary process.
library(tseries)
 print(adf.test(pruneData$y1))  # this case less than .01 or 1% that mean series is stationary.



Train = pruneData[1:1767,]
Test = pruneData[1767:1769,]
Train
Test
head(Test)
dim(Test)

TodayPrice = ts(Train$y1, frequency =30)
TodayPrice = ts(Train$y1)#, frequency =30)

class(TodayPrice)
str(TodayPrice)
head(TodayPrice)
#t = tbats(TodayPrice)



par(mfrow=c(1,2)) 
acf(TodayPrice,lag=20) 
pacf(TodayPrice,lag=20)

library(forecast)
library(stats)
MODEL_ARIMA <- auto.arima(Train$y1, ic='aic')
summary(MODEL_ARIMA)

forecastData = forecast(MODEL_ARIMA, h=30)
#table(forecastData[,"Forecast"])
summary(forecastData)
head(forecastData)

################################## Logistic Regression for Y2 varaible ##############################
mytrain<-read.csv("data11.csv")
mytest<-read.csv("test11.csv")

dim(mytrain)
dim(mytest)
mytrainsubset<-subset(mytrain,select = -c(y1,y2))

dim(mytrainsubset)

fulldata<-rbind(mytrainsubset,mytest)

sum(is.na(fulldata))

summary(fulldata)

library(DMwR)
fulldata_imputed<-centralImputation(fulldata)
train_imputed<-centralImputation(mytrainsubset) 
test_imputed<-centralImputation(mytest)

sum(is.na(train_imputed))
sum(is.na(test_imputed))
sum(is.na(fulldata_imputed))

library(vegan)
fulldata_stand_imputed<-decostand(fulldata_imputed,"standardize")
trStIm<-fulldata_stand_imputed[1:nrow(mytrainsubset),]

dim(trStIm)
teStIm<-fulldata_stand_imputed[1770:1799,]
dim(teStIm)

head(teStIm)

trStIm<-cbind(trStIm,y2=mytrain$y2)
summary(trStIm)
dim(trStIm)



d1<-trStIm
d2<-teStIm

###########USING PCA####################


targvar = as.factor(d1$y2)


d1$y2<-NULL
d2$timestamp = NULL
d1$timestamp = NULL
pca_train<-princomp(d1,cor = TRUE)
summary(pca_train)
predpcatest<-predict(pca_train,d2)
summary(predpcatest)
compressedtrain<-pca_train$scores[,1:5]

screeplot(pca_train,type = "lines")
dfcompressedtrain<-data.frame(compressedtrain)
compressedtrain<-cbind(dfcompressedtrain,y2=targvar)
class(compressedtrain)
head(compressedtrain)
summary(compressedtrain)

predlog<-glm(y2~.,data = data.frame(compressedtrain),family = "binomial")
summary(predlog)

pcatest<-data.frame(predpcatest)

finalpcapred<-predict(predlog,newdata = pcatest[,1:5],type = "response")
finalpcapred
summary(finalpcapred)
head(finalpcapred)


################################################### Linear MOdel for Y1 ##################################3

mytrain<-read.csv("data11.csv")
mytest<-read.csv("test11.csv")

dim(mytrain)
dim(mytest)
mytrainsubset<-subset(mytrain,select = -c(y1,y2))

dim(mytrainsubset)

fulldata<-rbind(mytrainsubset,mytest)

sum(is.na(fulldata))

summary(fulldata)

library(DMwR)
fulldata_imputed<-centralImputation(fulldata)
train_imputed<-centralImputation(mytrainsubset) 
test_imputed<-centralImputation(mytest)

sum(is.na(train_imputed))
sum(is.na(test_imputed))
sum(is.na(fulldata_imputed))

library(vegan)
fulldata_stand_imputed<-decostand(fulldata_imputed,"standardize")
trStIm<-fulldata_stand_imputed[1:nrow(mytrainsubset),]

dim(trStIm)
teStIm<-fulldata_stand_imputed[1770:1799,]
dim(teStIm)

head(teStIm)

trStIm<-cbind(trStIm,y1=mytrain$y1)
summary(trStIm)
dim(trStIm)



d1<-trStIm
d2<-teStIm

dim(d1)
dim(d2)
summary(d1)

model_lm<-lm(y1~.,data = d1)
summary(model_lm)



library(MASS)
stepAIC(model_lm,direction = "both")
model_lm_stepaic<-lm(formula = y1 ~ d_0 + d_1 + d_2 + d_4 + f_1 + f_2 + f_3 + f_5 + f_6 + f_7 + f_8 + f_15 + f_16 + f_22 + f_23 +  f_24 + f_25 + f_27 + f_28 + f_29 + f_31 + f_32 + f_33 + f_34 + f_37 + f_38 + f_39 + f_40 + f_43 + f_46 + f_49 + f_51 + f_52 +  f_54 + f_56 + f_59 + f_60 + f_63 + t_6 + t_9 + t_10 + t_12 + t_17 + t_20 + t_29 + t_30 + t_32 + t_35 + t_36 + t_39 + t_40+t_43 + t_44 , data = d1)
summary(model_lm_stepaic)
test_pred<-predict(model_lm_stepaic,d2)
head(test_pred)
test_pred




###############################PCA Linear Model on Y1################################

mytrain<-read.csv("data11.csv")
mytest<-read.csv("test11.csv")

dim(mytrain)
dim(mytest)
mytrainsubset<-subset(mytrain,select = -c(y1,y2))

dim(mytrainsubset)

fulldata<-rbind(mytrainsubset,mytest)

sum(is.na(fulldata))

summary(fulldata)

library(DMwR)
fulldata_imputed<-centralImputation(fulldata)
train_imputed<-centralImputation(mytrainsubset) 
test_imputed<-centralImputation(mytest)

sum(is.na(train_imputed))
sum(is.na(test_imputed))
sum(is.na(fulldata_imputed))

library(vegan)
fulldata_stand_imputed<-decostand(fulldata_imputed,"standardize")
trStIm<-fulldata_stand_imputed[1:nrow(mytrainsubset),]

dim(trStIm)
teStIm<-fulldata_stand_imputed[1770:1799,]
dim(teStIm)

head(teStIm)

trStIm<-cbind(trStIm,y1=mytrain$y1)
summary(trStIm)
dim(trStIm)



d1<-trStIm
d2<-teStIm

targvar =d1$y1


d1$y1<-NULL
d2$timestamp = NULL
d1$timestamp = NULL
pca_train<-princomp(d1,cor = TRUE)
summary(pca_train)
predpcatest<-predict(pca_train,d2)
summary(predpcatest)
compressedtrain<-pca_train$scores[,1:5]

screeplot(pca_train,type = "lines")
dfcompressedtrain<-data.frame(compressedtrain)
compressedtrain<-cbind(dfcompressedtrain,y1=targvar)
class(compressedtrain)
head(compressedtrain)
summary(compressedtrain)

model_pca_lm<-lm(y1~.,data = compressedtrain)
summary(model_pca_lm)

pcatest<-data.frame(predpcatest)

finalpcapred<-predict(model_pca_lm,newdata = pcatest[,1:5])
finalpcapred
summary(finalpcapred)
head(finalpcapred)

######################################## END ##################################



