library(ggplot2)
library(reshape2)
library(gridExtra)
library(MLmetrics)
library(neuralnet)
library(grid)
library(MASS)

#################### LOAD DATA ################
dataT1 <- read.csv("~/Desktop/BI/t-1.csv")
dataT2 <- read.csv("~/Desktop/BI/t-2.csv")
dataT3 <- read.csv("~/Desktop/BI/t-3.csv")

str(dataT1)
summary(dataT1)

str(dataT2)
summary(dataT2)

str(dataT3)
summary(dataT3)

boxplot(dataT1$T, plot=TRUE) ## Detecting if any outliers


# creating functions
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
  #-1 + 2.*(data - min(data))/(max(data) - min(data))
}



unnormalise <- function(x, min, max) { 
  return( (max - min)*x + min )
}

rmse <- function(error)
{
  sqrt(mean(error^2))
}

#NORMAILISEDalise data 
dataT1.NORMAILISED <- as.data.frame(lapply(dataT1, normalise))
dataT2.NORMAILISED <- as.data.frame(lapply(dataT2, normalise))
dataT3.NORMAILISED <- as.data.frame(lapply(dataT3, normalise))

summary(dataT1.NORMAILISED)
summary(dataT2.NORMAILISED)
summary(dataT3.NORMAILISED)

#Creating TRAINig datasets
dataT1.TRAIN <- dataT1[1:399, ]
dataT1.TEST <- dataT1[400:498, ]

dataT2.TRAIN <- dataT2[1:399, ]
dataT2.TEST <- dataT2[400:497, ]

dataT3.TRAIN <- dataT3[1:399, ]
dataT3.TEST <- dataT3[400:496, ]




######################    TRAINING    ##############################



set.seed(12345) # to guarantee repeatable results
######for t-1
dataT1.MODEL <- neuralnet(Tp1 ~ T + Tm1, hidden = c(3,4,2),  data = dataT1.TRAIN)
#plot(dataT1.MODEL)  

set.seed(12345) # to guarantee repeatable results # to guarantee repeatable results  
#####for t-2
dataT2.MODEL <- neuralnet(Tp1 ~ T + Tm1 + Tm2, hidden = c(3,4,2), data = dataT2.TRAIN)
#plot(dataT2.MODEL)  

set.seed(12345) # to guarantee repeatable results # to guarantee repeatable results
####for t-3
dataT3.MODEL <- neuralnet(Tp1 ~ T + Tm1 + Tm2 + Tm3, hidden = c(4), data = dataT3.TRAIN)
#plot(dataT3.MODEL)   




########################## EVALUATING PERFORMANCE ##############################

########## FOR T-1
#Number model result
MODEL1.results <- compute(dataT1.MODEL, dataT1.TEST[1:2])
#Obtaining predicted rate values
predicted.RATE1 <- MODEL1.results$net.result
cor(predicted.RATE1, dataT1.TEST$Tp1)
#head(predicted.RATE1)

#Retrieving original data
dataT1.TRAIN.original.RATE <- dataT1[1:399,3]  # the first 773 rows
dataT1.TEST.original.RATE <- dataT1[400:498,3] # the remaining rows



#unuormalise values
prediction1.UNNORMALISED <- predicted.RATE1
#prediction1.UNNORMALISED


final.result1 <- cbind(dataT1.TEST.original.RATE, prediction1.UNNORMALISED)
#final.result1

#####Indexex

#MRSE INDEX
error <- (dataT1.TEST.original.RATE - prediction1.UNNORMALISED)
pred1_RMSE <- rmse(error)  
pred1_RMSE     

#MPE INDEX
pred1_MAPE <- MAPE(prediction1.UNNORMALISED, dataT1.TEST.original.RATE)
pred1_MAPE

#MAE INDEX
pred1_MAE <- mae(prediction1.UNNORMALISED, dataT1.TEST.original.RATE)
pred1_MAE

#PLOT
par(mfrow=c(1,1))
plot(dataT1.TEST.original.RATE, prediction1.UNNORMALISED ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')




########## FOR T-2
#Number model result
MODEL2.results <- compute(dataT2.MODEL, dataT2.TEST[1:3])
#Obtaining predicted rate values
predicted.RATE2 <- MODEL2.results$net.result
cor(predicted.RATE2, dataT2.TEST$Tp1)
#head(predicted.RATE2)

#Retrieving original data
dataT2.TRAIN.original.RATE <- dataT2[1:399,4]  # the first 773 rows
dataT2.TEST.original.RATE <- dataT2[400:497,4] # the remaining rows

#Minimum and maximum
pred2.min <- min(dataT2.TRAIN.original.RATE)
pred2.max <- max(dataT2.TRAIN.original.RATE)
#head(dataT2.TRAIN.original.RATE)

#unuormalise values
prediction2.UNNORMALISED <- predicted.RATE2
#prediction2.UNNORMALISED


final.result2 <- cbind(dataT2.TEST.original.RATE, prediction2.UNNORMALISED)
head(final.result2)

#####Indexex

#MRSE INDEX
error <- (dataT2.TEST.original.RATE - prediction2.UNNORMALISED)
pred2_RMSE <- rmse(error)  
pred2_RMSE     

#MPE INDEX
pred2_MAPE <- MAPE(prediction2.UNNORMALISED, dataT2.TEST.original.RATE)
pred2_MAPE

#MAE INDEX
pred2_MAE <- mae(prediction2.UNNORMALISED, dataT2.TEST.original.RATE)
pred2_MAE

#PLOT
par(mfrow=c(1,1))
plot(dataT2.TEST.original.RATE, prediction2.UNNORMALISED ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')





########## FOR T-3
#Number model result
MODEL3.results <- compute(dataT3.MODEL, dataT3.TEST[1:4])
#Obtaining predicted rate values
predicted.RATE3 <- MODEL3.results$net.result
cor(predicted.RATE3, dataT3.TEST$Tp1)
#head(predicted.RATE3)

#Retrieving original data
dataT3.TRAIN.original.RATE <- dataT3[1:399,4]  # the first 773 rows
dataT3.TEST.original.RATE <- dataT3[400:496,4] # the remaining rows

#Minimum and maximum
pred3.min <- min(dataT3.TRAIN.original.RATE)
pred3.max <- max(dataT3.TRAIN.original.RATE)
#head(dataT3.TRAIN.original.RATE)

#unuormalise values
prediction3.UNNORMALISED <- predicted.RATE3
#prediction3.UNNORMALISED


final.result3 <- cbind(dataT3.TEST.original.RATE, prediction3.UNNORMALISED)
head(final.result3)

#####Indexes

#MRSE INDEX
error <- (dataT3.TEST.original.RATE - prediction3.UNNORMALISED)
pred3_RMSE <- rmse(error)  
pred3_RMSE     

#MPE INDEX
pred3_MAPE <- MAPE(prediction3.UNNORMALISED, dataT3.TEST.original.RATE)
pred3_MAPE

#MAE INDEX
pred3_MAE <- mae(prediction3.UNNORMALISED, dataT3.TEST.original.RATE)
pred3_MAE

#PLOT
par(mfrow=c(1,1))
plot(dataT3.TEST.original.RATE, prediction3.UNNORMALISED ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')


#OVERVIEW
pred1_RMSE 
pred1_MAPE
pred1_MAE

pred2_RMSE 
pred2_MAPE
pred2_MAE

pred3_RMSE 
pred3_MAPE
pred3_MAE

