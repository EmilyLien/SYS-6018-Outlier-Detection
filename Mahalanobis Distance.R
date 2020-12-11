#We will start with loading the appropriate libraries
library(tidyverse)
library(ROCR)

#Next, we load the data. The data for this demonstration can be downloaded here: https://www.kaggle.com/mlg-ulb/creditcardfraud
Transactions<-read.csv('creditcard.csv', header=TRUE, sep=',')

#Removing the column that indicates whether a transaction is fraudulent or not
Data<- Transactions %>% select(-Class)

#Data Summary
summary(Data)

#Data visualizations
plot(Data$Time, Data$Amount)
plot(Data$V1, Data$Amount)
plot(Data$V2, Data$Amount)
plot(Data$V3, Data$Amount)
plot(Data$V4, Data$Amount)
plot(Data$V5, Data$Amount)
plot(Data$V6, Data$Amount)
plot(Data$V7, Data$Amount)
plot(Data$V8, Data$Amount)
plot(Data$V9, Data$Amount)
plot(Data$V10, Data$Amount)
plot(Data$V11, Data$Amount)
plot(Data$V12, Data$Amount)
plot(Data$V13, Data$Amount)
plot(Data$V14, Data$Amount)
plot(Data$V15, Data$Amount)
plot(Data$V16, Data$Amount)
plot(Data$V17, Data$Amount)
plot(Data$V18, Data$Amount)
plot(Data$V19, Data$Amount)
plot(Data$V20, Data$Amount)
plot(Data$V21, Data$Amount)
plot(Data$V22, Data$Amount)
plot(Data$V23, Data$Amount)
plot(Data$V24, Data$Amount)
plot(Data$V25, Data$Amount)
plot(Data$V26, Data$Amount)
plot(Data$V27, Data$Amount)
plot(Data$V28, Data$Amount)

#We're going to use the mahalanobis function from the stats package. As per the formula, we need the covariance matrix, and the x-bar for each column
Xbar <- colMeans(Data)
Cov<- cov(Data)
MD<-mahalanobis(Data, Xbar, Cov)

#Plotting Mahalanobis distances
Distance<-tibble('MD' = MD)
ggplot(Distance, aes(x=MD)) + geom_density()

#We have the squared distances, now we need to determine the cutoff value to assess the outliers. One method is to calculate the Chi-Squared value of the data at a specified quantile, as shown in: https://towardsdatascience.com/mahalonobis-distance-and-outlier-detection-in-r-cb9c37576d7d
Cut <- qchisq(.975,ncol(Data))

#Now we assess how many outliers there are
sum(MD > Cut)

#There are 20981 outliers according to this cutoff value. This is more than the actual number of fraudulent cases. But does this number include all the cases?
MDclass<-0
for (i in(1:284807)){
  if (MD[i] > Cut){
    MDclass[i] <- 1
  } else MDclass[i] <- 0
}

Match <- NA
for (i in(1:284807)){
if (MDclass[i] == 1){
  if (Transactions$Class[i]==1){
    Match<-append(Match, i)
  }
    
  }
}

#Remove starter NA value
Match<-na.omit(Match)
length(Match)

#It correctly identified 436 fraudulent cases. However, that does not necessarily mean that it is a reliable method for identifying fraud!
#https://www.r-bloggers.com/2020/01/area-under-the-precision-recall-curve/

rates<-prediction(MDclass, Transactions$Class)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for identifying fraud",col="green")
lines(x = c(0,1), y = c(0,1), col="purple")

rates<-prediction(MDclass, Transactions$Class)
roc_result<-performance(rates,measure="prec", x.measure="rec")
plot(roc_result, main="Precision-Recall Curve for identifying fraud",col="green")
lines(x = c(0,1), y = c(0,1), col="purple")
