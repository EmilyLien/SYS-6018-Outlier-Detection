#We will start with loading the appropriate libraries
library(tidyverse)

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

#We're going to use the mahalanobis function from the stats package. As per the formula, we need the covariance matrix, and the x-bar for each column
Xbar <- colMeans(Data)
Cov<- cov(Data)
MD<-mahalanobis(Data, xbar, Cov)

#Plotting Mahalanobis distances
Distance<-tibble('MD' = MD)
barplot(MD)
ggplot(Distance, aes(x=MD)) + geom_density()

#We have the squared distances, now we need to determine the cutoff value to assess the outliers. One method is to calculate the Chi-Squared value of the data at a specified quantile https://towardsdatascience.com/mahalonobis-distance-and-outlier-detection-in-r-cb9c37576d7d
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
library(ROCR)
rates<-prediction(MDclass, Transactions$Class)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for identifying fraud",col="green")
lines(x = c(0,1), y = c(0,1), col="purple")

rates<-prediction(MDclass, Transactions$Class)
roc_result<-performance(rates,measure="prec", x.measure="rec")
plot(roc_result, main="Precision-Recall Curve for identifying fraud",col="green")
lines(x = c(0,1), y = c(0,1), col="purple")
