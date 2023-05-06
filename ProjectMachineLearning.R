setwd("C:/Users/honey/Desktop/Data Visualization with ggplot2")
install.packages("ggplot2")
install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(tidyverse) # metapackage with lots of helpful functions
library(data.table)


#Visualization
load(file="C:/Users/honey/Desktop/Data Visualization with ggplot2/WeatherAustralia2020.RData")
aw <- WeatherAustralia#deleting the date&location
View(aw)



#setting factors
aw$RainToday = factor(aw$RainToday,levels = c('No', 'Yes'),labels = c(0, 1))
View(aw)#converting yes/no to 0/1
aw$RainTomorrow = factor(aw$RainTomorrow,levels = c('No', 'Yes'),labels = c(0, 1))
View(aw)








#What is the rainfall Today rate in Australia?
#representing the rate of rainToday and Rate of RainTomorrow,as it is a factor(a categorical variable) bar graph is the best
ggplot(aw,aes(x=RainToday))+geom_bar()
ggplot(aw,aes(x=RainTomorrow))+geom_bar()

#If I want the data of RainToday and RainTomorrow  in numbers as a probability measure
prop.table(table(aw$RainToday))
prop.table(table(aw$RainTomorrow))

#2q.What is the RainTomorrow rate with respect to Cloud3pm?

ggplot(aw,aes(x=Cloud3pm,fill=RainTomorrow))+
  theme_bw()+
  geom_bar()+
  labs(y="Raintommorrow count",title="RainTomorrow rate by Cloud3pm")
#3q.What is the RainTomorrow rate with respect to Humidity3pm?

ggplot(aw,aes(x=RainTomorrow,y=Humidity3pm))+
  theme_bw()+
  geom_boxplot()+
  labs(y="Humidity3pm",x="RainTomorrow",title="RainTomorrow rate by Humidity3pm")

#RainTomorrow with respect to min and max temperature
  
  sp<-ggplot(aw,aes(Temp9am,Temp3pm))
  sp+geom_point(aes(color=RainTomorrow))+theme_light()+
    labs(x="Temp9am",y="Temp3pm",title = "RainTomorrow with respect to Temperature")
  
  #RainTomorrow with respect to Date and Temperature
  
  dt<-ggplot(aw,aes(Date,MinTemp))
  dt+geom_point(aes(color=RainTomorrow))+theme_light()+
  labs(x="Date",y="MinTemp",title = "RainTomorrow with respect to Maxtemperature")
  
  
 
###Got predictors 10 
#K-Nearest Neighbours knn
head(aw)
str(aw)
aw.subset<-subset(aw,select=-c(Date,Location,MinTemp,MaxTemp,Evaporation,WindSpeed9am,Humidity9am,Temp9am,Temp3pm,Cloud9am,Rainfall,RainToday,latitude,longitude))
View(aw.subset)


normalize<-function(x)
{
  return((x-min(x))/(max(x)-min(x)))
  
}

aw.subset.n<-as.data.frame(lapply(aw.subset[,1:7],normalize))
View(aw.subset.n)
set.seed(123)

dat.d<-sample(1:nrow(aw.subset.n),size=nrow(aw.subset.n)*0.7,replace = FALSE)
train.aw<-aw.subset[dat.d,]#70%
View(train.aw)
test.aw<-aw.subset[-dat.d,]#30%
View(test.aw)

train.aw_labels<-aw.subset[dat.d,8]
View(train.aw_labels)
test.aw_labels<-aw.subset[-dat.d,8]
View(test.aw_labels)
install.packages('class')
library(class)

NROW(train.aw_labels)
View(train.aw_labels)


knn.82<-knn(train=train.aw,test=test.aw,cl=train.aw_labels,k=82)
knn.83<-knn(train=train.aw,test=test.aw,cl=train.aw_labels,k=83)
 
#Calculating Accuracy
ACC.82<-100*sum(test.aw_labels==knn.82)/NROW(test.aw_labels)
ACC.83<-100*sum(test.aw_labels==knn.83)/NROW(test.aw_labels)
ACC.82#85.99727%
ACC.83#85.96311%

table(knn.82,test.aw_labels)#Checking accuracy in a table format
knn.82

table(knn.83,test.aw_labels)
knn.83
install.packages('caret')
library(caret)

#Confusion Matrix,Model Performance

confusionMatrix(table(knn.82,test.aw_labels))

i=1
k.optm=1
for(i in 1:84) 
{
   knn.mod<-knn(train = train.aw,test = test.aw,cl=train.aw_labels,k=i)
   k.optm[i]<-100*sum(test.aw_labels==knn.mod)/NROW(test.aw_labels)
   k=i
   cat(k,"=",k.optm[i],'\n')#to print accuracy
   
}
plot(k.optm,type="b",xlab = "K-value",ylab = "Accuracy level")
#according the plot k value for 21 gives more accurate value of K

#Logistic Regression
library(caTools)
model<-glm(RainTomorrow~ .,train.aw,family="binomial")
summary(model)

res<-predict(model,test.aw,type = "response")
res

#Confusion Matrix
table(AcualValue=test.aw$RainTomorrow,PredictiveValue=res>0.5)

#Calculate accuracy
((2181+342)/(2181+299+106+342))#Gives accuracy of 86.16803 for treshold 0.5 and checked with other tresholds which are not giving good results

#AOC Curve

library(ROCR)
res<-predict(model,train.aw,type="response")
ROCRPred=prediction(res,train.aw$RainTomorrow)
ROCRPref<-performance(ROCRPred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

##Deep Learning
library(keras)
library(neuralnet)
library(magrittr)
#Visualization of a neural network
n<-neuralnet(RainTomorrow ~ Sunshine+WindGustSpeed+Humidity3pm+Pressure9am+Pressure3pm+Cloud3pm,data = aw.subset,hidden = c(6,3),linear.output = F,lifesign = 'full',rep = 1)
plot(n,col.hidden='red',col.hidden.synapse='red',show.weights=F,information=F,fill='lightblue')

#creating the model
install_miniconda()
my_model<-keras_model_sequential() %>%
          layer_dense(unit=3,activation = "relu",input_shape = c(7)) %>%
          layer_dense(units=2,activation="sigmoid")