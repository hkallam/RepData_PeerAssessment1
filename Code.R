setwd("/Users/himabindu/repdata_peerassg1")
library(knitr)
library(ggplot2)
library(dplyr)
unzip("activity.zip")
filename<-read.csv("activity.csv",header = TRUE,sep=",") ## reading the file
stepsperday<-aggregate(steps~date,filename,FUN = sum)

ggplot(stepsperday,aes(steps)) +geom_histogram(fill=NA, color="blue") + labs(title="Total number of steps per day")        +labs(x= steps)
mean(stepsperday$steps) ## calculating the mean
median(stepsperday$steps)
stepsperinterval<-aggregate(steps~interval,filename,FUN=mean,na.rm = TRUE)
names(stepsperinterval)[2]<-"averagesteps"
plot(stepsperinterval$interval,stepsperinterval$averagesteps,type="l",xlab="5 min interval",ylab ="Average number of steps",main="Average Daily Activity in a 5 min interval")
maxinterval<-stepsperinterval[which.max(stepsperinterval$averagesteps),] ## finding the maximum interval
sum(is.na(filename)) ## finding the numv=ber of NA's
data<-merge(filename,stepsperinterval,by.x="interval") ## merging the original file and average steps perinterval file
data<-data[(order(data$date,data$interval)),]  ## ordering it by date and interval
rownames(data)<-NULL   
misscount<-0
for(i in 1:nrow(data))
{
  if(is.na(data$steps[i])) {
    data$steps[i]<-data$averagesteps[i]
    misscount<-misscount+1
  }
}
newdata<-select(data,steps,date,interval) ## selecting the required number of columns
new_steps_per_day<-aggregate(steps~date,newdata,FUN = sum)
mean(new_steps_per_day$steps)
median(new_steps_per_day$steps)
ggplot(new_steps_per_day,aes(steps)) +geom_histogram(fill=NA, color="red") + labs(title="Total number of steps per day")
