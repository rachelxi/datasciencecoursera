data<-read.csv("activity.csv")
#Q1What is mean total number of steps taken per day?
Q1<-data[complete.cases(data),]
StepsPerDay<-tapply(Q1$steps,Q1$date,sum)
windows()
hist(StepsPerDay)
dev.off()
mean(StepsPerDay,na.rm=TRUE)
median(StepsPerDay,na.rm=TRUE)
#Q2What is the average daily activity pattern?
Q2<-data[complete.cases(data),]
StepsPerInterval<-aggregate(Q2$steps,list(Q2$interval),mean)
colnames(StepsPerInterval)<-c("Interval","stepsPerInterval")
windows()
plot(StepsPerInterval,type="l")
title("Average Steps Per Interval")
dev.off()
Index<-which.max(StepsPerInterval[,2])
StepsPerInterval[Index,1]
#Q3Imputing missing values
sum(is.na(data))
#Strategy: replace the NAs with mean steps of that day
Q3<-data
Replace<-tapply(Q1$steps,Q1$date,mean)
Replace[is.na(Replace)]<-0
Dates<-as.character(data$date)
NAindex<-which(is.na(data))
n<-length(NAindex)
for(i in 1:n){
  index<-NAindex[i]
  inter<-grep(Dates[index],names(Replace))
  Q3$steps[index]<-Replace[inter]
}
StepsPerDay2<-tapply(Q3$steps,Q3$date,sum)
windows()
hist(StepsPerDay2)
dev.off()
mean(StepsPerDay2,na.rm=TRUE)
median(StepsPerDay2,na.rm=TRUE)
#Q4Are there differences in activity patterns between weekdays and weekends?
Sys.setlocale("LC_TIME", "English")
Indicator<-(weekdays(as.Date(Q3$date)))
n<-length(Indicator)
weekday<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
for(i in 1:n){
  inter2<-grepl(Indicator[i],weekday)
  if(sum(inter2)==1){Indicator[i]<-"weekday"} 
  if(sum(inter2)==0){Indicator[i]<-"weekend"}
}
Q4<-cbind(Q3,Indicator)
part<-split(Q4,Q4$Indicator)
wkSteps<-part[[1]]
wkeSteps<-part[[2]]
wkAvg<-aggregate(wkSteps$steps,list(wkSteps$interval),mean)
wkeAvg<-aggregate(wkeSteps$steps,list(wkeSteps$interval),mean)
wkAvg<-cbind(wkAvg,rep("weekday",288))
colnames(wkAvg)<-c("Interval","Steps","Indicator")
wkeAvg<-cbind(wkeAvg,rep("weekend",288))
colnames(wkeAvg)<-c("Interval","Steps","Indicator")
Q4<-rbind(wkAvg,wkeAvg)
windows()
p<-ggplot(Q4,aes(x=Interval,y=Steps))+theme_bw()
p+geom_line()+facet_grid(. ~ Indicator)
dev.off()