library(ggplot2)
library(dplyr)

##Read and prep data
#setwd("U:/COURSERA/knitr")
act<-read.csv("activity.csv")
act$date<- as.Date(act$date)

##Calculate mean and median and sum

act2<-act %>%
  group_by(date)%>%
  mutate(sum=sum(steps))%>%
  mutate(mean=mean(steps))%>%
  mutate(med=median(steps))%>%
  ungroup()%>%

act3<- act2[c(2,4:6) ]
act3<- act3[!duplicated(act3),]

##Plot total steps per day
ggplot(data=act3,aes(x=act3$sum))+
  geom_histogram()+
  labs(title="Total number of steps per day",y="Frequency",x="Steps")

##5 min interval average

int<-aggregate(steps ~ interval,FUN=mean,data=act)

ggplot(int,aes(x=int$interval,y=int$steps))+
  geom_line()+
  labs(title="Average steps per 5 minute interval",x="5 minute interval",y="Average number of steps")

int$interval[max(int$steps)]

##Number of NAs

length(act$steps[is.na(act$steps)])

##Replace NAs

act4<-act

act4$steps[is.na(act4$steps)]<- median(act4$steps,na.rm = T)

act5<- aggregate(steps ~ date,data=act4,FUN = sum,na.rm=T)

##Plot without NAs
ggplot(act5,aes(x=act5$steps))+
  geom_histogram(fill="blue")+
  labs(title="Total number of steps per day",y="Frequency",x="Steps")


##Mean without NA
mean(act4$steps, na.rm=T)
median(act4$steps,na.rm = T)

##Mean with NA
mean(act$steps,na.rm = T)
median(act$steps,na.rm=T)

##When is a weekday

act$weekday<-weekdays(act$date)
act$weekday <-ifelse(act$weekday == "Saturday"|act$weekday ==  "Sunday","Weekend","Weekday")

int2<-aggregate(steps ~ interval+weekday,FUN=mean,data=act)

##Plot average steps weekday vs weekend
ggplot(int2,aes(x=int2$interval,y=int2$steps))+
  geom_line()+
  facet_grid(weekday~.)+
  labs(title="Average steps per 5 minute interval",x="5 minute interval",y="Average number of steps")

