###########  1

file1<-unzip('activity.zip')
data<-read.csv(file1)


###########  2

library(ggplot2)
sumbyday<-aggregate(steps~date,data,sum)
qplot(steps, data=sumbyday,bins=9,xlab='Steps',main='Total steps by day',
      ylab='Count')
mean(sumbyday$steps)
median(sumbyday$steps)

###########  3
meanbyint<-aggregate(steps~interval,data,mean)
plot(meanbyint$interval,meanbyint$steps,type='l',xlab='Interval',
     main='Average steps by interval',ylab='Average steps',col='blue')
meanbyint[which.max(meanbyint[,2]),1]

###########  4
sum(is.na(data))
data2<-data
indexes<-which(is.na(data$steps))
int<-data$interval[is.na(data$steps)]
for (i in (1:length(indexes))){
        data2$steps[indexes[i]]<-meanbyint$steps[meanbyint$interval==int[i]]
}

sumbyday2<-aggregate(steps~date,data2,sum)
qplot(steps, data=sumbyday2,bins=9,xlab='Steps',main='Total steps by day',
      ylab='Count')
mean(sumbyday2$steps)
median(sumbyday2$steps)

###########  5
data2$date<-as.Date(data2$date,format='%Y-%m-%d')
data2$weekday<-weekdays(data2$date)
# wd<-c('Monday','Tuesday','Wednesday','Thursday','Friday')
# we<-c('Saturday','Sunday')
we<-c('sábado','domingo')
wd<-c('lunes','martes','miércoles','jueves','viernes')

for (i in 1:5){
        data2$weekday<-gsub(wd[i], "weekday", data2$weekday)
}
for (i in 1:2){
        data2$weekday<-gsub(we[i], "weekend", data2$weekday)
}

data2$weekday<-as.factor(data2$weekday)
meanbyint2<-aggregate(steps~interval+weekday,data2,mean)
library(lattice)
xyplot(steps~interval|weekday,data=meanbyint2,layout=c(1,2),
       type='l',xlab='Interval',main='Average steps by interval',
       ylab='Average steps',col='blue')

