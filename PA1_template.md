---
title: "Janet's Rmarkdown for Course Reproducibal Research"
author: "Janet"
date: "2017/3/24"
output: html_document
---

## 1-Loading and preprocessing the data
1.Load the data  
2.Process/transform the data (if necessary) into a format suitable for your analysis

```{r, echo=TRUE}
library(knitr)
library(lubridate)
```

1.1.Load the data  
```{r load, echo=TRUE}
activitydata <- read.csv("activity.csv")
str(activitydata)
```

1.2.Process/transform the data (if necessary) into a format suitable for your analysis
```{r Process, echo=TRUE}
activitydata$date <- ymd(activitydata$date)
str(activitydata)
```

## 2.What is mean total number of steps taken per day?
2.1.Calculate the total number of steps taken per day  
```{r sum,echo=TRUE}
tapply(activitydata$steps,as.factor(activitydata$date),sum,na.rm=TRUE)
```

2.2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day  
```{r hist,echo=TRUE}
sumstep<-tapply(activitydata$steps,as.factor(activitydata$date),sum,na.rm=TRUE)
hist(sumstep)
png(file="./figure/plot1.png",width = 480, height = 480,units = "px")
hist(sumstep)
dev.off()
```

2.3.Calculate and report the mean and median of the total number of steps taken per day
```{r mean_median,echo=TRUE}
meansteps <-tapply(activitydata$steps,as.factor(activitydata$date),mean,na.rm=TRUE)
mediansteps <- tapply(activitydata$steps,as.factor(activitydata$date),median,na.rm=TRUE)
meansteps
mediansteps
```

## 3.What is the average daily activity pattern?
3.1.Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r plot,echo=TRUE}
averagesteps <- tapply(activitydata$steps,as.factor(activitydata$interval),mean,na.rm=TRUE)
plot(unique(activitydata$interval),averagesteps,type="l",xlab="5-minute interval",ylab="average of steps")
png(file="./figure/plot2.png",width = 480, height = 480,units = "px")
plot(unique(activitydata$interval),averagesteps,type="l",xlab="5-minute interval",ylab="average of steps")
dev.off()
```

3.2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r maximum,echo=TRUE}
averagetable <- cbind(unique(activitydata$interval),averagesteps)
colnames(averagetable) <- c("interval","averagesteps")
averagetable[which.max(averagetable[,2]),]
```

## 4.Imputing missing values
4.1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
```{r NAs,echo=TRUE}
sum(is.na(activitydata$steps)|is.na(activitydata$date)|is.na(activitydata$interval))
```

4.2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
4.3.Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r missing,echo=TRUE}
activitybad <- activitydata[is.na(activitydata$steps),]
activitygood<- activitydata[!is.na(activitydata$steps),]
library(dplyr)
jointable <- left_join(activitybad,averagetable,by="interval",copy = TRUE)
jointable$steps=jointable$averagesteps
joinbad <- jointable[,1:3]
activitydata2 <- rbind(joinbad,activitygood)
```

4.4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r hist2_mean2_median2,echo=TRUE}
sumstep2<-tapply(activitydata2$steps,as.factor(activitydata2$date),sum,na.rm=TRUE)
hist(sumstep2)
png(file="./figure/plot3.png",width = 480, height = 480,units = "px")
hist(sumstep2)
dev.off()
meansteps2 <-tapply(activitydata2$steps,as.factor(activitydata2$date),mean,na.rm=TRUE)
mediansteps2 <- tapply(activitydata2$steps,as.factor(activitydata2$date),median,na.rm=TRUE)
meansteps2
mediansteps2
```

## 5.Are there differences in activity patterns between weekdays and weekends?
5.1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r weekday_weekend,echo=TRUE}
library(timeDate)
library(dplyr)
activitydata2<-mutate(activitydata2,weekdaylabel=isWeekday(activitydata2$date))
for(i in 1:nrow(activitydata2)){
             if(activitydata2[i,4]=="TRUE") activitydata2[i,4]="weekday"
     else activitydata2[i,4]="weekend"
}
```

5.2.Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r plot_weekday_weekend,echo=TRUE}
library(lattice)
activitydata2 <- transform(activitydata2,as.factor(activitydata2$weekdaylabel))
activityweekday <- activitydata2[activitydata2$weekdaylabel=="weekday",]
activityweekend <- activitydata2[activitydata2$weekdaylabel=="weekend",]
activityweekday2 <- tapply(activityweekday$steps,as.factor(activityweekday$interval),mean)
activityweekend2 <- tapply(activityweekend$steps,as.factor(activityweekend$interval),mean)
weeklabels <- rbind(cbind(unique(activitydata2$interval),as.character(activityweekday2),rep("weekday",length(activityweekday2))),cbind(unique(activitydata2$interval),as.character(activityweekend2),rep("weekend",length(activityweekend2))))
weekday <- data.frame(interval=unique(activitydata2$interval),average=activityweekday2,weekdaylabel=rep("weekday",length(activityweekday2)))
weekend <- data.frame(interval=unique(activitydata2$interval),average=activityweekend2,weekdaylabel=rep("weekend",length(activityweekend2)))
weeklabels <- rbind(weekday,weekend)
xyplot(average~interval|weekdaylabel,data=weeklabels,layout=c(1,2),type="l",ylab="average steps")
png(file="./figure/plot4.png",width = 480, height = 480,units = "px")
xyplot(average~interval|weekdaylabel,data=weeklabels,layout=c(1,2),type="l",ylab="average steps")
dev.off()
```