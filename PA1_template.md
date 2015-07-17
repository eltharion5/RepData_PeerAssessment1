---
title: "peer_assignemennt"
author: "eltharion5"
date: "Thursday, July 16, 2015"
output: html_document
---
#Loading and preprocessing the data

```r
raw_data<-read.csv("activity.csv")
```

#What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day


```r
steps_day<-sapply(split(raw_data,raw_data$date),function(x) sum(x$steps,na.rm=TRUE))
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(steps_day,xlab = "steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
m<-mean(steps_day,na.rm=TRUE)
md<-median(steps_day,na.rm=TRUE)
```

the mean is 9354.2295082 and the median 10395

#What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library("ggplot2")
time_series<- sapply(split(raw_data,raw_data$interval),function(x) colMeans(x[,c("steps","interval")],na.rm=TRUE))
time_series<-as.data.frame(t(time_series))
 qplot(data = time_series,x = interval,y = steps,na.rm=TRUE,geom="line")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
int_max<- time_series$interval[which.max(time_series$steps)]
```

the interval that contains on average the maximum number of steps is 835

#Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nb_na<-sum(is.na(raw_data))
```

the number of NA values is 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

here we replace with the mean for the 5-minute interval
  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
mer<- merge(raw_data,time_series,by.x="interval",by.y="interval")
nna<- is.na(mer$steps.x)
mer[nna,2]=mer[nna,4]
mer<- mer[,-4]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_day_2<-sapply(split(mer,mer$date),function(x) sum(x$steps.x,na.rm=TRUE))
hist(steps_day_2,xlab = "steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
m_2<-mean(steps_day_2,na.rm=TRUE)
md_2<-median(steps_day_2,na.rm=TRUE)
```

the mean is 1.0766189 &times; 10<sup>4</sup> compared to 9354.2295082  before
and the median 1.0766189 &times; 10<sup>4</sup> compared to 10395 before

replaciinng the NA's by the mean on a 5 minute interval tends make the mean and the median closer. The diistribution is less spread than before.

#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
d <- weekdays(as.Date(mer$date))
d_lev <- vector()
for (i in 1:nrow(mer)) {
    if (d[i] == "samedi") {
        d_lev[i] <- "Weekend"
    } else if (d[i] == "dimanche") {
        d_lev[i] <- "Weekend"
    } else {
        d_lev[i] <- "Weekday"
    }
}
mer$d_lev <- d_lev
mer$d_lev <- factor(mer$d_lev)
time_series_2<- aggregate(mer[2],list(interval=mer$interval,d_lev=mer$d_lev),FUN=mean)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
qplot(data=time_series_2,y = steps.x,x = interval,facets = d_lev~.,geom="line",ylab = "steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

