# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
  library(base)
  library(reshape2)
  library(lattice)
  
  activitydata <- read.csv("activity.csv")
  activitydata$date <- as.Date(activitydata$date)
```
## What is mean total number of steps taken per day?

```r
  completedata <- activitydata[!is.na(activitydata$steps),]
  
  meltedtotaldata <- melt(completedata, id=c("date"), measure = "steps")
  castedtotaldata <- dcast(meltedtotaldata, date ~ variable, sum)
  
  hist(castedtotaldata$steps, col="cyan",main = "Total Steps Per Day Histogram",xlab = "Total Steps Per Day")
```

![](PA1_files/figure-html/unnamed-chunk-2-1.png) 

### Average total number of steps taken per day

```r
  mean(castedtotaldata$steps)
```

[1] 10766.19

### Median total number of steps taken per day

```r
  median(castedtotaldata$steps)
```

[1] 10765

## What is the average daily activity pattern?

```r
  meltedaveragedata <- melt(completedata, id=c("interval"), measure = "steps")
  castedaveragedata <- dcast(meltedaveragedata, interval ~ variable, mean)
  
  plot(castedaveragedata$interval,castedaveragedata$steps,type = "l",xlab = "Time Interval", ylab = "Average Steps Across All Days",main = "Average Steps Across All Days Per Time Interval")
```

![](PA1_files/figure-html/unnamed-chunk-5-1.png) 

### The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps

```r
  castedaveragedata[castedaveragedata$steps == max(castedaveragedata$steps),]$interval
```

[1] 835

## Imputing missing values

```r
  missingdata <- activitydata[is.na(activitydata$steps),]
```

### Total number of missing values in the dataset

```r
  nrow(missingdata)
```

[1] 2304

### Filling the missing steps with the average steps across all days based on time interval

```r
  filleddata <- missingdata
  
  for (i in 1:nrow(filleddata)) {
    filleddata[i,"steps"] <- castedaveragedata[castedaveragedata$interval == filleddata[i,"interval"],]$steps
  }
  
  enhanceddata <- rbind(filleddata,completedata)
  
  meltedenhanceddata <- melt(enhanceddata, id=c("date"), measure = "steps")
  castedenhanceddata <- dcast(meltedenhanceddata, date ~ variable, sum)
  
  hist(castedenhanceddata$steps, col="cyan",main = "Total Steps Per Day Histogram",xlab = "Total Steps Per Day")
```

![](PA1_files/figure-html/unnamed-chunk-9-1.png) 

### Average total number of steps taken per day with filled missing data

```r
  mean(castedenhanceddata$steps)
```

[1] 10766.19

### Median total number of steps taken per day with filled missing data

```r
  median(castedenhanceddata$steps)
```

[1] 10766.19

### Do these values differ from the estimates from the first part of the assignment? 

Only the median has changed.

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean has not changed since averages were used to fill the missing data. But it has shifted the median to equal the mean.

## Are there differences in activity patterns between weekdays and weekends?

```r
  enhanceddata$daytype <- "0"
  
  for (i in 1:nrow(enhanceddata)) {
    enhanceddata[i,"daytype"] <- weekdays(as.Date(enhanceddata[i,]$date))
  }
  
  enhanceddata[enhanceddata$daytype == "Monday",]$daytype <- "weekday"
  enhanceddata[enhanceddata$daytype == "Tuesday",]$daytype <- "weekday"
  enhanceddata[enhanceddata$daytype == "Wednesday",]$daytype <- "weekday"
  enhanceddata[enhanceddata$daytype == "Thursday",]$daytype <- "weekday"
  enhanceddata[enhanceddata$daytype == "Friday",]$daytype <- "weekday"
  enhanceddata[enhanceddata$daytype == "Saturday",]$daytype <- "weekend"
  enhanceddata[enhanceddata$daytype == "Sunday",]$daytype <- "weekend"
  
  enhanceddata$daytype <- as.factor(enhanceddata$daytype)
  
  meltedtypedata <- melt(enhanceddata, id=c("interval","daytype"), measure = "steps")
  castedtypedata <- dcast(meltedtypedata, interval + daytype ~ variable, mean)
  
  xyplot(steps ~ interval | daytype,data = castedtypedata,type = "l",layout = c(1,2))
```

![](PA1_files/figure-html/unnamed-chunk-12-1.png) 

During the weekdays, there is a large spike in the morning and a smaller spike in the afternoon, likely before and after work.

During the weekends, there are spikes and valleys throughout the day. The highest number of steps is still in the morning though.
