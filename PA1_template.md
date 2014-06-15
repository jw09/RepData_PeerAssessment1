Personal Movement Data Analysis
========================================================


### Loading and preprocessing the data. 


```r
#read data
data <-read.csv("activity.csv", colClasses=c("numeric","Date","numeric"))
```

- Make a histogram of the total number of steps taken each day

```r
# sum steps by date
stepsum <- aggregate(data$steps, by=list(date=data$date), FUN=sum)
# plot histogram
hist(stepsum$x, xlab="Sum of steps by day", ylab="Number of days", main="")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### What is mean total number of steps taken per day?

- Calculate and report the mean and median total number of steps taken per day


```r
mean(stepsum$x, na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(stepsum$x, na.rm=TRUE)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# calculate average steps taken by date
DT <- data[is.na(data$steps)==FALSE,] 
stepmean <- aggregate(DT$steps, by=list(interval=DT$interval), FUN=mean)
# Make a time series plot of the 5-minute intervaland the average number of #steps taken.
plot(stepmean$interval, stepmean$x, type="l", xlab="interval", ylab="average number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

- Find out which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
stepmean$interval[stepmean$x==max(stepmean$x)]
```

```
## [1] 835
```

### Imputing missing values

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(data[is.na(data$steps)==TRUE,])
```

```
## [1] 2304
```

- Use mean of 5-minute interval to replace NA value, and creat a new dataset.

```r
# merge data and stepmean
dat <- merge(data, stepmean)
# subset steps with NA
dat1 <- dat[is.na(dat$steps)==TRUE,] 
# replace NA with average steps
dat1$steps <- dat1$x
# subset steps without NA
dat2 <- dat[is.na(dat$steps)==FALSE,] 
# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
d <- rbind(dat1, dat2)
data1 <- d[order(d$date, d$interval),]
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsum1 <- aggregate(data1$steps, by=list(date=data1$date), FUN=sum)
hist(stepsum1$x, xlab="Sum of steps by date", ylab="Number of days", main="")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(stepsum1$x)
```

```
## [1] 10766
```

```r
median(stepsum1$x)
```

```
## [1] 10766
```

- Calculate the difference. The results show that remove NA is a good estimation.

```r
mean(stepsum1$x) - mean(stepsum$x, na.rm=TRUE)
```

```
## [1] 0
```

```r
median(stepsum1$x) - median(stepsum$x, na.rm=TRUE)
```

```
## [1] 1.189
```


### Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data1$weektime <- as.factor(ifelse(weekdays(data1$date) %in% c("Saturday","Sunday"),"weekend", "weekday"))
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
stepmean1 <- aggregate(data1$steps, by=list(weektime=data1$weektime,interval=data1$interval), FUN=mean)
# Make a time series plot of the 5-minute intervaland the average number of #steps taken.
library(lattice)
xyplot(stepmean1$x ~ stepmean1$interval | stepmean1$weektime, data = stepmean1, layout = c(1, 2), type="l", xlab="interval", ylab="Number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
