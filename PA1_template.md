# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Show any code that is needed to

1\. Load the data (i.e. `read.csv()`)


```r
data <- read.csv(unzip("activity.zip"))
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

2\. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data$date <- as.Date(data$date,"%Y-%m-%d")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1\. Make a histogram of the total number of steps taken each day


```r
sum.by.day <- aggregate(steps ~ date,data=data,FUN=sum)
barplot(sum.by.day$steps,names.arg=sum.by.day$date, xlab="date", ylab="total steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2\. Calculate and report the **mean** and **median** total number of steps taken per day


```r
steps.mean <- mean(sum.by.day$steps)
steps.median <- median(sum.by.day$steps)
```


* mean: 1.0766 &times; 10<sup>4</sup>

* median: 10765


## What is the average daily activity pattern?

1\. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg.by.interval <- aggregate(steps ~ interval,data=data,FUN=mean)
plot(avg.by.interval, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

2\. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avg.by.interval$interval[which.max(avg.by.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1\. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(data))
```

```
## [1] 2304
```

2\. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
data.with.avg <- merge(data,avg.by.interval,by="interval")
```

3\. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
nas <- is.na(data.with.avg$steps.x)
data.with.avg$steps.x[nas] <- data.with.avg$steps.y[nas]
new.data <- data.with.avg[,1:3]
str(new.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ interval: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ steps.x : num  1.72 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-01" "2012-11-23" ...
```

```r
sum(is.na(new.data))
```

```
## [1] 0
```

4\. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sum.by.day.new <- aggregate(steps.x ~ date,data=new.data,FUN=sum)
barplot(sum.by.day.new$steps.x,names.arg=sum.by.day.new$date, xlab="date", ylab="total steps per day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


```r
steps.mean.new <- mean(sum.by.day.new$steps)
steps.median.new <- median(sum.by.day.new$steps)
```


* mean: 1.0766 &times; 10<sup>4</sup>

* median: 1.0766 &times; 10<sup>4</sup>

## Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

1\. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data$day <- format(data$date,"%w")
data$day[data$day=="0"] <- "weekend"
data$day[data$day=="1"] <- "weekday"
data$day[data$day=="2"] <- "weekday"
data$day[data$day=="3"] <- "weekday"
data$day[data$day=="4"] <- "weekday"
data$day[data$day=="5"] <- "weekday"
data$day[data$day=="6"] <- "weekend"
data$day <- as.factor(data$day)
str(data)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

2\. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using **simulated data**:

**Your plot will look different from the one above** because you will be using the activity monitor data. Note that the above plot was made using the lattice system but you can make the same version of the plot using any plotting system you choose.


```r
avg <- aggregate(steps ~ interval + day,data=data,FUN=mean)
str(avg)
```

```
## 'data.frame':	576 obs. of  3 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ steps   : num  2.333 0.462 0.179 0.205 0.103 ...
```

```r
par(mfrow=c(2,1))
plot(avg$interval[avg$day=="weekday"],avg$steps[avg$day=="weekday"], type="l",xlab="interval weekdays",ylab="mean steps")
plot(avg$interval[avg$day=="weekend"],avg$steps[avg$day=="weekend"], type="l",xlab="interval weekends",ylab="mean steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
