# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the data


```r
d0 <- read.csv("activity.csv", header=T)
```

Process the data (dates as POSIXct, POSIXt)


```r
library(lubridate)
d0$date <- ymd(d0$date)
str(d0)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(d0)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

Histogram of the total number of steps per day


```r
sumPerDay <- tapply(d0$steps, d0$date, sum, na.rm=T)
hist(sumPerDay, main = "Total number of steps by day", xlab = "Number of steps by day")
abline(v=mean(sumPerDay), col="red", lwd=3)
abline(v=median(sumPerDay), col="blue", lwd=3)
legend(x="topright", legend=c("mean","median"), col=c("red","blue"), bty="n", lwd=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
summary(sumPerDay)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

## What is the average daily activity pattern?

```r
averageSteps <- tapply(d0$steps, d0$interval, mean, na.rm=T)
intervalByHour <- as.numeric(names(averageSteps))/60
plot(intervalByHour, averageSteps, type = "l", ylab = "Average steps every 5 min", xlab = "Interval by hour")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

Hour that contains the maximum number of steps


```r
maxAverageSteps <- which(averageSteps==max(averageSteps))
d0$interval[maxAverageSteps]/60
```

```
## [1] 13.91667
```

## Imputing missing values
Total number of missing values in the dataset:

```r
sum(is.na(d0))
```

```
## [1] 2304
```
Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
d1 <- d0
filler <- mean(d0$steps, na.rm = T)
d1$steps[is.na(d1$steps)] <- filler
head(d1)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```


```r
sumPerDayd1 <- tapply(d1$steps, d1$date, sum, na.rm=T)
hist(sumPerDayd1, main = "Total number of steps by day w/filled NA values", xlab = "Number of steps by day w/filled NA values")
abline(v=mean(sumPerDayd1), col="red", lwd=3)
abline(v=median(sumPerDayd1), col="blue", lwd=3, lty=2)
legend(x="topright", legend=c("mean","median"), col=c("red","blue"), bty="n", lwd=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)

```r
summary(sumPerDayd1)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

## Are there differences in activity patterns between weekdays and weekends

```r
weekDays <- c("Monday", "Tuesday", "Wednsday", "Thursday", "Friday")
d1$weekDay <- factor((weekdays(d1$date) %in% weekDays), levels=c(FALSE, TRUE), labels=c("weekend", "weekday"))
head(d1)
```

```
##     steps       date interval weekDay
## 1 37.3826 2012-10-01        0 weekday
## 2 37.3826 2012-10-01        5 weekday
## 3 37.3826 2012-10-01       10 weekday
## 4 37.3826 2012-10-01       15 weekday
## 5 37.3826 2012-10-01       20 weekday
## 6 37.3826 2012-10-01       25 weekday
```

```r
wdSubset <- subset(d1, weekDay == "weekday")
weSubset <- subset(d1, weekDay == "weekend")
weekdayMean <- tapply(wdSubset$steps, wdSubset$date, mean)
weekendMean <- tapply(weSubset$steps, weSubset$date, mean)
head(weekdayMean)
```

```
## 2012-10-01 2012-10-02 2012-10-04 2012-10-05 2012-10-08 2012-10-09 
##   37.38260    0.43750   42.06944   46.15972   37.38260   44.48264
```

```r
head(weekendMean)
```

```
## 2012-10-03 2012-10-06 2012-10-07 2012-10-10 2012-10-13 2012-10-14 
##   39.41667   53.54167   38.24653   34.37500   43.14583   52.42361
```

```r
par(mfrow = c(2,1))
plot(weekdayMean, type = "l")
plot(weekendMean, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)
