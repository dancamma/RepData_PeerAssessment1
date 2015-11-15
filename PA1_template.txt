# Reproducible Research: Peer Assessment 1





## Loading and preprocessing the data

First, I read the data by calling ```read.csv()``` fuction, passing the content of the **zipped file** contained in the repository.


```r
file <- unz('activity.zip', 'activity.csv')
data <- read.csv(file)
```

### Exploratory Data Analysis

I do a very basic **Exploratory Data Analyisis**.


```r
dim(data)
```

```
## [1] 17568     3
```

```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


## What is mean total number of steps taken per day?

To calculate the **total number of steps taken per day**, I group entries by date and, for each group, **sum up** all the steps.

The **dplyr package** is a perfect fit for this operation.


```r
processedData <- data %>% group_by(date) %>% summarize(totalSteps = sum(steps))
hist(processedData$totalSteps, 
     breaks = 10,
     xlab = "Total Steps",
     main="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Then I calculate the **mean** and the **median** of the total number of steps taken per day and show it it the histogram


```r
mean <- mean(processedData$totalSteps, na.rm = TRUE)
mean
```

```
## [1] 10766.19
```

```r
median <- median(processedData$totalSteps, na.rm = TRUE)
median
```

```
## [1] 10765
```


```r
par(mfrow=c(1,2))
hist(processedData$totalSteps, 
     breaks = 10, 
     xlab = "Total Steps",
     main="Mean")
abline(v = mean, col = "red")
hist(processedData$totalSteps, 
     breaks = 10,
     xlab = "Total Steps",
     main="Median")
abline(v = median, col = "green")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## What is the average daily activity pattern?

The code below displays a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
timeSeriesData <- data %>% group_by(interval) %>% summarize(totalSteps = mean(steps, na.rm = TRUE))
max <- timeSeriesData %>% arrange(desc(totalSteps))
plot(type="l", 
     timeSeriesData$totalSteps,
     x = timeSeriesData$interval,
     ylab = "Total Steps",
     xlab = "5-min intervals",
     main = "Average number of steps taken over 5-minute intervals"
     )
abline(v = max[1,]$interval, col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

The 5-minute interval that contains the maximum number of steps is:


```r
max[1,]$interval
```

```
## [1] 835
```


## Imputing missing values

The total number of missing values in the dataset is:


```r
rowsWithNA <- is.na(data$steps)
sum(rowsWithNA)
```

```
## [1] 2304
```

I filli in all of the missing values in the dataset with the **average across all the days** in the dataset for that particular time-interval.

I create a **new dataset** that is equal to the original dataset but with the missing **data filled in**:


```r
cleanedData <- data
for (i in 1:nrow(data)) {
  row <- data[i,]
  if (is.na(row$steps)) {
    fiveMinInt <- row$interval
    meanIndex <- which(timeSeriesData$interval == fiveMinInt)
    cleanedData[i,]$steps <- (timeSeriesData[meanIndex,]$totalSteps)
  }
}
```

and make a **histogram** of the **total number of steps** taken each day. I compare it with the previous one


```r
processedCleanedData <- cleanedData %>% group_by(date) %>% summarize(totalSteps = sum(steps))
ylim <- c(0,30)
par(mfrow=c(1,2))
hist(processedCleanedData$totalSteps, 
     breaks = 10, 
     ylim = ylim,
     xlab = "Total Steps",
     main="With imputed values")
hist(processedData$totalSteps, 
     breaks = 10,
     ylim = ylim,
     xlab = "Total Steps",
     main="Original Dataset")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


Then, I calculate the **mean** and the **median** of total number of steps taken per day


```r
cleanedMean <- mean(processedCleanedData$totalSteps, na.rm = TRUE)
cleanedMean
```

```
## [1] 10766.19
```

```r
cleanedMedian <- median(processedCleanedData$totalSteps, na.rm = TRUE)
cleanedMedian
```

```
## [1] 10766.19
```

To see if these values differ from the estimates from the first part of the assignment, I calculate the **difference** between means and medians between the two datasets.


```r
mean - cleanedMean
```

```
## [1] 0
```

```r
median - cleanedMedian
```

```
## [1] -1.188679
```

We can see that the mean **doesn't change** (because I've imputed missing values with the mean itself), while the **median changes slightly**.

## Are there differences in activity patterns between weekdays and weekends?

To create a new factor variable in the dataset with two levels - *"weekday"* and *"weekend"* indicating whether a given date is a weekday or weekend day I first create a **function that transform a date to this factor**: 


```r
getPeriodFactor = function(date) {
  weekday <- weekdays(as.Date(date))
  if (weekday == "Sunday" || weekday == "Saturday") {
    result <- "weekend"    
  } else {
    result <- "weekday"
  }
  return (as.factor(result))
}
```

Then, I use **lattice** package to make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
cleanedData$period <- sapply(cleanedData$date, getPeriodFactor)
timeSeriesCleanedData <- cleanedData %>% group_by(interval,period) %>% summarize(totalSteps = mean(steps, na.rm = TRUE))

xyplot(totalSteps ~ interval | period,
        data = timeSeriesCleanedData,
        type = "l",
        xlab = "5-min Interval",
        ylab = "Agerage number of steps",
        main = "Average number of steps taken over 5-min intervals (Weekend vs Weekdays)" )
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
