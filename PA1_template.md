# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# read csv file
rawData <- read.csv("./data/activity.csv")
#processing data
rawData <- mutate(rawData, fullDate = as.POSIXct(rawData$date, format="%Y-%m-%d"))
data <- rawData %>%
        select(steps, fullDate, interval) %>%
        mutate(dayType = as.factor(ifelse(weekdays(fullDate) == "Sunday" | weekdays(fullDate) == "Saturday", "wk", "wd")))
```

## What is mean total number of steps taken per day?

```r
sumOfSteps <- aggregate(data$steps, by = list(date = data$fullDate), sum)
head(sumOfSteps)
```

```
##         date     x
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
qplot(sumOfSteps$x, geom="histogram", fill = I("lightgreen"), col = I("black"), xlab = "Steps (thousands)", ylab = "Frequency", main = "Histogram of Total Steps each day", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
mean(sumOfSteps$x, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
quantile(sumOfSteps$x, probs = c(.5), na.rm = TRUE)
```

```
##   50% 
## 10765
```

## What is the average daily activity pattern?
Calculate average on each 5-minute intervals

```r
intervalData <- aggregate(data$steps, by = list(interval = data$interval), mean, na.rm = TRUE)
head(intervalData)
```

```
##   interval         x
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
p <- ggplot(data = intervalData, aes(x = interval, y = x))
p + geom_line()  + labs(x = "Interval", y = "Average Number of Steps", title = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Maximum number of steps is 206.1698113 at 5-minute interval of 835

## Imputing missing values
1. Total missing values

```r
nrow(data[is.na(data$steps),])
```

```
## [1] 2304
```

2. Fill up NAs with median values of each interval

```r
## I couldn't find a more elegant way to replace NAs with mean values for each interval.  Ideally, using dplyr's mutate function would suffice.
#newData <- mutate(data, cleanSteps = ifelse(is.na(data$steps), getMeanInterval(intv = data$interval, intvDf = intervalData), data$step))
#head(newData)

cleanSteps <- numeric()
for(i in 1:nrow(data)) {
        if(is.na(data[i, 1])){
                intv <- data[i, 3]
                cleanSteps <- c(cleanSteps, intervalData[which(intervalData$interval == intv), 2])  
        }
        else {
                cleanSteps <- c(cleanSteps, data[i, 1]) 
        }
}
newData <- cbind(data, cleanSteps = cleanSteps)         

newSumOfSteps <- aggregate(newData$cleanSteps, by = list(date = newData$fullDate), sum)
head(newSumOfSteps)
```

```
##         date        x
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
qplot(newSumOfSteps$x, geom="histogram", fill = I("lightgreen"), col = I("black"), xlab = "Steps (thousands)", ylab = "Frequency", main = "Histogram of Total Steps each day", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 


## Are there differences in activity patterns between weekdays and weekends?
