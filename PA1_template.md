# Reproducible Research: Peer Assessment 1

## External source usage:
<strong>>Multiplot function</strong>

Courtesy of Winston Chang, the author of "R Graphics Cookbook"

URL: <a href="http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/">http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/</a>

License: <small>This work is licensed under a Creative Commons Attribution-Share Alike 3.0 Unported License.
The R code is freely available for use without any restrictions. In other words: you may reuse the R code for any purpose (and under any license), but if you want to reuse the other content of this website, you must adhere to the CC license.</small>


```r
# Multiple plot function
# ======================================
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```


## Loading and preprocessing the data

```r
# reading csv file
rawData <- read.csv("./data/activity.csv")

#processing data
## converting date column to POSIX time
rawData <- mutate(rawData, fullDate = as.POSIXct(rawData$date, format="%Y-%m-%d"))
## selecting neccessary columns, adding another column to indicate type of a day 
## (weekend or weekday type) derived from POSIX time
data <- rawData %>%
        select(steps, fullDate, interval) %>%
        mutate(dayType = as.factor(ifelse(weekdays(fullDate) == "Sunday" | weekdays(fullDate) == "Saturday", "weekend", "weekday")))
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
qplot(sumOfSteps$x, geom="histogram", fill = I("lightgreen"), col = I("black"), xlab = "Steps", ylab = "Frequency", main = "Histogram of Total Steps Each Day", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
meanX <- mean(sumOfSteps$x, na.rm = TRUE)
print(meanX)
```

```
## [1] 10766.19
```

```r
quantileX <- quantile(sumOfSteps$x, probs = c(.5), na.rm = TRUE)
print(quantileX)
```

```
##   50% 
## 10765
```
The mean and the median of total number of steps taken per day are <strong>10766.19</strong> and <strong>10765.00</strong>, respectively.

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

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
maxX <- max(intervalData$x)
intervalMax <- intervalData[which(intervalData$x == max(intervalData$x)), 1]
```

Maximum number of steps is <strong>206.1698</strong> at 5-minute interval of <strong>0835</strong>

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
## I couldn't find a more elegant way to replace NAs with mean values for each 
## interval.  Ideally, using dplyr's mutate function would suffice.
# function(intv, intvDf) {
#        return(as.numeric(intvDf[which(intvDf$interval == intv), 2]))
#}
#newData <- mutate(data, cleanSteps = ifelse(is.na(data$steps), 
#       getMeanInterval(intv = data$interval, intvDf = intervalData), data$step))
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
qplot(newSumOfSteps$x, geom="histogram", fill = I("lightgreen"), col = I("black"), xlab = "Steps", ylab = "Frequency", main = "Histogram of Total Steps Each Day (after imputing NAs)", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
newMeanX <- mean(newSumOfSteps$x)
print(newMeanX)
```

```
## [1] 10766.19
```

```r
newQuantileX <- quantile(newSumOfSteps$x, probs = c(.5))
print(newQuantileX)
```

```
##      50% 
## 10766.19
```
After imputing missing values, new values for the mean and the median of total number of steps taken per day are <strong>10766.19</strong> and <strong>10766.19</strong>.  The mean remains the same, but the median moves closer to the mean (in this case, they are identical).

## Are there differences in activity patterns between weekdays and weekends?
A factor variable with two levels "weekday" and "weekend" has been created for the dataset <i>(Please refer to data processing code near the top of the page)</i>


```r
# filter interval data of weekdays
dataWD <- filter(newData, dayType == "weekday")
dataWD <- aggregate(dataWD$cleanSteps, by = list(interval = dataWD$interval), FUN = "mean")

p2 <- ggplot(data = dataWD, aes(x = interval, y = x)) + geom_line(color = "blue")  + labs(x = "Interval", y = "Average Number of Steps", title = "Weekday") + ylim(0, 250)

# filter interval data of weekends
dataWK <- filter(newData, dayType == "weekend")
dataWK <- aggregate(dataWK$cleanSteps, by = list(interval = dataWK$interval), FUN = "mean")

p3 <- ggplot(data = dataWK, aes(x = interval, y = x)) + geom_line(color = "green")  + labs(x = "Interval", y = "Average Number of Steps", title = "Weekend") + ylim(0, 250)

multiplot(p2, p3, cols=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
##testing
#dTest <- aggregate(newData$cleanSteps, by = list(interval = newData$interval), FUN = "sum")
dTest <- group_by(newData, interval, dayType)
dTest <- summarise(dTest, average = mean(cleanSteps))

pTest <- ggplot(data = dTest, aes(x = interval, y = average, color = dayType))
pTest + geom_line() +  aes(group = dayType) + 
    labs(x = "Interval", y = "Average Number of Steps", title = "Test") + 
    scale_colour_discrete(name  ="Day Type",
                          breaks=c("weekday", "weekend"),
                          labels=c("Weekday", "Weekend"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-2.png) 
