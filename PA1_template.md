``` r
library(knitr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
opts_chunk$set(echo = TRUE)
setwd('/Users/Sharon/Documents')
```

Loading and preprocessing the data
----------------------------------

``` r
# load data
data_row <- read.csv('activity.csv')

# remove NA in data
data <- data_row[ with (data_row, { !(is.na(steps)) } ), ]

# print header summary data
head(data)
```

    ##     steps       date interval
    ## 289     0 2012-10-02        0
    ## 290     0 2012-10-02        5
    ## 291     0 2012-10-02       10
    ## 292     0 2012-10-02       15
    ## 293     0 2012-10-02       20
    ## 294     0 2012-10-02       25

What is mean total number of steps taken per day?
-------------------------------------------------

``` r
Daily_steps <- group_by(data, date)
Daily_steps2 <- summarise(Daily_steps, total = sum(steps))
Daily_steps2
```

    ## # A tibble: 53 × 2
    ##          date total
    ##        <fctr> <int>
    ## 1  2012-10-02   126
    ## 2  2012-10-03 11352
    ## 3  2012-10-04 12116
    ## 4  2012-10-05 13294
    ## 5  2012-10-06 15420
    ## 6  2012-10-07 11015
    ## 7  2012-10-09 12811
    ## 8  2012-10-10  9900
    ## 9  2012-10-11 10304
    ## 10 2012-10-12 17382
    ## # ... with 43 more rows

``` r
hist(Daily_steps2$total, main="Number of steps per day", col = "blue")
```

![](hist-1.png)

``` r
summary(Daily_steps2)
```

    ##          date        total      
    ##  2012-10-02: 1   Min.   :   41  
    ##  2012-10-03: 1   1st Qu.: 8841  
    ##  2012-10-04: 1   Median :10765  
    ##  2012-10-05: 1   Mean   :10766  
    ##  2012-10-06: 1   3rd Qu.:13294  
    ##  2012-10-07: 1   Max.   :21194  
    ##  (Other)   :47

What is the average daily activity pattern?
-------------------------------------------

``` r
stepsinterval <- aggregate(steps ~ interval, data, mean)
plot(stepsinterval$interval, stepsinterval$steps, type='l', 
     main="Average steps per day")
```

![](unnamed-chunk-5-1.png)

``` r
max <- which.max(stepsinterval$steps)

# find interval with this max
stepsinterval[max, ]
```

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

``` r
sum(is.na(data_row))
```

    ## [1] 2304

Filling in all the missing values using the mean of that 5 minute interval

``` r
data_imputed <- data_row
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_value <- data_imputed$interval[i]
    steps_value <- stepsinterval[
      stepsinterval$interval == interval_value,]
    data_imputed$steps[i] <- steps_value$steps
  }
}
```

``` r
df_imputed_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)
head(df_imputed_steps_by_day)
```

    ##         date    steps
    ## 1 2012-10-01 10766.19
    ## 2 2012-10-02   126.00
    ## 3 2012-10-03 11352.00
    ## 4 2012-10-04 12116.00
    ## 5 2012-10-05 13294.00
    ## 6 2012-10-06 15420.00

``` r
hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day with filled missing values")
```

![](unnamed-chunk-10-1.png)

``` r
mean(df_imputed_steps_by_day$steps)
```

    ## [1] 10766.19

``` r
median(df_imputed_steps_by_day$steps)
```

    ## [1] 10766.19

``` r
mean(Daily_steps2$total)
```

    ## [1] 10766.19

``` r
median(Daily_steps2$total)
```

    ## [1] 10765

means remain the same, but there is a difference in median value

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

``` r
data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"
```

``` r
# convert type_of_day
data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)

# calculate average daily steps by interval
df_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)

# plot
qplot(interval, 
      steps, 
      data = df_imputed_steps_by_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps",
      col = "red",
      main = "Weekdays vs Weekends") +
  facet_wrap(~ type_of_day, ncol = 1)
```

    ## Warning: Ignoring unknown parameters: type

![](unnamed-chunk-16-1.png)
