# Reproducible Research: Peer Assessment 1

We'll be taking advantage of the `lubridate` package to ease working with dates.

```r
library(lubridate)
```

## Loading and preprocessing the data

We read the csv, and take a copy of the data subset to complete cases only. 

```r
act_data <- read.csv("activity.csv")
act_complete <- act_data[complete.cases(act_data),]
```


## What is the mean total number of steps taken per day?

First we'll take a look at the break down of steps taken per day
with `NA` values removed. 


```r
# Group the steps by date and sum all the steps taken on each day
by_steps <- by(act_complete$steps, act_complete$date, FUN = sum)

hist(by_steps, 
     breaks = length(unique(by_steps)), 
     main   = "Histogram of number of steps taken per day",
     xlab = "Total Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The mean of the steps taken per day is

```r
mean(by_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

The median of the steps taken per day is

```r
median(by_steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

The average daily activity pattern is rendered from our complete
case data, taking the mean number of steps over each interval.


```r
mean_interval <- tapply(act_complete$steps, act_complete$interval, mean)
plot(names(mean_interval), 
     mean_interval, 
     type = "l", 
     xlab = "5 minute interval (hh:mm)", 
     ylab = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

#### Interval with maximum average number of steps


```r
time <- names(which.max(mean_interval))
# Pad the time to 4 places by prefixing with 0s then parse to lubridate
lubtime <- parse_date_time(sprintf("%04s", time), "H!M!")
# Concatenate the parts into a readable format.
form_time <- paste(hour(lubtime), ":", minute(lubtime), sep = "")
```

This tells us that the interval with the maximum average steps
at approx. 206 steps is **8:35**.

## Imputing missing values

Using

```r
sum(!complete.cases(acd))
```

```
## [1] 2304
```
we can establish that there are 2304 rows with `NA` values.

Our strategy for filling in the NA values will be to reuse the mean of the intervals calculated earlier to impute values for the `NA` holes in the data.

We produce a new data set by looping and lookuping up the interval mean via each
row's `interval` value.

```r
act_filled <- act_data
for ( i in 1:nrow(act_filled) ) {
  if ( is.na(act_filled$steps[i]) ) {
    act_filled$steps[i] <- mean_interval[as.character(act_filled$interval[i])]
  }
}
```

Now we can reproduce the same histogram using the data with `NA`s replaced by the mean.

```r
# Group the steps by date and sum all the steps taken on each day
by_steps_filled <- by(act_filled$steps, act_filled$date, FUN = sum)

hist(by_steps, 
     breaks = length(unique(by_steps_filled)), 
     main   = "Histogram of number of steps taken per day",
     xlab = "Total Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

This replacement yields the mean

```r
mean(by_steps_filled, na.rm = TRUE)
```

```
## [1] 10766.19
```
which is equal to our earlier mean.

However, there is a change in the median, which is now

```r
median(by_steps_filled, na.rm = TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

We introduce a factor that has the value `weekday` when the week day index of 
the `date` is neither 1 nor 7 (which are the weekend indices), or `weekend` when 
the index is 2-6.

```r
act_filled["weekday"] <- ifelse(wday(act_filled$date) != 1 & wday(act_filled$date) != 7, "weekday", "weekend")
```

The data is then aggregated across this interval and we can discern
differences through a panel plot by using the lattice library.


```r
intervalmean <- aggregate(steps ~ weekday + interval, data = act_filled, mean)

library(lattice)
xyplot(steps ~ interval | weekday,
       data = intervalmean,
       layout = c(1,2),
       xlab = "Interval",
       ylab = "Number of steps",
       type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

Here we can see on average fewer steps taken during the weekend morning 
'rush hour' - approximately 8:00am to 9:00am.  Instead we see more
steps taken on average throughout the rest of the day at the weekend.
