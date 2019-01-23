# Reproduce Research - project 1


Load required libraries



```r
library(dplyr)
library("ggplot2")
```

**1. Read data from the file**


```r
activity_data <- read.csv("activity.csv")
```

Calculate steps per day and

**2. Plot histogram**


```r
daily_activity <- with(activity_data, 
                       tapply(steps, date, sum, na.rm=TRUE))
daily_activity <- data.frame(date=names(daily_activity), steps=daily_activity)
hist(daily_activity$steps, main = "Activity Summary", xlab = "Steps", ylab = "")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

**3. Mean and meadian steps of each day**


```
##          date        steps      
##  2012-10-01: 1   Min.   :    0  
##  2012-10-02: 1   1st Qu.: 6778  
##  2012-10-03: 1   Median :10395  
##  2012-10-04: 1   Mean   : 9354  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

Calculate time interval's average steps

```r
time_activity <- with(activity_data, tapply(steps, interval, mean, na.rm=TRUE))
time_activity <- data.frame(interval=names(time_activity), steps=time_activity, stringsAsFactors = FALSE)
```

**4. Time series plot of the average number of steps taken**

```r
plot.ts(time_activity, plot.type=c("single"), ylab="Steps", xlab="Time Interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

**5. The following interval has the maximum number steps taken by an average**

```
## [1] 167
```



### 6. Imputing missing values

The total number of missing data is

```
## [1] 2304
```


The missing values are reported as NA.
These missing / NA values are replaced by the daily avarage 
of on that day.

If the activities were missing/NA then the daily average for 
that day is calculated as 0.


The following algorithem used to update the impute data.  


```r
impute_data <- read.csv("activity.csv")
daily_mean <- with(impute_data, 
                   tapply(steps, date, mean, na.rm=TRUE))
daily_mean <- data.frame(date=names(daily_mean), steps=daily_mean)

daily_mean$steps[is.nan(daily_mean$steps)] <- 0
for( i in 1:nrow(daily_mean)) {
      ind <- impute_data$date == daily_mean$date[i] & is.na(impute_data$steps)
      impute_data[ind, 1] <- daily_mean$steps[i]
}
```


![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)


Mean and meadian of imputed data


```
##          date        steps      
##  2012-10-01: 1   Min.   :    0  
##  2012-10-02: 1   1st Qu.: 6778  
##  2012-10-03: 1   Median :10395  
##  2012-10-04: 1   Mean   : 9354  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

Based the comparision of the mean and meadian with same in section 3
there is no change.

The impact of adding the daily average to the missing data is nothing.
It could be the result, either if there is missing data on a day all the data is missing or there no data is missing on a day





**8. Differences in activity patterns between weekdays and weekends**         

```r
impute_data$date <- as.Date(impute_data$date)
impute_data$day <- ""
names(impute_data)[3] <- "Time"

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekenddays <- c("Saturday", "Sunday")

impute_data$day[weekdays(impute_data$date) %in% weekdays] <- "Weekday"
impute_data$day[weekdays(impute_data$date) %in% weekenddays] <- "Weekendday"

ggplot(impute_data, aes(x=Time, y=steps))+
      geom_line()+
      facet_wrap(~day)+
      labs(x = "Time", 
           y = "Steps", 
           title="Time series of imputed data")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

