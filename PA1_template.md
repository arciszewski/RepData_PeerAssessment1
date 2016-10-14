# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Unzip the archive:

```r
unzip("activity.zip")
```
Load the data:

```r
raw_activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```
Load `ggplot2` and `dplyr` libraries:

```r
library(ggplot2)
library(dplyr)
```


## What is mean total number of steps taken per day?
Group input data by steps (NA values are filtered away):

```r
daily.steps <- raw_activity %>%
    na.omit() %>%
    group_by(date) %>%
    summarize(steps = sum(steps))
```
A histogram of days per number of steps:

```r
g1 <- ggplot(daily.steps, aes(x = steps)) +
    geom_histogram(bins = 20, fill = "salmon") +
    xlab("Steps") +
    ylab("Number of days")
print(g1)
```

![long caption](PA1_template_files/figure-html/qplot1-1.png)
Summary:

```r
summary(daily.steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

## What is the average daily activity pattern?
Activity, grouped by interval:

```r
avg.intervals <- raw_activity %>%
    na.omit() %>%
    group_by(interval) %>%
    summarize(steps = mean(steps, na.rm = TRUE)) %>%
    as.data.frame()
```
Plot of average daily activity:

```r
g2 <- ggplot(avg.intervals, aes(x = interval, y = steps)) +
    geom_area(colour = "darkgray", fill = "lightgray") +
    xlab("Minutes") +
    ylab("Steps")
print(g2)
```

![long caption](PA1_template_files/figure-html/qplot2-1.png)


Maximum steps were taken in

```r
avg.intervals[which.max(avg.intervals$steps), 1]
```

```
## [1] 835
```
minutes (in average).

## Imputing missing values
The number of missing values:

```r
sum(is.na(raw_activity))
```

```
## [1] 2304
```
Missing values will be imputed with averaged interval activity value:

```r
daily.steps.new <- raw_activity %>%
    left_join(avg.intervals, by = "interval") %>%
    mutate(steps = ifelse(is.na(steps.x), steps.y, steps.x)) %>%
    select(-steps.x, -steps.y) %>%
    group_by(date) %>%
    summarize(steps = sum(steps, na.rm = TRUE))
```
Comparrisson of histograms with and without imputting:

```r
g3 <- ggplot() +
    geom_histogram(data = daily.steps, bins = 20, aes(steps, fill = "With NA's", y= -..count..)) +
    geom_histogram(data = daily.steps.new, bins = 20, aes(steps, fill = "With imputed", y= ..count..)) +
    xlab("Steps") +
    ylab("Number of days") +
    scale_fill_hue("Type")
print(g3)
```

![long caption](PA1_template_files/figure-html/qplot3-1.png)
In general both datasets seem similar, apart from a peak at approximately 11000 steps.

Summary of the imputed dataset:

```r
summary(daily.steps.new$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
After imputing median and mean values both became equal to 10770.

## Are there differences in activity patterns between weekdays and weekends?
Grouping by Weekday/Weekend

```r
weekly.activity <- raw_activity %>%
    transform(weekday = as.factor(ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))) %>%
    group_by(weekday, interval) %>%
    summarize(steps = mean(steps, na.rm = TRUE))
```
Activity profiles:

```r
g4 <- ggplot(weekly.activity, aes(x = interval, y = steps, fill = weekday, colour = weekday)) +
    geom_area(alpha=0.6, lwd=0.8)
print(g4)
```

![long caption](PA1_template_files/figure-html/qplot4-1.png)
Activity profiles have similar shape, but overall, during weekends level of activity is significantly higher.

