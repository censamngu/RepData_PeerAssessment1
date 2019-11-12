---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

Load packages and unzip data to obtain csv file.


```r
#load packages
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

#set directory
setwd("H:/Training/Coursera/RepData_1/RepData_PeerAssessment1-master")

#unzip file
unzip("activity.zip")
```

1. Load the data.


```r
#read csv file
act <-read.csv("activity.csv", header=TRUE, stringsAsFactors=FALSE)
```

2. Process/transform the data.


```r
#format date as %y%m%d
act[,2]<-as.Date(act$date)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day.


```r
#find the total number of steps taken by date
total_steps <- act %>%
  group_by(date) %>%
  summarize(steps = sum(steps))

#preview total_steps
head(total_steps, 5)
```

```
## # A tibble: 5 x 2
##   date       steps
##   <date>     <int>
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
```


```r
#create a histogram using ggplot
ggplot(total_steps, aes(x = steps)) +
    geom_histogram(fill = "palevioletred", binwidth = 1000) +
    labs(title = "Total Steps Taken Per Day", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/step_histogram-1.png)<!-- -->
    
2. Calculate and report the mean and median total number of steps taken per day.


```r
#find the mean then print the results
mean_steps <- total_steps %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))

print(mean_steps)
```

```
## # A tibble: 1 x 1
##   mean_steps
##        <dbl>
## 1     10766.
```



```r
#find the median then print the results
median_steps <- total_steps %>%
  summarize(median_steps = median(steps, na.rm = TRUE))

print(median_steps)
```

```
## # A tibble: 1 x 1
##   median_steps
##          <int>
## 1        10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
#compute the mean activity pattern by interval
avg_daily_steps <- act %>%
  group_by(interval) %>%
  summarize(steps = mean(steps, na.rm=TRUE))

#preview 
head(avg_daily_steps, 5)
```

```
## # A tibble: 5 x 2
##   interval  steps
##      <int>  <dbl>
## 1        0 1.72  
## 2        5 0.340 
## 3       10 0.132 
## 4       15 0.151 
## 5       20 0.0755
```


```r
#create time series plot using ggplot
ggplot(avg_daily_steps, aes(x = interval, y = steps)) +
    geom_line(color = "slateblue4", size = 1) +
    labs(title = "Average Daily Activity Pattern", x = "Interval", y = "Average Steps Per Day")
```

![](PA1_template_files/figure-html/lineplot_pattern-1.png)<!-- -->
    
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#find the mean step by interval then filter for the max step
act %>% 
  group_by(interval) %>% 
  summarize(steps = mean(steps, na.rm=TRUE)) %>%
  filter(steps == max(steps))
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
#report the total NAs
sum(is.na(act$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
#find the median 
median_imputed <- act %>%
  summarize(med_step = median(steps, na.rm=TRUE))

#replace the missing values with the median above
act <- act %>%
  mutate(steps = replace(steps, which(is.na(steps)), median_imputed) %>% unlist())
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#export the data
write.csv(act, file = "act_tidy.csv", row.names = FALSE)
```
  
4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#find the total number of steps taken by date of the imputed dataset
total_steps <- act %>%
  group_by(date) %>%
  summarize(steps = sum(steps))
  
#preview
head(total_steps, 5)
```

```
## # A tibble: 5 x 2
##   date       steps
##   <date>     <dbl>
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
```


```r
#find the mean of the imputed dataset
mean_steps <- total_steps %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))

#report the imputed mean
print(mean_steps)
```

```
## # A tibble: 1 x 1
##   mean_steps
##        <dbl>
## 1      9354.
```


```r
#find the median of the imputed dataset
median_steps <- total_steps %>%
  summarize(median_steps = median(steps, na.rm = TRUE))

#report the imputed median
print(median_steps)
```

```
## # A tibble: 1 x 1
##   median_steps
##          <dbl>
## 1        10395
```


```r
#create histogram of imputed dataset with ggplot
ggplot(total_steps, aes(x = steps)) + geom_histogram(fill = "darksalmon", binwidth = 1000) + labs(title = "Total Steps Taken Per Day: Data Imputed", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/histogram_imputed-1.png)<!-- -->

| Estimates                              | Mean\_Steps | Median\_Steps |
|----------------------------------------|-------------|---------------|
| First Dataset (including NA)           | 10765       | 10765         |
| Second Dataset (replace NA with median | 9354.23     | 10395         |

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
#creating 2 level factor variable: weekday and weekend
act_week <- act %>%
  mutate(day_of_week = weekdays(date)) %>%
  mutate(type = ifelse(day_of_week == "Saturday" | day_of_week == "Sunday", "Weekend", "Weekday")) %>%
  mutate_each(list(factor), type) 

#preview
head(act_week, 5)
```

```
##   steps       date interval day_of_week    type
## 1     0 2012-10-01        0      Monday Weekday
## 2     0 2012-10-01        5      Monday Weekday
## 3     0 2012-10-01       10      Monday Weekday
## 4     0 2012-10-01       15      Monday Weekday
## 5     0 2012-10-01       20      Monday Weekday
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#create panel plot using ggplot
act_week %>%
  group_by (type, interval) %>%
  mutate(mean_daily_steps = mean(steps)) %>%
  ggplot(aes(x = interval, y = mean_daily_steps, color =`type`)) + geom_line() + labs(title = "Average Daily Steps: Weekday Versus Weekend", x = "Interval", y = "Steps") + facet_wrap(~`type`, ncol = 1, nrow = 2) 
```

![](PA1_template_files/figure-html/day_plot-1.png)<!-- -->

