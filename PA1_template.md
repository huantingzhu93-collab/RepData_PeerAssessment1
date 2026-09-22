---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. This assignment uses data from a personal activity monitoring device that records the number of steps taken in 5-minute intervals throughout the day. The data covers two months (October and November 2012) from an anonymous individual.

The dataset contains 17,568 observations with three variables:

- **steps**: Number of steps in a 5-minute interval (missing values coded as `NA`)
- **date**: Date in `YYYY-MM-DD` format
- **interval**: 5-minute interval identifier


``` r
library(dplyr)
library(lattice)

activity <- read.csv("activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## Loading and preprocessing the data

The data is loaded using `read.csv()`. No additional transformation is needed at this stage.

## What is mean total number of steps taken per day?

For this part, missing values are ignored.


``` r
daily_steps <- activity %>%
  group_by(date) %>%
  summarise(total = sum(steps, na.rm = TRUE))

# Histogram of total steps per day
hist(daily_steps$total,
     main = "Histogram of Total Steps Taken Each Day",
     xlab = "Total Steps per Day",
     col = "skyblue",
     breaks = 20)
```

![](PA1_template_files/figure-html/daily-steps-1.png)<!-- -->

``` r
# Mean and median
mean_steps <- mean(daily_steps$total, na.rm = TRUE)
median_steps <- median(daily_steps$total, na.rm = TRUE)

mean_steps
```

```
## [1] 9354.23
```

``` r
median_steps
```

```
## [1] 10395
```

**Results:**

- Mean total number of steps per day: 9354.2
- Median total number of steps per day: 10395

## What is the average daily activity pattern?


``` r
interval_avg <- activity %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

plot(interval_avg$interval, interval_avg$avg_steps,
     type = "l",
     main = "Average Number of Steps Across All Days",
     xlab = "5-minute Interval",
     ylab = "Average Steps",
     col = "blue")
```

![](PA1_template_files/figure-html/activity-pattern-1.png)<!-- -->

``` r
max_interval <- interval_avg$interval[which.max(interval_avg$avg_steps)]
max_interval
```

```
## [1] 835
```

The 5-minute interval that, on average, contains the maximum number of steps is **835**.

## Imputing missing values


``` r
# 1. Total number of missing values
total_na <- sum(is.na(activity$steps))
total_na
```

```
## [1] 2304
```

There are **2304** missing values in the dataset.

**Strategy for imputing missing values:**  
I fill each missing value with the mean number of steps for that specific 5-minute interval across all days.


``` r
interval_means <- activity %>%
  group_by(interval) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE))

activity_imputed <- activity
for (i in 1:nrow(activity_imputed)) {
  if (is.na(activity_imputed$steps[i])) {
    int <- activity_imputed$interval[i]
    activity_imputed$steps[i] <- interval_means$mean_steps[interval_means$interval == int]
  }
}

# New dataset with imputed values
daily_steps_imputed <- activity_imputed %>%
  group_by(date) %>%
  summarise(total = sum(steps))

hist(daily_steps_imputed$total,
     main = "Histogram of Total Steps (After Imputation)",
     xlab = "Total Steps per Day",
     col = "lightgreen",
     breaks = 20)
```

![](PA1_template_files/figure-html/impute-1.png)<!-- -->

``` r
mean_imputed <- mean(daily_steps_imputed$total)
median_imputed <- median(daily_steps_imputed$total)

mean_imputed
```

```
## [1] 10766.19
```

``` r
median_imputed
```

```
## [1] 10766.19
```

**Comparison after imputation:**

- Mean after imputation: 1.07662\times 10^{4}
- Median after imputation: 1.0766189\times 10^{4}

The mean and median are very similar to the original estimates (with missing values ignored). Imputing missing values has little impact on the overall daily step estimates in this dataset.

## Are there differences in activity patterns between weekdays and weekends?


``` r
activity_imputed$date <- as.Date(activity_imputed$date)
activity_imputed$daytype <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"),
                                   "weekend", "weekday")
activity_imputed$daytype <- as.factor(activity_imputed$daytype)

daytype_avg <- activity_imputed %>%
  group_by(interval, daytype) %>%
  summarise(avg_steps = mean(steps), .groups = "drop")

xyplot(avg_steps ~ interval | daytype,
       data = daytype_avg,
       type = "l",
       layout = c(1, 2),
       xlab = "Interval",
       ylab = "Average Number of Steps",
       main = "Average Steps: Weekday vs Weekend")
```

![](PA1_template_files/figure-html/weekday-weekend-1.png)<!-- -->

The panel plot shows that activity patterns differ between weekdays and weekends. Weekdays typically show a clearer morning peak, while weekends have a more evenly distributed activity pattern throughout the day.
