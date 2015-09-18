---
title: "PA1_template"
author: "Chang, Yang Yaw"
date: "Thursday, September 17, 2015"
output: html_document
---
#Peer Assessment 1
##Loading and preprocessing the data


```r
#unzip the pre downloaded data package
unzip(zipfile="repdata-data-activity.zip")
#read the data csv file
df_activity_original <- read.csv("activity.csv")
#remove rows with NA in steps column
df_activity <- df_activity_original[!is.na(df_activity_original$steps),]
```

##What is mean total number of steps taken per day?
Calculate the total number of steps taken per day

```r
array_total_steps <- tapply(df_activity$steps, df_activity$date,FUN=sum, na.rm=T)
```

Make a histogram of the total number of steps taken each day

```r
hist(array_total_steps, ylab = "Sum of Steps by Date", main = "Total number of steps taken each day", col="Green")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
#compute mean
mean(array_total_steps, na.rm=T)
```

```
## [1] 10766.19
```

```r
#compute median
median(array_total_steps, na.rm=T)
```

```
## [1] 10765
```

##What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(data.table)
#convert to datatable for subsequent processing
dt <- as.data.table(df_activity)
#compute mean by interval
dt_mean_steps <- dt[, mean(steps, na.rm=T), by="interval"]
#rename columns
setnames(dt_mean_steps, c("interval", "steps"))
#produce line plot
plot(dt_mean_steps$interval,dt_mean_steps$steps, type="l", col="green", xlab="Interval", ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
dt_mean_steps[which.max(dt_mean_steps$steps), interval]
```

```
## [1] 835
```

##Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing_data_count <- sum(is.na(df_activity_original$steps))
missing_data_count
```

```
## [1] 2304
```

Filling in all of the missing values in the dataset with mean

```r
#obtain the original column of steps data
new_dt_steps <- df_activity_original$steps
#identify the row index which steps has NA
na_steps <- is.na(new_dt_steps)
#produce a mean step array of same size
array_mean_steps <- rep(dt_mean_steps$steps, 288)
#replace NA with corresponding mean step by interval
new_dt_steps[na_steps] <- array_mean_steps[na_steps]
```

```
## Warning in new_dt_steps[na_steps] <- array_mean_steps[na_steps]: number of
## items to replace is not a multiple of replacement length
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
#create new data frame
new_df <- df_activity_original
#overwrite original steps data with new steps data
new_df$steps <- new_dt_steps
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```r
#compute total steps by date
new_total_steps <- tapply(new_df$steps, new_df$date,FUN=sum)
#generate histogram on new data
hist(new_total_steps, ylab = "Sum of Steps by Date", main = "Total number of steps taken each day", col="Green")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
#compute mean from new data
mean(new_total_steps)
```

```
## [1] 10766.19
```

```r
#compute median from new data
median(new_total_steps)
```

```
## [1] 10766.19
```

Upon replacing NA with mean data, followings are observed:
1 mean remains unchanged. This is due to missing data are replaced with mean of distribution (Expectation). Hence, the mean value is not impacted.
2 median value is now the mean value. This is due to the addition of numerical data (replacing NA with mean) changes the quantile profile of steps and essentially making the mean value the mode in the new distribution.

##Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
week_converter <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
}
#convert date
new_df$date <- as.Date(new_df$date)
#apply week_converter function
new_df$day <- sapply(new_df$date, FUN=week_converter)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(ggplot2)
#convert to data table
new_dt <- as.data.table(new_df)
#compute mena steps
new_dt_mean_steps <- new_dt[, mean(steps, na.rm=T), by="interval,day"]
#rename columns
setnames(new_dt_mean_steps, c("interval", "day", "steps"))
#generate line plots
ggplot(new_dt_mean_steps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("Interval") + ylab("Average number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
