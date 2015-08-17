--- 
title: "Peer Assessment 1 - Template"
author: "Melanie Maggard"
date: "Tuesday, August 11, 2015"
output: html_document
---

## Data Description
The data for this assignment was downloaded from the course web site and includes the following variables in 17,568 observations:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

##Loading and preprocessing the data

```{r global_options, echo=FALSE}
knitr::opts_chunk$set(echo=TRUE)
```

```{r load_data, resuls=FALSE}
setwd("~/ReproducibleResearchProject")
data <- read.csv(file = "./activity.csv", head=TRUE, sep = ",")
data$date <- as.Date(data$date, format="%Y-%m-%d")
```

##Data analysis

###What is the mean total number of steps taken per day?

The total number of steps taken per day is:
```{r}
total_steps <- with(data, tapply(steps, date, sum, na.rm=TRUE))
total_steps
```

The following is a histogram of the total number of steps taken each day:
```{r}
library(ggplot2)
qplot(total_steps,
    geom="histogram", 
    binwidth = 2000,
    main = "Total number of steps taken each day", 
    xlab="Total number of steps", 
    ylab="Frequency", 
    fill=I("steelblue")
    )
```

The mean and median of the total number of steps taken per day is:
```{r}
mean(total_steps, na.rm=TRUE)
median(total_steps, na.rm=TRUE)
```

###What is the average daily activity pattern?

The following is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):
```{r}
average_interval <- data.frame(unique(data$interval), with(data, tapply(steps, interval, mean, na.rm=TRUE)))
names(average_interval) <- c("interval", "average_steps")

par(mar=c(4,4,4,4))
plot(average_interval$interval, average_interval$average_steps, type="l",
      main = "Average number of steps per time interval",
      xlab = "Time Interval in Minutes of 24-hr Day \n(5 minute intervals from midnight (0) to 11:55pm (2355))",
      ylab = "Average Steps"
     )
```

The 5-minute interval, on average across all days in the dataset, that contains the maximum number of steps is:
```{r}
max_interval<- average_interval$interval[average_interval$average_steps==max(average_interval$average_steps)]
max_interval
```

This value corresponds to a time of 08:35am.

###Imputing missing values

The total number of missing values in the dataset is:
```{r}
missing <- sum(is.na(data))
missing
```

All missing values were replaced with the mean for that 5-minute interval, creating a new dataset.
```{r}
new_data <- merge(data, average_interval, by = "interval", all.x=TRUE)
new_data$steps[is.na(new_data$steps)] <- new_data$average_steps[is.na(new_data$steps)]
```

The following is a histogram of the total number of steps taken each day, when replacing the missing values with the mean for the 5-minute intervals:
```{r}
total_steps_new <- with(new_data, tapply(steps, date, sum))
qplot(total_steps_new,
    geom="histogram", 
    binwidth = 2000,
    main = "Total number of steps taken each day", 
    xlab="Total number of steps", 
    ylab="Frequency", 
    fill=I("steelblue")
    )
```

The mean and median of the total number of steps taken per day, when replacing the missing values with the mean for the 5-minute intervals, is:
```{r}
mean(total_steps_new, na.rm=TRUE)
median(total_steps_new, na.rm=TRUE)
```

Do these values differ from the estimates from the first part of the assignment?  What is the impact of imputing missing data on the estimates of the total daily number of steps?

These values did differ significantly, which demonstrated that there was an impact of imputing missing data on the estimates of the total daily number of steps.  The mean when the NAs were excluded was `r mean(total_steps, na.rm=TRUE)`, which was `r (mean(total_steps_new, na.rm=TRUE) - mean(total_steps, na.rm=TRUE))` lower than the mean when the NAs were replaced with the mean for that time interval (mean = `r mean(total_steps_new, na.rm=TRUE)`.  The median when the NAs were excluded was `r median(total_steps, na.rm=TRUE)`, which was also lower than the median when the NAs were replaced with the mean for that time interval, which was `r mean(total_steps_new, na.rm=TRUE)`.

###Are there differences in activity patterns between weekdays and weekends?

In order to answer this question we must create a new factor variable in the dataset with two levels - "weekday" and "weekend", indicating whether a given date is a weekday or weekend day.  We will use our new data with the missing values replaced with the values for the mean for the 5-minute intervals.

```{r}
library(lubridate)

new_data$day <- ifelse (grepl('^S', weekdays(new_data$date)), "weekend", "weekday")
new_data$day <- as.factor(new_data$day)
```

The following is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):

```{r}
new_average_interval <- aggregate(new_data$steps ~ new_data$interval + new_data$day, new_data, mean)
names(new_average_interval) <- c("interval", "day", "average_steps")

qplot(interval, average_steps, data=new_average_interval, geom="line" ,
    main = "Average number of steps per time interval",
    xlab = "Time Interval in Minutes of 24-hr Day \n(5 minute intervals from midnight (0) to 11:55pm (2355))",
      ylab = "Average Steps") +
    facet_grid(day ~ .) 

```