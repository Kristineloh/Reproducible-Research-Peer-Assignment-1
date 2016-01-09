---
title: "PA1_template"
author: "Kristine Loh"

---

#1. Loading and preprocessing the data

##a. Read the data
```{r}
data <- read.csv("~/Desktop/activity.csv", header = TRUE, sep = ',', colClasses = c("numeric","character","integer"))
```

##b.Process/ transform the data

```{r}
library(knitr)
library(dplyr)
library(lubridate)
library(ggplot2)

data$date <- ymd(data$date)
```

##c. Check data

```{r}
str(data)
head(data)
```

#2. Mean total number of steps taken per day

##a. Total steps

```{r}
steps <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%

print
```

##b. Histogram of mean total number of steps

```{r}
library(ggplot2)
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "grey", binwidth = 5000) +
  labs(title = "Histogram of Steps per day", x = "Average steps taken at 5 minutes interval", y = "Frequency")
```

##c. Calculate mean and median

```{r}
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)
```

#3. Average daily activity pattern

##a. Average steps taken

```{r}
library(dplyr)
interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
```

##b. Times series plot

```{r}
library(ggplot2)
ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "black")
```

##c. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
interval[which.max(interval$steps),]
```

#4. Imputing missing values

##a. Calculate and report the total number of missing values in the dataset

```{r}
sum(is.na(data$steps))
```

##b. Fill in all of the missing values in the dataset
Fill in the missing values with the mean number of steps in the 5-minutes interval

##c. Create a new dataset that is equal to the original dataset but with the missing data filled in

```{r}
data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
```

###Proof that all missing values have been filled in
```{r}
sum(is.na(data_full$steps))
```

##di. Calculate the number of steps taken in each 5-minute interval

```{r}
library(dplyr)
steps_full <- data_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
        print
```

##dii. Make a histogram of the total number of steps taken each day - full data

```{r}
library(ggplot2)
ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "grey", binwidth = 5000) +
  labs(title = "Histogram of Steps per day full data", x = "Steps per day", y = "Frequency")
```

##diii. Mean and Median of filled data

```{r}
mean(steps_full$steps)
median(steps_full$steps)
```

##div. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The values did not different much. The mean remained the same. The median subsequently had a value of the mean. 
Filling in missing data with estimates of total daily number of steps would not affect the mean and median much.

#5. Are there differences in activity patterns between weekdays and weekends?

##a. Create a new factor variable in the dataset with weekday and weekend

```{r}
library(dplyr)
data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)
```

##b. Panel plot containing a time series plot

```{r}
library(lattice)
interval_full <- data_full %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))

ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
```

