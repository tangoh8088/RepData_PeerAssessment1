##Loading and processing the data
unzip("activity.zip")
dat <- read.csv("activity.csv")
dat$date <- as.Date(dat$date, format = "%Y-%m-%d")
library(plyr)
library(dplyr)
library(ggplot2)
library(lattice)

##What is mean total number of steps taken per day?
##Find out total number of steps per day
todat <- aggregate(dat$steps, list(dat$date), sum)
colnames(todat) <- c("Date", "Total_Steps")
as.numeric(todat$Total_Steps)

##Plot histogram
hist(todat$Total_Steps, breaks = 20, ylab = "Frequency", xlab = "Total Steps", main = "Total Number of Steps Taken per Day")

##Mean total number of steps taken per day
mean(todat$Total_Steps, na.rm = TRUE)

## [1] 10766.19

##Median total number of steps taken per day
median(todat$Total_Steps, na.rm = TRUE)
## [1] 10765

##What is the average daily activity pattern?
newdat <- na.omit(dat)
intdat <- ddply(newdat, .(interval), summarise, ave = mean(steps))

##Create a time series plot
ggplot(intdat, aes(interval, ave)) + geom_line() + labs(title = "Average Number of Steps Taken by Interval") + labs(x = "Interval") + labs(y = "Average Number of Steps")

##5-minute interval that contains the maximum number of steps
intdat[intdat$ave == max(intdat$ave),]
##interval      ave
##104      835 206.1698

##Total number of rows with NAs
sum(is.na(dat))

##[1] 2304

##Use mean of 5-minute interval to fill missing data
datrow <- nrow(dat)
filldat <- dat
for (i in 1:datrow) {
  if (is.na(filldat$steps[i])) {
    filldat$steps[i] <- intdat[which(filldat$interval[i] == intdat$interval),]$ave
  }
}

sum(is.na(filldat))
##[1] 0

##Plot new histogram
newtodat <- aggregate(filldat$steps, list(filldat$date), sum)
colnames(newtodat) <- c("Date", "Total_Steps")
as.numeric(newtodat$Total_Steps)

hist(newtodat$Total_Steps, breaks = 20, ylab = "Frequency", xlab = "Total Steps", main = "Total Number of Steps Taken per Day (with Filled Data)")

##Mean total number of steps taken per day
mean(newtodat$Total_Steps)

## [1] 10766.19

##Median total number of steps taken per day
median(newtodat$Total_Steps)
## [1] 10766.19

##Are there differences in activity patterns between weekdays and weekends?
filldat$day <- weekdays(filldat$date)
filldat$day <- as.factor(ifelse(filldat$day == "Saturday" | filldat$day == "Sunday", "Weekend", "Weekday"))


##Panel plot
avedat <- aggregate(filldat$steps, list(filldat$interval,filldat$day), mean)
colnames(avedat) <- c("Interval", "Day", "Average_Steps")
xyplot(avedat$Average_Steps ~ avedat$Interval | avedat$Day, type = "l", layout = c(1,2), main = "Average Number of Steps Across All Weekday Days and Weekend Days", ylab = "Average Number of Steps", xlab = "Interval")

