## Loading and preprocessing the data  

## setwd(""D:/Sameers/SamProjs/myR"")
setwd("D:/Sameers/SamProjs/myR")
library(plyr)
library(ggplot2)
library(lattice)

## Read activity csv
ACT <- read.csv("ReprRes_1/activity.csv", colClasses = c("numeric", "character","numeric"))

## format date
ACT$date <- as.Date(ACT$date, "%Y-%m-%d")

## remove NA
ACT.na <- na.omit(ACT) 

# sum steps by date
ACT.steps <- rowsum(ACT.na$steps, format(ACT.na$date, '%Y-%m-%d')) 
ACT.steps <- data.frame(ACT.steps) 
names(ACT.steps) <- ("steps")

# plot histogram
hist(ACT.steps$steps, xlab = "", col ="blue",
          main="Total Number of Steps Taken Daily")

# get the mean
mean(ACT.steps$steps); 

# get the median
median(ACT.steps$steps) 

## What is the average daily activity pattern?

# Calculate average steps for each of 5-minute interval during a 24-hour period
MinInt.mean.steps <- ddply(ACT.na,~interval, summarise, mean=mean(steps))

## plot
qplot(x=interval, y=mean, data = MinInt.mean.steps,  geom = "line",
      xlab="5 Minute Interval",
      ylab="Step Count", col ="blue",
      main="Avg No. of Steps Taken Across All Days"
)

## get mean
MinInt.mean.steps[which.max(MinInt.mean.steps$mean), ]

## Imputing missing values
ACT_NA <- sum(is.na(ACT))
ACT_NA

Avgsteps <- aggregate(steps ~ interval, data = ACT, FUN = mean)
fillNA <- numeric()

## loop and fill
for (i in 1:nrow(ACT)) {
  obs <- ACT[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(Avgsteps, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}

mod_ACT <- ACT
mod_ACT$steps <- fillNA

StepsTotal2 <- aggregate(steps ~ date, data = mod_ACT, sum, na.rm = TRUE)

## plot 
hist(StepsTotal2$steps, main = "Total steps by day", xlab = "day", col = "blue")

## get mean
mean(StepsTotal2$steps)

## get median
median(StepsTotal2$steps)

## Are there differences in activity patterns between weekdays and weekends?

## classify data for weekday & weekends
day <- weekdays(ACT$date)
daylevel <- vector()
for (i in 1:nrow(ACT)) {
  if (day[i] == "Saturday") {
    daylevel[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    daylevel[i] <- "Weekend"
  } else {
    daylevel[i] <- "Weekday"
  }
}
ACT$daylevel <- daylevel
ACT$daylevel <- factor(ACT$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = ACT, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")

## plot
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")



