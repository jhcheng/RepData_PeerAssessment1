# change to english to prevent multi-byte error
Sys.setlocale("LC_TIME", "English")

# Load the data 
data <- read.csv("activity.csv", header=T, stringsAsFactors=F)

# ignore the missing values in the dataset
ok <- complete.cases(data)
data_ok <- data[ok,]

#1) What is mean total number of steps taken per day?
sum_per_day <- tapply(data_ok$steps, as.factor(data_ok$date), sum)
#  1.Make a histogram of the total number of steps taken each day
hist(sum_per_day, xlab="Total Number of Steps", main="")
#  2.Calculate and report the mean and median total number of steps taken per day
mean(sum_per_day)
median(sum_per_day)

#2) What is the average daily activity pattern?
#  1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#    averaged across all days (y-axis)
avg_per_interval <- tapply(data_ok$steps, as.factor(data_ok$interval), mean)
plot(names(avg_per_interval), 
     avg_per_interval, 
     type="l", 
     xlab="Interval", 
     ylab="Average Number of Steps")

#  2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
avg_per_interval[which.max(avg_per_interval)]

#3) Imputing missing values
#  1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(!ok)

#  2.Devise a strategy for filling in all of the missing values in the dataset. 
#    The strategy does not need to be sophisticated. 
#    For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
data_imputing <- data
data_imputing$steps <- ifelse(is.na(data_imputing$steps), 
                              round(avg_per_interval[as.character(data_imputing$interval)]) , 
                              data_imputing$steps)

#  3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

#  4.Make a histogram of the total number of steps taken each day and Calculate and 
#    report the mean and median total number of steps taken per day. 
#    Do these values differ from the estimates from the first part of the assignment? 
#    What is the impact of imputing missing data on the estimates of the total daily number of steps?
sum_per_day_imputing <- tapply(data_imputing$steps, 
                               as.factor(data_imputing$date), sum)
hist(sum_per_day_imputing, xlab="Total Number of Steps", main="")
mean(sum_per_day_imputing)
median(sum_per_day_imputing)

#4) Are there differences in activity patterns between weekdays and weekends?
#  1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating 
#    whether a given date is a weekday or weekend day.
data_imputing2 <- apply(data_imputing, 1, function(x) {
  wd <- weekdays(as.Date(x["date"], "%Y-%m-%d"))
  if (wd == "Saturday" | wd == "Sunday")
    x$wd <- "weekend"
  else
    x$wd <- "weekday"
  x
})
#  cast a list to data frame
#  reference: 
#  http://stackoverflow.com/questions/4512465/what-is-the-most-efficient-way-to-cast-a-list-as-a-data-frame?rq=1
f = function(x) function(i) unlist(lapply(x, `[[`, i), use.names=FALSE)
data_imputing2 <- as.data.frame(Map(f(data_imputing2), names(data_imputing2[[1]])))

#  2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
avg_per_interval_wd <- tapply(as.numeric(data_imputing2$steps), 
                              list(data_imputing2$interval, data_imputing2$wd), 
                              mean)

library(reshape2)
df <- data.frame(avg_per_interval_wd)
df$interval <- rownames(df)
df_m <- melt(df, id=c("interval"))

library(ggplot2)
as.factor(df_m$variable)
g <- ggplot(df_m, aes(as.numeric(interval), value))
p <- g + geom_line(aes(group=variable)) + 
         facet_grid(variable ~ .) + 
         scale_x_discrete(breaks=seq(0, 2355, by=100)) +
         xlab("Interval") +
         ylab("Average Number of Steps")
print(p)
