
getData <- function() {
  
  if (!file.exists("activity.zip")) {    
    unzip("activity.zip")
  }
  
  read.csv("activity.csv")
}

get_mean_per_day <- function(data) {
  total_steps_per_day <- tapply(data$steps, data$date, sum)
  hist(total_steps_per_day)
  cat("mean: ",mean(total_steps_per_day, na.rm=TRUE), "\n")
  cat("median: ",median(total_steps_per_day, na.rm=TRUE), "\n")
}

## Main Processing

## Fetch the data##
act_df <- getData()

#What is mean total number of steps taken per day?
get_mean_per_day(act_df)

#What is the average daily activity pattern?
mean_Steps_per_interval <- tapply(act_df$steps, act_df$interval, mean, na.rm = TRUE)
df <- as.data.frame(mean_Steps_per_interval)
df$interval = rownames(df)
plot(df$interval, df$mean_Steps_per_interval, type ="l")
df[df$mean_Steps_per_interval==max(df$mean_Steps_per_interval),]


#Inputing missing values
sum(is.na(act_df[1]))
sum(is.na(act_df[2]))
sum(is.na(act_df[3]))

##Fill in the blanks
library(plyr)
mergeDf <- join(act_df,df)
mergeDf[is.na(mergeDf[1]),]$steps = mergeDf[is.na(mergeDf[1]),]$mean_Steps_per_interval

## Now recreate the histogram as before
get_mean_per_day(mergeDf)



#Are there differences in activity patterns between weekdays and weekends?
library(lubridate)
mergeDf_wk <- mutate(mergeDf, wkend=factor(weekdays(ymd(as.character(mergeDf$date))) %in% c("Saturday", "Sunday"), labels = c("Weekday","Weekend")))

##weekdays
get_mean_per_day(mergeDf_wk[mergeDf_wk$wkend=="Weekday",])

##weekends
get_mean_per_day(mergeDf_wk[mergeDf_wk$wkend=="Weekend",])

library(lattice)

#xyplot  scatterplot  y~x|A
#Make a panel plot containing a time series plot (i.e. type = "l") 
#of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all 
#weekday days or weekend days (y-axis).




#What is the average daily activity pattern?
agg <- aggregate( mergeDf_wk[,"steps"], mergeDf_wk[,c("interval","wkend")], FUN = mean )

big_join <- join(mergeDf_wk,agg)

panel.smoother <- function(x, y) {
  panel.xyplot(x, y) # show points 
  panel.loess(x, y)  # show smoothed line 
}

big_join =  transform(big_join, interval)

xyplot(x~interval|wkend, data=big_join, layout=c(1,2), xlab = "Interval", ylab = "Average steps per interval", type = "o")

