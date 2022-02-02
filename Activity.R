#Import data from repository
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


activity <- read.csv("C:/Users/Guillermo Polanco/Documents/RepData_PeerAssessment1/activity.csv")
activity <- as_tibble(activity)

#Turn date variable into date format and day of week

activity$wkday <- wday(ymd(activity$date),label=TRUE)

activity_mean <- group_by(activity, wkday)
act_sum <-summarize(activity_mean, steps=sum(steps, na.rm = TRUE))
act_sum

#Plot sum of steps 

ggplot(data=act_sum, aes(wkday, steps))+
  geom_bar(stat="identity")+
  xlab("Day of week")+
  ylab("Number of steps")+
  ggtitle("Number of Steps per day")

#Get mean and median
mean(act_sum$steps)
median(act_sum$steps)

#Transform to time series

act_time <- group_by(activity, interval)
act.time.avg <-summarize(act_time, steps=mean(steps, na.rm = TRUE))
act.time.avg <- act.time.avg[!is.na(act.time.avg$steps),]
act.time.avg

ggplot(act.time.avg, aes(interval, steps))+ #Yes! it works
  geom_line()+
  xlab("Steps")+
  ylab("Average number of steps")+
  ggtitle("Average of Steps per day")


#Calculating rows where NAs
sum(!complete.cases(activity))

#Separate NA vs not, calculate the average steps per interval to replace NAs

activity.na <- activity[is.na(activity$steps),]
activity.non.na2 <- activity[!is.na(activity$steps),]
activity.non.na <- activity[!is.na(activity$steps),] %>%
  group_by(interval) %>%
  summarize(steps=mean(steps))

activity.na.merged <- left_join(activity.na, activity.non.na, by= c("interval" = "interval")) %>%
  select(date,interval, steps.y,wkday) %>%
  rename(steps = steps.y)

#merge two datasets, NA and non-NA into one

activity.repl <- rbind(activity.non.na2,activity.na.merged)
activity.repl

#prepare data for charting
#Turn date variable into date format and day of week

activity.repl$wkday <- wday(ymd(activity.repl$date),label=TRUE)

activity.repl.mean <- group_by(activity.repl, wkday)
act.repl.sum <-summarize(activity.repl.mean, steps=sum(steps, na.rm = TRUE))
act.repl.sum

#Plot sum of steps 

ggplot(data=act.repl.sum, aes(wkday, steps))+
  geom_bar(stat="identity")+
  xlab("Day of week")+
  ylab("Number of steps")+
  ggtitle("Number of Steps per day")

#Get mean and median
mean(act.repl.sum$steps)
median(act.repl.sum$steps)


#Data for weekday vs weekend comparison
activity.repl <-activity.repl %>%
  mutate(daytype=if_else(activity.repl$wkday == "Sun", "Weekend",
                         if_else(activity.repl$wkday == "Sat","Weekend", "Weekday"),
                         "Weekday"))
                  
#Line plot 2x2
act.repl.time.avg <- aggregate(steps~interval + daytype, data=activity.repl, FUN="mean")


ggplot(act.repl.time.avg, aes(interval, steps, color=daytype))+ #Yes! it works
  geom_line()+
  facet_wrap(~daytype, ncol=1, nrow=2) +
  xlab("Interval")+
  ylab("Average number of steps")+
  ggtitle("Average of Steps Weekend vs Weekday")