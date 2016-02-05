#
# All of the code is documented in the
# README.md and PA1_Template.Rmd file. This file only contains
# the code, with remarks to which question
# it belongs. Full "novel" in the md/Rmd file.
#

setwd("~/mycloud/Private_DataScience/Coursera/10 Data Science Specialisation/40 Reproducible Research/Assignments/week1")

#
# 1
#

#
# Load the libraries we are going to use
# and clean out any left-overs in the $env
rm(list=ls())
dev.off(dev.list())
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(data.table)

#
# Read the csv file and transform the date string to a proper date
# and the interval to a factor
steps <- read.csv("activity.csv")
steps$date <- as.Date(steps$date,"%Y-%m-%d")
steps$interval <- as.factor(steps$interval)

#
# Clean out the NAs
steps <- steps[complete.cases(steps),]

#
# Get the number of steps per day and clean up column names
stepsDay <- as.data.frame(tapply(steps$steps,steps$date,sum))
stepsDay$Day <- row.names(stepsDay)
colnames(stepsDay) <- c("Steps","Day")

#
# Add the mean, median and WeekDay values to the data.frame
stepsDay$AvgSteps <- rep(mean(stepsDay$Steps),nrow(stepsDay))
stepsDay$MedSteps <- rep(median(stepsDay$Steps),nrow(stepsDay))
stepsDay$WeekDay <- weekdays(as.Date(stepsDay$Day))

#
# 2
#


#
# Plot the historgram of the step groups.
png(filename = "reprores1a.png", width = 640, height = 640)
par(mar=c(10,10,10,10))
hist(stepsDay$Steps, 
     xlab = "Total Number of Steps in Groups
     Example: 5 days with less than 5000 steps", 
     ylab = "Days with Step Ranges", 
     main = "Number of Steps per Day in Groups",
     col = "lightblue")
dev.off()

#
# Plot it on the X axis, dont plot legend and turn the x-labels
png(filename = "reprores1b.png", width = 960, height = 640)
g <- ggplot(stepsDay, aes(x=Day, y=Steps, fill = 20))
g + geom_bar(stat = "Identity", alpha = 0.9) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Day") +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Number of Recorded Step") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Number of Steps Recorded per Day") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
dev.off()   

#
# 3
#

#
# The mean and median values for the documentation
median(stepsDay$Steps)
mean(stepsDay$Steps)

#
# 4
#

#
# Plot that time series 
png(filename = "reprores1c.png", width = 960, height = 640)
g <- ggplot(stepsDay, aes(x=Day, y=AvgSteps, fill = 20))
g + geom_bar(stat = "Identity", alpha = 0.9) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Steps per Day") +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Average Number of Steps") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Average Number of Steps per Day") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
dev.off() 


#
# 5 Minute interval

#
# Lets grab the average values for the intervals first
# and fix the column names
stepsAvgInt <- as.data.frame(tapply(steps$steps,steps$interval, mean))
stepsAvgInt$Interval <- row.names(stepsAvgInt)
colnames(stepsAvgInt) <- c("Steps","Interval")

#
# Ensure geom_bar does not try and sort the x Axis
stepsAvgInt$Interval <- factor(stepsAvgInt$Interval, levels = stepsAvgInt$Interval)


#
# lets plot that
png(filename = "reprores1d.png", width = 960, height = 640)
g <- ggplot(stepsAvgInt, aes(x=Interval, y=Steps, fill = 20))
g + geom_bar(stat = "Identity", alpha = 0.9) + geom_hline(yintercept=100, col = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    #theme(axis.ticks.x = element_blank()) +
    #theme(axis.ticks.length = 12) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Daily Intervals") +
    #
    # ticks only at the full hour
    scale_x_discrete(breaks=seq(0,2355,100)) +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Number of Steps per Interval") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Average Number of Steps per Interval") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
dev.off()

#
# visual inspection gives us the time interval between 0800 and 0900 
# in the morning seems to be the most active ones. Can
# we verify this by taking the means of every hours bucket?
# I want twelve buckets and the average of those

#
# No time for that. Lets grab the values between 800 and 855 and plot those
stepsMaxInt <- stepsAvgInt[with(stepsAvgInt, stepsAvgInt$Interval %in% seq(800,855,5)),]

#
# lets plot that
png(filename = "reprores1e.png", width = 960, height = 640)
g <- ggplot(stepsMaxInt, aes(x=Interval, y=Steps, fill = 20))
g + geom_bar(stat = "Identity", alpha = 0.9) + geom_hline(yintercept=100, col = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    #theme(axis.ticks.x = element_blank()) +
    #theme(axis.ticks.length = 12) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Between 800 and 855") +
    #
    # ticks only at the full hour
    #scale_x_discrete(breaks=seq(0,2355,100)) +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Number of Steps per Interval") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Steps per Interval between 800 and 855") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
dev.off()


#
# Lets split the stuff out per day of week

#
# Average steps per weekday, day
stepsAvgWday <- arrange(stepsDay %>% group_by(WeekDay,Day) %>% summarise(mean(Steps)),WeekDay,Day)
colnames(stepsAvgWday) <- c("WeekDay","Day","Steps")
#

g <- ggplot(stepsAvgWday, aes(x=Day, y=Steps))
p1 <- g + geom_bar(stat = "Identity", aes(fill=WeekDay)) +
    facet_grid(.~WeekDay) +
    theme(legend.position="none") 

#
# Compare this with the number of steps per interval and day
steps$WeekDay <- weekdays(as.Date(steps$date))
stepsAvgDayInt <- arrange(steps %>% group_by(WeekDay,interval) %>% summarise(mean(steps)),WeekDay,interval)
colnames(stepsAvgDayInt) <- c("WeekDay","Interval","Steps")

g <- ggplot(stepsAvgDayInt, aes(x=Interval, y=Steps))
p2 <- g + geom_bar(stat = "Identity", aes(fill=WeekDay)) +
    facet_grid(.~WeekDay) +
    theme(legend.position="none")

png(filename = "reprores1f.png", width = 720, height = 960)
grid.arrange(p1,p2, nrow = 2, ncol = 1, top = "Daily Steps Profile vs Interval Steps Profile")
dev.off() 

#
# 6 Imputing the missing data
# 7 Plotting the data as well.
#

stepsOrig <- read.csv("activity.csv")
stepsOrig$date <- as.Date(steps$date,"%Y-%m-%d")
stepsOrig$interval <- as.factor(steps$interval)
stepsOrig$WeekDay <- weekdays(as.Date(stepsOrig$date))
colnames(stepsOrig) <- c("Steps","Day","Interval","WeekDay")

#
# Lookup and replace the missing values
for (i in 1:nrow(stepsOrig)) {

    row = stepsOrig[i,]
    ISNA <- FALSE
    
    if (is.na(stepsOrig[i,1])) {
        ISNA <- TRUE
        lookWeekDay <- row[1,4]
        lookInterval <- row[1,3]
        stepsOrig[i,1] <- as.integer(round(filter(stepsAvgDayInt, WeekDay == lookWeekDay & Interval == lookInterval)[,3]))
    }
    
}

#
# Average the values per day
stepsOrigAvg <- as.data.frame(tapply(stepsOrig$Steps, stepsOrig$Day, sum))
stepsOrigAvg$Day <- row.names(stepsOrigAvg)
colnames(stepsOrigAvg) <- c("Steps","Day")


#
# Plot that time series 
g <- ggplot(stepsOrigAvg, aes(x=Day, y=Steps, fill = 20))
p1 <- g + geom_bar(stat = "Identity", alpha = 0.9) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Steps per Day") +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Average Number of Steps") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Average Number of Steps per Day Adjusted for Missing Values") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))

#
# Add the comparison as a service for the reader
g <- ggplot(stepsDay, aes(x=Day, y=Steps, fill = 20))
p2 <- g + geom_bar(stat = "Identity", alpha = 0.9) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightgrey")) +
    theme(strip.background = element_rect(fill = "lightgrey")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Steps per Day") +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Average Number of Steps") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Average Number of Steps per Day without Adjustment for Missing Values") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))

png(filename = "reprores1g.png", width = 720, height = 960)
grid.arrange(p1,p2, nrow = 2, ncol = 1)
dev.off() 

#
# 8 Separating out the data for Weekend and Weekdays
#

stepsOrigAvg$WeekDay <- weekdays(as.Date(stepsOrigAvg$Day))
stepsOrigWEDays <- filter(data.table(stepsOrigAvg), WeekDay %in% c("Saturday","Sunday"))
stepsOrigWEDays <- as.data.frame(tapply(stepsOrigWEDays$Steps,stepsOrigWEDays$WeekDay, mean))
stepsOrigWEDays$Day <- row.names(stepsOrigWEDays)
colnames(stepsOrigWEDays) <- c("Steps","Day")

stepsOrigWKDays <- filter(data.table(stepsOrigAvg), WeekDay %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
stepsOrigWKDays <- as.data.frame(tapply(stepsOrigWKDays$Steps,stepsOrigWKDays$WeekDay, mean))
stepsOrigWKDays$Day <- row.names(stepsOrigWKDays)
colnames(stepsOrigWKDays) <- c("Steps","Day")

g <- ggplot(stepsOrigWEDays, aes(x=Day, y=Steps))
p1 <- g + geom_bar(stat = "Identity", aes(fill=Day)) +
    geom_text(aes(label=paste(round(Steps,0)," Steps")), position = position_dodge(width=0.9), vjust=-.5, color="black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightgrey")) +
    theme(strip.background = element_rect(fill = "lightgrey")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Steps per Day") +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Average Number of Steps") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Average Number of Steps for Weekend Days Adjustmed for Missing Values") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))

g <- ggplot(stepsOrigWKDays, aes(x=Day, y=Steps))
p2 <- g + geom_bar(stat = "Identity", aes(fill=Day)) +
    geom_text(aes(label=paste(round(Steps,0)," Steps")), position = position_dodge(width=0.9), vjust=-.5, color="black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightgrey")) +
    theme(strip.background = element_rect(fill = "lightgrey")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Steps per Day") +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Average Number of Steps") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Average Number of Steps for Weekdays Adjustmed for Missing Values") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))

png(filename = "reprores1h.png", width = 960, height = 1440)
grid.arrange(p1,p2, nrow = 2, ncol = 1)
dev.off() 
