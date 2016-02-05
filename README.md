## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Assignment

This assignment will be described in multiple parts. You will need to
write a report that answers the questions detailed below. Ultimately,
you will need to complete the entire assignment in a **single R
markdown** document that can be processed by **knitr** and be
transformed into an HTML file.

Throughout your report make sure you always include the code that you
used to generate the output you present. When writing code chunks in
the R markdown document, always use `echo = TRUE` so that someone else
will be able to read the code. **This assignment will be evaluated via
peer assessment so it is essential that your peer evaluators be able
to review the code for your analysis**.

For the plotting aspects of this assignment, feel free to use any
plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the [GitHub repository created for this
assignment](http://github.com/rdpeng/RepData_PeerAssessment1). You
will submit this assignment by pushing your completed files into your
forked repository on GitHub. The assignment submission will consist of
the URL to your GitHub repository and the SHA-1 commit ID for your
repository state.

NOTE: The GitHub repository also contains the dataset for the
assignment so you do not have to download the data separately.



### Loading and preprocessing the data

We will be using ggplot2 to plot data and read.csv to load the data during this exercise.


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




### What is mean total number of steps taken per day?

Looking at the available data there are missing values that are ignored for now. The code below shows 5 categories in which the days can be group into.

Note: Plots do not contain any missing values. They have all been filtered out prior to the plot command.


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
    # Plot the historgram of the step groups.
    par(mar=c(10,10,10,10))
    hist(stepsDay$Steps, 
         xlab = "Total Number of Steps in Groups
         Example: 5 days with less than 5000 steps", 
         ylab = "Days with Step Ranges", 
         main = "Number of Steps per Day in Groups",
         col = "lightblue")
    
    ![alt text](https://github.com/bjoernsteffens/RepData_PeerAssessment1/blob/master/reprores1a.png)
    
    #
    # Plot it on the X axis, dont plot legend and turn the x-labels
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
        theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
        ylab("Number of Recorded Step") + 
        theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
        ggtitle("Number of Steps Recorded per Day") +
        theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
        
    ![alt text](https://github.com/bjoernsteffens/RepData_PeerAssessment1/blob/master/reprores1b.png)



## What is the average daily activity pattern?


If we take the average of the steps in each interval across the entire data set we see that the interval in between ~800 and ~900 in the morning seems a likely candidate for the most active period during the day.


    #
    # Lets grab the average values for the intervals first
    # and fix the column names
    stepsAvgInt <- as.data.frame(tapply(steps$steps,steps$interval, mean))
    stepsAvgInt$Interval <- row.names(stepsAvgInt)
    colnames(stepsAvgInt) <- c("Steps","Interval")
    
    #
    # Ensure geom_bar does not try and sort the x Axis
    stepsAvgInt$Interval <- factor(stepsAvgInt$Interval, levels = stepsAvgInt$Interval)
    
    g <- ggplot(stepsAvgInt, aes(x=Interval, y=Steps, fill = 20))
    g + geom_bar(stat = "Identity", alpha = 0.9) + geom_hline(yintercept=100, col = "red") +
        theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
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
        
        ![alt text](https://github.com/bjoernsteffens/RepData_PeerAssessment1/blob/master/reprores1c.png)


Looking closer at the data in between 0800 and 0900 we see that there are values below 100 steps so I should take a deeper look into that later to get a better specification of the interval.

    #
    # visual inspection gives us the time interval between 0800 and 0900 
    # in the morning seems to be the most active ones.
    stepsMaxInt <- stepsAvgInt[with(stepsAvgInt, stepsAvgInt$Interval %in% seq(800,855,5)),]
    
    #
    # lets plot that
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
        theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
        ylab("Number of Steps per Interval") + 
        theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
        ggtitle("Steps per Interval between 800 and 855") +
        theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
        

![alt text](https://github.com/bjoernsteffens/RepData_PeerAssessment1/blob/master/reprores1d.png)
    



We learn that simply taking the daily average across the entire data set will be a bad strategy for imputing mussing data. We also see that depening on what time during the day and also the day of week.

Let's investigate that in more detail and see if we can learn more for implementing an impute strategy


    #
    # Lets split the stuff out per day of week
    
    #
    # Average steps per weekday, day
    stepsAvgWday <- arrange(stepsDay %>% group_by(WeekDay,Day) %>% summarise(mean(Steps)),WeekDay,Day)
    colnames(stepsAvgWday) <- c("WeekDay","Day","Steps")
    #
    
    g <- ggplot(stepsAvgWday, aes(x=Day, y=Steps))
    p1 <- g + geom_bar(stat = "Identity", aes(fill=WeekDay)) +
        facet_grid(.~WeekDay) 
    
    #
    # Compare this with the number of steps per interval and day
    steps$WeekDay <- weekdays(as.Date(steps$date))
    stepsAvgDayInt <- arrange(steps %>% group_by(WeekDay,interval) %>% summarise(mean(steps)),WeekDay,interval)
    colnames(stepsAvgDayInt) <- c("WeekDay","Interval","Steps")
    
    g <- ggplot(stepsAvgDayInt, aes(x=Interval, y=Steps))
    p2 <- g + geom_bar(stat = "Identity", aes(fill=WeekDay)) +
        facet_grid(.~WeekDay) 
    
    grid.arrange(p1,p2, nrow = 2, ncol = 1, top = "Daily Steps Profile vs Interval Steps Profile")
    
![alt text](https://github.com/bjoernsteffens/RepData_PeerAssessment1/blob/master/reprores1e.png)



New information from these plots are that Fridays and Thursdays have higher average number of steps. We also see that during the night we have considerably less steps and may need to consider assigning a very low value between 12:00am and 06:00am for missing values.



### Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

![Sample panel plot](instructions_fig/sample_panelplot.png) 


**Your plot will look different from the one above** because you will
be using the activity monitor data. Note that the above plot was made
using the lattice system but you can make the same version of the plot
using any plotting system you choose.


## Submitting the Assignment

To submit the assignment:

1. Commit your completed `PA1_template.Rmd` file to the `master` branch of your git repository (you should already be on the `master` branch unless you created new ones)

2. Commit your `PA1_template.md` and `PA1_template.html` files produced by processing your R markdown file with the `knit2html()` function in R (from the **knitr** package)

3. If your document has figures included (it should) then they should have been placed in the `figure/` directory by default (unless you overrode the default). Add and commit the `figure/` directory to your git repository.

4. Push your `master` branch to GitHub.

5. Submit the URL to your GitHub repository for this assignment on the course web site.

In addition to submitting the URL for your GitHub repository, you will
need to submit the 40 character SHA-1 hash (as string of numbers from
0-9 and letters from a-f) that identifies the repository commit that
contains the version of the files you want to submit. You can do this
in GitHub by doing the following:

1. Go into your GitHub repository web page for this assignment

2. Click on the "?? commits" link where ?? is the number of commits you have in the repository. For example, if you made a total of 10 commits to this repository, the link should say "10 commits".

3. You will see a list of commits that you have made to this repository. The most recent commit is at the very top. If this represents the version of the files you want to submit, then just click the "copy to clipboard" button on the right hand side that should appear when you hover over the SHA-1 hash. Paste this SHA-1 hash into the course web site when you submit your assignment. If you don't want to use the most recent commit, then go down and find the commit you want and copy the SHA-1 hash.

A valid submission will look something like (this is just an **example**!)

```r
https://github.com/rdpeng/RepData_PeerAssessment1

7c376cc5447f11537f8740af8e07d6facc3d9645
```
