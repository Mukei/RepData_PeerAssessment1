# Reproducible Research: Peer Assessment 1


```r
opts_chunk$set(echo = TRUE, fig.width = 20, fig.height = 5)
```

## Loading and preprocessing the data

```r
# Load the csv data from the zip file 
# Assure that `date` are converted to `Date` format
activity <- read.csv(unzip("./activity.zip"), 
                     header = TRUE, 
                     stringsAsFactors = FALSE, 
                     colClasses = c("integer", "Date", "integer")
                     )
```

## What is mean total number of steps taken per day?


```r
# We will use the `library("reshape2")` to tidy the data following our needs.
require("reshape2")
```

```
## Loading required package: reshape2
```

```r
require("plyr")
```

```
## Loading required package: plyr
```

```r
require("ggplot2")
```

```
## Loading required package: ggplot2
```

```r
# Indices of each NA value
NAaway <- which(!is.na(activity$steps))

# We remove the NA value from the data set
activity2 <- activity[c(!NAIndice),]
```

```
## Error: object 'NAIndice' not found
```

```r
# We melt the `activity` data around the id `date` and `interval` with data being `steps`
# activityMolten <- melt(activity, id = c("date", "interval"), measure.vars = "steps")

# We can then compute the `sum` of variable `steps` for each `date` to get the `Total number of steps per day`
activityMoltenTidyTotalStepsPerDay <- dcast(activityMolten, 
                                            date ~ variable, 
                                            sum, drop = TRUE)
```

```
## Error: object 'activityMolten' not found
```

```r
# activityTotalStepsPerDay <- ddply(activity, .(date), summarize, total = sum(steps, na.rm = T))

# We can then compute the `mean` of variable `steps` for each `date` to get the `Average number of steps per day`
# BELOW IS OF NO USE YET ---
activityMoltenTidyAverageStepsPerDay <- dcast(activityMolten, 
                                              date ~ variable, 
                                              mean, drop = TRUE)
```

```
## Error: object 'activityMolten' not found
```

```r
# Mean value of the total number of steps per day
activityMoltenTidyTotalStepsPerDayMean <-
        mean(activityMoltenTidyTotalStepsPerDay$steps, na.rm = TRUE)
```

```
## Error: object 'activityMoltenTidyTotalStepsPerDay' not found
```

```r
# Median value of the total number of steps per day 
activityMoltenTidyTotalStepsPerDayMedian <- 
        median(activityMoltenTidyTotalStepsPerDay$steps, na.rm = TRUE)
```

```
## Error: object 'activityMoltenTidyTotalStepsPerDay' not found
```

```r
# We will use ggplot for all the graph of this report.
library("ggplot2")
# ggplot(activityMoltenTidyTotalStepsPerDay, aes(x=date)) + geom_histogram(binwidth=nrow(activityMoltenTidyTotalStepsPerDay))

ggplot(activityMoltenTidyTotalStepsPerDay, aes(x=date, y=steps)) +
        geom_bar(stat="identity") + theme_bw() +
        geom_text(aes(label=steps), vjust=2, colour="white", angle=90, size=3, hjust=1.5) +
        theme(axis.text.x = element_text(angle=90)) + 
        scale_x_date(breaks="1 day", limits=c(min(activityMoltenTidyTotalStepsPerDay$date)+3, 
                                              max(activityMoltenTidyTotalStepsPerDay$date)-3)) + 
        geom_hline(yintercept=activityMoltenTidyTotalStepsPerDayMean, colour = "red", size = 2) + 
        geom_hline(yintercept=activityMoltenTidyTotalStepsPerDayMedian)
```

```
## Error: object 'activityMoltenTidyTotalStepsPerDay' not found
```

```r
ggplot(activityMoltenTidyTotalStepsPerDay, aes(x=date, y=steps)) +
        geom_bar(stat="identity") + theme_bw() +
        geom_text(aes(label=steps), size=1, vjust=-.5) +
        theme(axis.text.x = element_text(angle=90)) + 
        scale_x_date(breaks="1 day", limits=c(min(activityMoltenTidyTotalStepsPerDay$date)+3, 
                                              max(activityMoltenTidyTotalStepsPerDay$date)-3)) + 
        geom_hline(yintercept=activityMoltenTidyTotalStepsPerDayMean, colour = "red", size = 2) + 
        geom_hline(yintercept=activityMoltenTidyTotalStepsPerDayMedian)
```

```
## Error: object 'activityMoltenTidyTotalStepsPerDay' not found
```










