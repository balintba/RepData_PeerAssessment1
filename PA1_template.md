# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
#Loading the libraries
library(ggplot2)
library(dplyr)

#Reading the CSV file
activity <- read.csv("activity.csv")

#Processing the data
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

```r
#Processing the data
  totalStep <- activity %>% group_by(date) %>% summarise(Total.Step = sum(steps, na.rm=T))
  #Calculating the mean and median
  Step.mean <- mean(totalStep$Total.Step)
  Step.median <- median(totalStep$Total.Step)
  #Plotting
  ggplot(totalStep, aes(x=Total.Step))+geom_histogram(fill="darkgreen",col="black")+theme_bw()+
    labs(title="Histogram of total steps taken each day",x="Total Step")+
    theme(plot.title = element_text(hjust = 0.5))+
    annotate("text",x=18000,y=9,label= paste("Median of steps is ",as.character(Step.median)))+
    annotate("text",x=18000,y=8,label=paste("Mean of steps is ",as.character(round(Step.mean,digits = 2))))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](assignment_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


## What is the average daily activity pattern?

```r
#Processing the data
  AveStep <- activity %>% group_by(interval) %>% summarise(Average.Step = mean(steps, na.rm=T))
  #Calculating the most active period on average
  Max.Interval <- AveStep$interval[which.max(AveStep$Average.Step)]
  #Plotting
  ggplot(AveStep, aes(x=interval,y=Average.Step))+geom_line()+theme_bw()+
    labs(title="Average activity within the day",x="Hour:Minute")+ theme(plot.title = element_text(hjust=0.5))+
    annotate("text", x=1700,y=175,label=paste("Most active interval on average is ",as.character(Max.Interval)))
```

![](assignment_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Imputing missing values

```r
  #Total number of missing values
  sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
  #Creating a new dataset
  ImpActivity <- activity
  #Imputing the missing values using the Average over all days for that interval
  for(i in 1:dim(activity)[1]) {
    if(is.na(ImpActivity$steps[i])){
      ImpActivity$steps[i] <- AveStep$Average.Step[which(AveStep$interval==ImpActivity$interval[i])]
    }
  }
  #Calculating the mean and median with the imputed data
  Imp.totalStep <- ImpActivity %>% group_by(date) %>% summarise(Total.Step = sum(steps))
  Imp.Step.Mean <- mean(Imp.totalStep$Total.Step)
  Imp.Step.Median <- median(Imp.totalStep$Total.Step)
  #Plotting
  ggplot(Imp.totalStep, aes(x=Total.Step))+geom_histogram(fill="darkblue",col="black")+theme_bw()+
    labs(title="Histogram of total steps taken each day",x="Total Step")+
    theme(plot.title = element_text(hjust=0.5))+
    annotate("text", x=18000,y=9,label=paste("Median of steps is ", as.character(Step.median)))+
    annotate("text", x=18000,y=8,label=paste("Mean of steps is ", as.character(round(Step.mean,digits=2))))+
    annotate("text", x=4000,y=7, label="Since we used the Average steps")+
    annotate("text", x=4000,y=6, label="over all days for imputing the data")+
    annotate("text", x=4000,y=5, label="the imputing does not seem to have" )+
    annotate("text", x=4000,y=4,label="an affect on the mean and median")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](assignment_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
## Are there differences in activity patterns between weekdays and weekends?

```r
#Creating a new factor variable
  ImpActivity$day <- (weekdays(ImpActivity$date) %in% c("Saturday","Sunday")) 
  
  for(i in 1:length(ImpActivity$day)){
    if(ImpActivity$day[i]) {ImpActivity$day[i] <- "Weekend"}
    else {ImpActivity$day[i] <- "Weekdays"}
  }
  
  ImpActivity$day <- as.factor(ImpActivity$day)
  
  #Processing the data to get the average step per interval and per the new day variable
  Imp.Average.Step <- ImpActivity %>% group_by(day,interval) %>% summarise(Mean.Step = mean(steps))
  
  #Plotting
  ggplot(Imp.Average.Step, aes(x=interval, y=Mean.Step))+geom_line()+facet_wrap(~day)+theme_bw()+
    labs(title="Average number of steps acc. to intervals, faceted by weekends/weekdays",x = "Interval", y="Average number of steps")+
    theme(plot.title = element_text(hjust=0.5))
```

![](assignment_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
