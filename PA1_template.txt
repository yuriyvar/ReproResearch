---
title: "PA1_template.Rmd"
author: "Yuriy Varvashenya"
date: "Sunday, February 15, 2015"
output:
  html_document:
    fig_height: 5.5
    fig_width: 7.5
  pdf_document: default
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>. When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.

## Reproducible Research, Assignment 1

### Loading and preprocessing the data

The code below first loads the packages 'dplyr' and 'ggplot2' that are needed for the R code below to complete the Project Assignmen, then sets the working directory and opens a source file *'activity.csv'*. The date filed is then converted into R's Date format.


```r
library(dplyr); library(ggplot2)
```

```r
# Enter your working directory path below...
wd <- "~/Coursera/5- Reproducible Research/Projects/A1/"
setwd(wd)
activity <- tbl_df(read.csv("activity.csv", na.strings="NA", colClasses = NA, quote = "\"", 
                           stringsAsFactors=FALSE))
# Transforming the date field into the Date format
activity <- transform(activity, date = as.Date(date))
```

The following histogram represents the number of steps taken per day (*missing values were ignored for this calculation*).


```r
sum_tb <- group_by(activity, date) %>%
summarize(day_steps = sum(steps))
hist(sum_tb$day_steps, main = "Total steps taken daily", xlab = "Daily Steps", col = "green")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

The following code calculates mean and median total number of steps taken per day:

```r
initmn <- mean(sum_tb$day_steps,na.rm=TRUE)
initmd <- median(sum_tb$day_steps,na.rm=TRUE)
```

The mean and median of the total number of steps taken per day are __10766__ and __10765__ respectively.


###The average daily activity pattern
is depicted in the following time series plot of the 5-minute interval (*x-axis*) and the average number of steps taken, averaged across all days (*y-axis*)...

```r
sum_int <- na.omit(activity) %>%
  group_by(interval) %>%
  summarize(avg_steps = mean(steps))
ggplot(sum_int, aes(interval, avg_steps)) + geom_line()
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


###The average daily maximum steps per inrval

```r
maxsteps <- max(sum_int$avg_steps, na.rm=T); print(maxsteps)
```

```
## [1] 206.1698
```

```r
maxint <- sum_int[sum_int$avg_steps == maxsteps,1]; print(maxint)
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```

On average, 5-minute interval '__835__' contains __206__ steps, which is the highes number of steps per time interval across all the days in the dataset.


###Imputing missing values(coded as NA).

The missing values in the dataset initially were populated with 'NA' as shown in the example below...


```r
ex <- head(activity,5)
print(ex, type = "html")
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

```r
# Counting rows with missing values
msn <- nrow(activity[!complete.cases(activity),]); print(msn)
```

```
## [1] 2304
```

In the source dataset *"activity"*, consisting of *17568* rows and *3* columns,  there are **2304** rows with the missing values. 

The following code converts (fills) the missing values into zeros.


```r
# filling in all of the missing values in the dataset with 0s
complDT <- tbl_df(activity)
complDT$steps[(is.na(complDT$steps))] <- 0
#displaying the sample rows...
print(head(complDT,5), type = "html")
```

```
## Source: local data frame [5 x 3]
## 
##   steps       date interval
## 1     0 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
```

As seen in the latest example, the NAs are now replaced with zeros and the new dataset **"complDT"** is created with *17568* rows and *3* columns -- same as our initial dataset *"activity"*.

The new calculation of the average daily steps taken is depicted in the histogram below:


```r
#a histogram of the total number of steps taken each day
sum_tb_CM <- group_by(complDT, date) %>%
  summarize(day_steps = sum(steps)) 
hist(sum_tb_CM$day_steps, main = "Total steps taken daily", xlab = "Daily Steps", col = "green")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

The new *mean* and *median* daily steps taken are calculated as follows using the newly created dataset *"comlDT"*


```r
#mean total number of steps taken per day
clnAVG <- round(mean(sum_tb_CM$day_steps),0); print(clnAVG)
```

```
## [1] 9354
```

```r
# median total number of steps taken per day
clnMED <- round(median(sum_tb_CM$day_steps),0); print(clnMED)
```

```
## [1] 10395
```

Those values are *9354* for the new mean and *10395*  for the new median.

Now, we will calculate the impact of the missing values on the new values of mean and median total daily numbers of the steps taken...


```r
#The impact of imputing missing data on the mean of the total daily number of steps
varmn <- round(initmn - clnAVG, 0); print(varmn)
```

```
## [1] 1412
```

```r
#The impact of imputing missing data on the median of the total daily number of steps
varmd <- round(initmd - clnMED, 0); print(varmd)
```

```
## [1] 370
```

That impact is *1412* steps for the mean and *370* -- for the median.


### The differences in activity patterns between weekdays and weekends

The code below creates a new two-level factor variable  with the values "weekday" and "weekend" 


```r
# Creating a new factor variable 'wkdy' in the dataset with "weekday" values
sum_tb_dt <- tbl_df(mutate(complDT, wkdnm = weekdays(date), wkdy = "weekday"))
# Assigning value "weekend" where a given date is the weekend day
sum_tb_dt[which(sum_tb_dt$wkdnm =="Sunday" | sum_tb_dt$wkdnm == "Saturday"), c("wkdy")] <- "weekend"
# Preparing a new table for the newly created variable 'wkdy' to be used as a factor in the plot
sum_tb_dt <- transform(sum_tb_dt, wkdy = as.factor(wkdy)) %>%
  group_by(interval, wkdy) %>%
  summarize(avg_steps = mean(steps))
head(sum_tb_dt,5)
```

```
## Source: local data frame [5 x 3]
## Groups: interval
## 
##   interval    wkdy avg_steps
## 1        0 weekday 2.0222222
## 2        0 weekend 0.0000000
## 3        5 weekday 0.4000000
## 4        5 weekend 0.0000000
## 5       10 weekday 0.1555556
```

####Constructing a time-series panel plot 
of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
g <- ggplot(sum_tb_dt, aes(interval, avg_steps)) 
g + geom_line() + facet_grid(wkdy ~ .) + xlab("5-minute intervals") + ylab("AVG steps taken")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

output:
  md_document:
    variant: "markdown_github"
