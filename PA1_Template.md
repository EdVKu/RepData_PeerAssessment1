Introduction
============

Reading the dataset
===================

<font size="5">First, a “read data” script is added to obtain the file
that will be read.</font>

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
  if(!file.exists("./data")){
    
    dir.create("./data")

    unzip(zipfile ="./data/repdata_data_activity.zip",exdir = "./data")
    # The script automatically looks for and validates if the selected directory for the file is in the computer. If it is not, it's added, along with the file
    
  }
  
path1 <- file.path("./data", "activity.csv")
act <- read.csv(path1)
```

<font size="5">This script makes sure there’s a “data” directory path
and, if not, it creates one, while unzipping the already downloaded
file. Make sure to check that the file and the data are both in the same
directory path.</font>

Obtaining the quantitative data of the number of steps taken each day, and presenting it as a histogram
=======================================================================================================

<font size="5">First off, a snapshot and a summary of the obtained data
structure is done.</font>

``` r
head(act)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
summary(act)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

<font size="5">We’ll check if the “date” section of the data frame is in
an actual POSTIX date format</font>

``` r
class(act$date)
```

    ## [1] "factor"

<font size="5">We’ll soon realize this is not the case, so we’ll rectify
that.</font>

``` r
act$date <- as.POSIXct(act$date)
```

<font size="5">We’ll now rectify that the class of “date” is in a POSIX
date format.</font>

``` r
class(act$date)
```

    ## [1] "POSIXct" "POSIXt"

<font size="5">The number of steps taken per day can now be
inicialized.</font>

<font size="5">First, a data frame that uses the dates as a grouping
method will be created using the “date” and “steps” subsets. The gouping
will also be done here</font>

``` r
actDatSte2 <- with(act, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))

actDatSte2 <- actDatSte2 %>% rename(steps = x, date = Group.1)
```

<font size = "5">Finally, the histogram is obtained, giving the reader
the required information without considering NAs</font>

``` r
hist(actDatSte2$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "red", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_Template_files/figure-markdown_github/hist_steps-1.png)

Obtention of mean and median of daily steps
===========================================

<font size = "5">Once the filtered data set is obtained we just need to
use the mean and median for the “steps” dataset</font>

``` r
mean(actDatSte2$steps)
```

    ## [1] 9354.23

<font size = "5">Same goes for the median.</font>

``` r
median(actDatSte2$steps)
```

    ## [1] 10395

Obtention of the 5 minute interval containing the maximum number of steps (on average)
======================================================================================

``` r
actInt <- with(act, aggregate(steps, by = list(interval), FUN = mean, na.rm = TRUE))
actInt <- actInt %>% rename(steps = x, interval = Group.1)
actInt[actInt$steps==max(actInt$steps),][1]
```

    ##     interval
    ## 104      835

<font size = "5">The plot of the average number of steps per five minute
interval will be done in order to illustrate the aforementioned
result.</font>

``` r
plot(actInt$interval,actInt$steps, type = "l",col = "red", xlab = "time (minutes)", ylab = "number of steps on average", main = "Average steps per 5-minute intervals")
```

![](PA1_Template_files/figure-markdown_github/plot_5min-1.png)

Strategy used to imput missing data
===================================

``` r
avgStep <- actInt$steps[match(act$interval,actInt$interval)]
actImp <- transform(act, steps = ifelse(is.na(act$steps), yes = avgStep, no = act$steps))
```

Corrected histogram present in previous steps
=============================================

``` r
actDatSte3 <- with(actImp, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))

actDatSte3 <- actDatSte3 %>% rename(steps = x, date = Group.1)

hist(actDatSte3$steps,main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "red", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_Template_files/figure-markdown_github/hist_steps_2-1.png)

Plotting the average number of steps taken per 5-minute intervals on weekdays and weekends
==========================================================================================

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.6.3

``` r
act2 <- act

act2$wday <- weekdays(act$date)
act2$na.rm <- NULL

act2$wed <- (act2$wday == "Saturday" | act2$wday == "Sunday")
act2 <- act2 %>% mutate(wed = factor(wed, labels = c("weekday","weekend")))
actI2 <- aggregate(steps~interval + wed, act2, mean, na.rm = TRUE)

ggplot(actI2, aes(x = (interval), y = steps, color = wed)) + 
  geom_line() +  facet_grid(wed~.,scales="free")
```

![](PA1_Template_files/figure-markdown_github/steps_intervals_wdays-1.png)
