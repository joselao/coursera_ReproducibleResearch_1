# COURSERA Reproducible Research - week 1
library(ggplot2)
library(plyr)
library(reshape2)
    
    
# setting working directory
setwd("~/Documents/COURSERA_COURSES/Data Science [Especialization]/05-Reproducible Research_Rscripts/week1")


# 1) Code for reading in the dataset and/or processing the data
    # Reading data
    DATA <- read.csv("activity.csv")
    
    # converting to data class
    DATA$date <- as.Date(strptime(DATA$date,"%Y-%m-%d"))
    
    # introducing weekdays
    DATA$day <- format(DATA$date, "%u") # Weekday as a decimal number (1–7, Monday is 1)
    DATA$day2 <- as.factor(weekdays(DATA$date, abbreviate=TRUE))
    DATA$weekend <- 0
    DATA$weekend[DATA$day == 6 | DATA$day == 7] <- 1
    DATA$weekend <- factor(DATA$weekend, levels=c(0,1), labels=c("weekday","weekend"))
    
# 2) Histogram of the total number of steps taken each day
    DATA.sum.day <- ddply(DATA, .(date), summarize, steps = sum(steps, na.rm=TRUE)) # DATA statistics by day
    ggp1 <- ggplot(data=DATA.sum.day, aes(steps)) + geom_histogram(binwidth=500)
    ggp1 <- ggp1 + xlab("steps/day")
    ggp1 <- ggp1 + labs(title = "Histogram of the total number of steps taken each day")
    plot(ggp1)
    
# 3) Mean and median number of steps taken each day
    MEAN <- mean(DATA.sum.day$steps)
    MEDIAN <- median(DATA.sum.day$steps)
    ggp1 <- ggp1 + geom_vline(xintercept = MEAN, colour="blue")
    ggp1 <- ggp1 + geom_text(aes(x=MEAN, y=0, label=paste0("MEAN: ",round(MEAN,1))), colour="blue", size=5, angle=90, hjust=-1, vjust=-0.5)
    ggp1 <- ggp1 + geom_vline(xintercept = MEDIAN, colour="cyan")
    ggp1 <- ggp1 + geom_text(aes(x=MEDIAN, y=0, label=paste0("MEDIAN: ",round(MEDIAN,1))), colour="cyan", size=5, angle=90, hjust=-1, vjust=-0.5)
    plot(ggp1)
        
# 4) Time series plot of the average number of steps taken
    DATA.mean.interval <- ddply(DATA, .(interval), summarize, steps=mean(steps, na.rm=TRUE))
    ggp2 <- ggplot(data=DATA.mean.interval, aes(interval, steps)) + geom_line(size=1.5)
    ggp2 <- ggp2 + xlab("5-min interval of each day") + ylab("mean steps/5-min")
    ggp2 <- ggp2 + labs(title = "Average of 5-min steps by interval")
    plot(ggp2)

# 5) The 5-minute interval that, on average, contains the maximum number of steps
    MAX.interval <- DATA.mean.interval[which.max(DATA.mean.interval$steps), ]
    ggp2 <- ggp2 + geom_point(data=MAX.interval, aes(interval, steps), size=2, color="red")
    ggp2 <- ggp2 + geom_text(data=MAX.interval, aes(interval, steps, label=paste0("MAX:",round(MAX.interval$steps,1)," steps at interval: ",MAX.interval$interval)), size=4, color="red", hjust=-0.1)
    plot(ggp2)
    
# 6) Code to describe and show a strategy for imputing missing data
    # counting number of NA
    table(is.na(DATA$steps))

    # there are 2304 NAs, that is the 13% of the whole data:
    2304/(15264 + 2304)
    
    # casting data we will see that al the NAs are due to full day with no data tracking
    DATA.cast <- dcast(DATA, date+day+day2+weekend~interval, value.var="steps")
    
    # here we can see that all the 2304 NAs are because full day no data recording (DATA.NA subseting)
    DATA.NA <- subset(DATA.cast, is.na(DATA.cast[,5]))
    table(is.na(melt(DATA.NA, id=c("date","day","day2", "weekend"))$value))

    # so my strategy is to use impute.knn function
    #installing Impute function from Bioconductor.org
    # you need this library for doing a k-nearest-neighbors imputation of missing data 
    # source("https://bioconductor.org/biocLite.R") # try http:// if https:// URLs are not supported
    # biocLite("impute")
    library(impute)

    DATA.imputed <- as.data.frame(impute.knn(as.matrix(DATA.cast[,5:NCOL(DATA.cast)]))$data)
    DATA.imputed <- cbind(DATA.cast[,1:4], DATA.imputed)
    
    # melt the data
    DATA.imputed <- melt(DATA.imputed, id=c("date","day","day2","weekend"))
    colnames(DATA.imputed) <- c("date","day","day2","weekend","interval","steps")
    DATA.imputed <- DATA.imputed[,c("steps","date","interval","day","day2","weekend" )]
    
# 7) Histogram of the total number of steps taken each day after missing values are imputed
    DATA.sum.day.imputed <- ddply(DATA.imputed, .(date), summarize, steps = sum(steps, na.rm=TRUE)) # DATA statistics by day
    ggp3 <- ggplot(data=DATA.sum.day.imputed, aes(steps)) + geom_histogram(binwidth=500)
    ggp3 <- ggp3 + xlab("steps/day")
    ggp3 <- ggp3 + labs(title = "Histogram of the total number of steps taken each day (NA data imputed)")
    MEAN <- mean(DATA.sum.day$steps)
    MEDIAN <- median(DATA.sum.day$steps)
    ggp3 <- ggp3 + geom_vline(xintercept = MEAN, colour="blue")
    ggp3 <- ggp3 + geom_text(aes(x=MEAN, y=0, label=paste0("MEAN: ",round(MEAN,1))), colour="blue", size=5, angle=90, hjust=-1, vjust=-0.5)
    ggp3 <- ggp3 + geom_vline(xintercept = MEDIAN, colour="cyan")
    ggp3 <- ggp3 + geom_text(aes(x=MEDIAN, y=0, label=paste0("MEDIAN: ",round(MEDIAN,1))), colour="cyan", size=5, angle=90, hjust=-1, vjust=-0.5)
    plot(ggp3)
    
    
# 8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
    DATA.mean.interval <- ddply(DATA, .(interval, weekend), summarize, steps=mean(steps, na.rm=TRUE))
    ggp4 <- ggplot(data=DATA.mean.interval, aes(interval, steps, colour=weekend)) + geom_line(size=1.5) + facet_grid(weekend~.)
    ggp4 <- ggp4 + xlab("5-min interval of each day") + ylab("mean steps/5-min")
    ggp4 <- ggp4 + labs(title = "Average of 5-min steps by interval")
    plot(ggp4)


