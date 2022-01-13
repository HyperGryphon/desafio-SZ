#load required packages
library(tidyverse)
library(chron)

#remove previous data
rm(list=ls())

#load files
detail <- as.data.frame(read.csv(file.choose(), encoding = "UTF-8"))
prices <- as.data.frame(read.csv(file.choose(), encoding = "UTF-8"))

#clean data
prices<-prices[which(prices$booked_on!="blank"),]
prices<-prices[which(prices$booked_on!=as.Date("2000-01-01")),]

#add a column with suburbs
prices$suburb<-detail$suburb[match(prices$airbnb_listing_id, detail$airbnb_listing_id)]

#estimate difference in days between booked and actual date
prices$booked_on<-as.Date(prices$booked_on)
prices$date<-as.Date(prices$date)
prices$diff<-abs(as.numeric(difftime(prices$booked_on,prices$date,units="days")))

#when weekends
weekend <- T #T to select only weekends; F to select only weekdays
prices<-prices[is.weekend(prices$date)==weekend,]
dim(prices[is.weekend(prices$date),])
dim(prices)

#average antecedence by listing
ant.list<-aggregate(prices$diff, 
                    list(prices$airbnb_listing_id),
                    mean)
colnames(ant.list)<-c("airbnb_listing_id","avg antecedence")

#average antecedence by suburb
ant.sub<-aggregate(prices$diff, 
                   list(prices$suburb),
                   mean)
colnames(ant.sub)<-c("suburb","avg antecedence")

head(ant.list)
cat("By listings, the antecedence in case of", 
    ifelse(weekend,"weekends","weekdays"),
    "is",round(mean(ant.list[,2]),1),"days")

head(ant.sub)
cat("By suburbs, the antecedence in case of", 
    ifelse(weekend,yes = "weekends",no = "weekdays"),
    "is",round(mean(ant.sub[,2]),1),"days")

