#load required packages
library(tidyverse)
library(chron)

#remove previous data
rm(list=ls())

#load files
detail <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/desafio_details.csv",
                                 encoding = "UTF-8"))
prices <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/desafio_priceav.csv",
                                 encoding = "UTF-8"))

#clean data
prices<-prices[which(prices$booked_on!="blank"),]
prices<-prices[which(prices$booked_on!=as.Date("2000-01-01")),]

#add a column with suburbs
prices$suburb<-detail$suburb[match(prices$airbnb_listing_id, detail$airbnb_listing_id)]

#estimate difference in days between booked and actual date
prices$booked_on<-as.Date(prices$booked_on)
prices$date<-as.Date(prices$date)
prices$diff<-abs(as.numeric(difftime(prices$booked_on,prices$date,units="days")))

#divide between weekends and weekdays
prices.t<-prices[is.weekend(prices$date)==T,]

#dim(prices[is.weekend(prices$date),])
#dim(prices)

#average antecedence by listing
ant.list.t<-aggregate(prices.t$diff,
                    list(prices.t$airbnb_listing_id),
                    mean)
colnames(ant.list.t)<-c("airbnb_listing_id","avg antecedence")

#average antecedence by suburb
ant.sub.t<-aggregate(prices.t$diff, 
                   list(prices.t$suburb),
                   mean)
colnames(ant.sub.t)<-c("suburb","avg antecedence")

#head(ant.sub.t)
#head(ant.list.t)

prices.f<-prices[is.weekend(prices$date)==F,]

#average antecedence by listing
ant.list.f<-aggregate(prices.f$diff,
                      list(prices.f$airbnb_listing_id),
                      mean)
colnames(ant.list.f)<-c("airbnb_listing_id","avg antecedence")

#average antecedence by suburb
ant.sub.f<-aggregate(prices.f$diff, 
                     list(prices.f$suburb),
                     mean)
colnames(ant.sub.f)<-c("suburb","avg antecedence")

#head(ant.sub.f)
#head(ant.list.f)

cat("By listings, the antecedence in case of weekends is",
    round(mean(ant.list.t[which(ant.list.t[,2]<=median(ant.list.t[,2])),2]),1),"days")

cat("By suburbs, the antecedence in case of weekends is",
    round(mean(ant.sub.t[,2]),1),"days")

cat("By listings, the antecedence in case of weekdays is",
    round(mean(ant.list.f[which(ant.list.f[,2]<=median(ant.list.f[,2])),2]),1),"days")

cat("By suburbs, the antecedence in case of weekdays is",
    round(mean(ant.sub.f[,2]),1),"days")

#plot by listings
par(mar=c(5,5,1.5,1.5))
plot(density(ant.list.t[,2]), main="", col = 1, #log = "x",
     ylab = "", xlab = "Dias de antecedência", lwd = 2, cex.axis=1.2,cex.lab=1.5)
lines(density(ant.list.f[,2]), main="", col = 2, lwd = 2, cex.axis=1.2,cex.lab=1.5)
legend("topright",legend=c("Fins de semana", "Dias normais"), 
       lwd = 2, col = c(1,2), lty = 1, bty = "n", cex = 2)
  
#plot by suburbs
ggplot(ant.sub.t, aes(x=reorder(suburb, `avg antecedence`), y = `avg antecedence`))+
  geom_bar(stat = "identity",fill=c(2:(length(unique(ant.sub.t$suburb))+1)))+
  labs(x="Bairro", y="Dias de antecedência em fins de semana")+
  theme(axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.2, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))
ggplot(ant.sub.f, aes(x=reorder(suburb, `avg antecedence`), y = `avg antecedence`))+
  geom_bar(stat = "identity",fill=c(2:(length(unique(ant.sub.f$suburb))+1)))+
  labs(x="Bairro", y="Dias de antecedência em fins de semana")+
  theme(axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.2, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))
