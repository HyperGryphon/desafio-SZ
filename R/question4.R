#load required packages
library(tidyverse); library(chron); library(patchwork)

#remove previous data
rm(list=ls())

#load files
detail <- read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/data/desafio_details.csv",
                                 encoding = "UTF-8")
prices <- data.table::fread("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/data/desafio_priceav.csv",
                                 encoding = "UTF-8")

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
#t(true): weekends, f(false): weekdays
prices.t<-prices[is.weekend(prices$date)==T,]
prices.f<-prices[is.weekend(prices$date)==F,]

#average antecedence by suburb
ant.sub.t<-aggregate(prices.t$diff, 
                   list(prices.t$suburb),
                   median)
colnames(ant.sub.t)<-c("suburb","avg antecedence")

#average antecedence by suburb
ant.sub.f<-aggregate(prices.f$diff, 
                     list(prices.f$suburb),
                     median)
colnames(ant.sub.f)<-c("suburb","avg antecedence")

#head(ant.sub.f)

cat("Median of the antecedence in case of weekends is",
    round(median(ant.sub.t[,2]),1),"days")

cat("Median of the antecedence in case of weekdays is",
    round(median(ant.sub.f[,2]),1),"days")

#plot distributions
png("kde_book_advance.png", height = 800, width = 1200, units = "px")
par(mar=c(5,5,1.5,1.5))
plot(density(prices.f$diff), main="", col = 1, xlim = c(-5,60),#log = "x",
     ylab = "", xlab = "Dias de antecedência", lwd = 2, cex.axis=2.4,cex.lab=3)
abline(v=median((prices.f$diff)), lwd=2, lty = 2, col = 1)
lines(density(prices.t$diff), main="", col = 2, lwd = 2, cex.axis=2.4,cex.lab=3)
abline(v=median((prices.t$diff)), lwd=2, lty = 2, col = 2)
legend("topright",legend=c(paste("Mediana para dias úteis:", median(prices.f$diff), "dias"), 
                           paste("Mediana para finais de semana:", median(prices.t$diff), "dias")), 
       lwd = 2, col = c(1,2), lty = 2, bty = "n", cex = 3)
dev.off()
  
#plot by suburbs
g1<-ggplot(ant.sub.t, aes(x=reorder(suburb, `avg antecedence`), y = `avg antecedence`))+
  geom_bar(stat = "identity",fill=c(2:(length(unique(ant.sub.t$suburb))+1)))+
  labs(x="", y="Dias")+
  geom_text(x=1, y=10, label="Finais de semana", size = 20, hjust = 0)+
  theme(axis.text.x = element_blank(),# text(size = 15, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size = 50),
        axis.title.y = element_text(size = 50),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.4, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))
g2<-ggplot(ant.sub.f, aes(x=reorder(suburb, `avg antecedence`), y = `avg antecedence`))+
  geom_bar(stat = "identity",fill=c(2:(length(unique(ant.sub.f$suburb))+1)))+
  labs(x="Bairro", y="Dias")+
  geom_text(x=1, y=10, label="Dias úteis", size = 20, hjust = 0)+
  theme(axis.text.x = element_text(size = 40, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size = 50),
        axis.title.y = element_text(size = 50),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.4, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))

png("book_advance.png", height = 2000, width = 1600, units = "px")
g1+g2+plot_layout(ncol=1)+plot_annotation(
  title = 'Antecedência em dias')&
  theme(plot.title = element_text(size = 60, hjust = 0.5))
dev.off()
