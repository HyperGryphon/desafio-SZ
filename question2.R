#load required packages
library(tidyverse)

#remove previous data
rm(list=ls())

#load files
detail <- as.data.frame(read.csv(file.choose(), encoding = "UTF-8"))
prices <- as.data.frame(read.csv(file.choose(), encoding = "UTF-8"))

#head(detail)
#head(prices)

#add a column with suburbs
prices$suburb<-detail$suburb[match(prices$airbnb_listing_id, detail$airbnb_listing_id)]

#average revenue by suburb
rev.sub<-aggregate(prices$price_string[which(prices$occupied==0)], 
          list(prices$suburb[which(prices$occupied==0)]),
          mean)

#average revenue by listing
rev.list<-aggregate(prices$price_string[which(prices$occupied==0)], 
          list(prices$airbnb_listing_id[which(prices$occupied==0)]),
          mean)
colnames(rev.list)<-c("airbnb_listing_id","price_string")
rev.list$suburb<-detail$suburb[match(rev.list$airbnb_listing_id, detail$airbnb_listing_id)]
#sum by suburb average revenue by listing
rev.sub.avg<-aggregate(rev.list$price_string, 
                       list(rev.list$suburb),
                       sum)
colnames(rev.sub.avg)<-c( "suburb","price_string")
rev.sub.avg <- within(rev.sub.avg, suburb <- factor(suburb,levels=names(sort(table(suburb), 
                                                        decreasing=F))))

#plot results
ggplot(rev.sub.avg, aes(x = reorder(suburb, price_string), y=price_string))+
  geom_bar(stat = "identity", fill=c(2:(length(unique(rev.sub.avg$price_string))+1)))+
  labs(x="Bairro", y="Faturamento médio (R$)")+
  theme(axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.2, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))
  
        