#load required packages
library(tidyverse)
#package vroom reads prices dataset much quicker than read.csv(), 
#consider updating in the future
#here I tested data.table::fread() function

#remove previous data
rm(list=ls())

#load files
detail <- read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/data/desafio_details.csv",
                                 encoding = "UTF-8")
prices <- data.table::fread("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/data/desafio_priceav.csv",
                                 encoding = "UTF-8")

#head(detail)
#head(prices)

#add a column with suburbs
prices$suburb<-detail$suburb[match(prices$airbnb_listing_id, detail$airbnb_listing_id)]

#number of rented listings per neighborhood
nlistings <- as.data.frame(table(prices$suburb[which(prices$occupied==0)]))
colnames(nlistings) <- c("suburb","count")

#total revenue by suburb only for rented listings
rev.sub <- aggregate(prices$price_string[which(prices$occupied==0)], 
          list(prices$suburb[which(prices$occupied==0)]),
          sum)
colnames(rev.sub) <- c("suburb","sumrev")

#estimate mean revenue by dividing total revenue by number of listings per neighborhood
rev.sub <- merge(rev.sub, nlistings, by = c("suburb")) 
rev.sub$meanrev <- rev.sub$sumrev/rev.sub$count

#plot results
png("neighborhood_by_avgrevenue.png", height = 1200, width = 1600, units = "px")
ggplot(rev.sub, aes(x = reorder(suburb, meanrev), y=meanrev))+
  geom_bar(stat = "identity", fill=c(2:(length(unique(rev.sub$meanrev))+1)))+
  labs(x="Bairro", y="Faturamento médio (R$)")+
  theme(axis.text.x = element_text(size = 40, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size = 50),
        axis.title.y = element_text(size = 50),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.4, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))
dev.off()
        