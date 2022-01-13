#load required packages
library(tidyverse)

#remove previous data
rm(list=ls())

#load files
detail <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/desafio_details.csv",
                                 encoding = "UTF-8"))
prices <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/desafio_priceav.csv",
                                 encoding = "UTF-8"))

#head(detail)
#head(prices)

#add a column with suburbs
prices$suburb<-detail$suburb[match(prices$airbnb_listing_id, detail$airbnb_listing_id)]

#average revenue by suburb
rev.sub<-aggregate(prices$price_string[which(prices$occupied==0)], 
          list(prices$suburb[which(prices$occupied==0)]),
          mean)
colnames(rev.sub) <- c("suburb","meanrev")

#plot results
ggplot(rev.sub, aes(x = reorder(suburb, meanrev), y=meanrev))+
  geom_bar(stat = "identity", fill=c(2:(length(unique(rev.sub$meanrev))+1)))+
  labs(x="Bairro", y="Faturamento médio (R$)")+
  theme(axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.2, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))
  
        