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

#order by number of listings
detail <- within(detail, suburb <- factor(suburb,levels=names(sort(table(suburb), 
                                                        decreasing=F))))

#plot results
ggplot(detail, aes(x = suburb))+
  geom_bar(fill=c(2:(length(unique(detail$suburb))+1)))+
  labs(x="Bairro", y="N? de listings")+
  theme(axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.2, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))
  
        