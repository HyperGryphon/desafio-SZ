#load required packages
library(tidyverse)

#remove previous data
rm(list=ls())

#load files
detail <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/desafio_details.csv",
                                 encoding = "UTF-8"))
#prices <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/desafio_priceav.csv",
#                                 encoding = "UTF-8"))

#head(detail)
#head(prices)

#uncomment if you want only complete rows, omitting NAs 
#detail <- detail[which(complete.cases(detail)==T),]

#order by number of listings
detail <- as.data.frame(sort(table(detail$suburb),decreasing=TRUE))
colnames(detail) <- c("suburb","count")

#plot results
ggplot(detail, aes(x = reorder(suburb, count), y=count))+
  geom_bar(stat="identity",fill=c(2:(length(unique(detail$suburb))+1)))+
  labs(x="Bairro", y="Nº de listings")+
  theme(axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.2, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))
  
        