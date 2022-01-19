#load required packages
library(tidyverse)

#remove previous data
rm(list=ls())

#load files
detail <- read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/data/desafio_details.csv",
                                 encoding = "UTF-8")
#prices <- data.table::fread("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/data/desafio_priceav.csv",
#                                 encoding = "UTF-8")

#head(detail)
#head(prices)

#uncomment if you want only complete rows, omitting NAs 
#detail <- detail[which(complete.cases(detail)==T),]

#order by number of listings
nlistings <- as.data.frame(sort(table(detail$suburb),decreasing=TRUE))
colnames(nlistings) <- c("suburb","count")

#plot results
png("neighborhood_by_listings.png", height = 1200, width = 1600, units = "px")
ggplot(nlistings, aes(x = reorder(suburb, count), y=count))+
  geom_bar(stat="identity",fill=c(2:(length(unique(nlistings$suburb))+1)))+
  labs(x="Bairro", y="Nº de listings")+
  theme(axis.text.x = element_text(size = 30, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.4, "cm"),
        plot.margin=unit(c(1,1,1,1),"cm"))
dev.off()
  
        