#load required packages
library(tidyverse)

#remove previous data
rm(list=ls())
graphics.off()

#load files
detail <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/desafio_details.csv",
                                 encoding = "UTF-8"))
prices <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/desafio_priceav.csv",
                                 encoding = "UTF-8"))

#head(detail)
#head(prices)

#average revenue by listing
rev.list<-aggregate(prices$price_string[which(prices$occupied==0)], 
                    list(prices$airbnb_listing_id[which(prices$occupied==0)]),
                    sum)
colnames(rev.list)<-c("airbnb_listing_id","summed")

#average price by listing
avg.price<-aggregate(prices$price_string[which(prices$occupied==0)], 
                    list(prices$airbnb_listing_id[which(prices$occupied==0)]),
                    mean)

#only select apartments that are in the prices csv
detail.sel <- semi_join(detail,rev.list,by="airbnb_listing_id")
dim(detail.sel)

#averages
avg.rooms<-aggregate(detail.sel$number_of_bedrooms, 
                     list(detail.sel$airbnb_listing_id),
                     mean)
avg.baths<-aggregate(detail.sel$number_of_bathrooms, 
                     list(detail.sel$airbnb_listing_id),
                     mean)
avg.stars<-aggregate(detail.sel$star_rating, 
                     list(detail.sel$airbnb_listing_id),
                     mean)
avg.reviews<-aggregate(detail.sel$number_of_reviews, 
                     list(detail.sel$airbnb_listing_id),
                     mean)

data <- data.frame(rev.list, avg.price[,2], avg.rooms[,2],avg.baths[,2],avg.stars[,2],avg.reviews[,2])
colnames(data) <- c("airbnb_listing_id","revenue","price","rooms","baths","stars","reviews")

comp.data <- data[which(complete.cases(data)==T),]

library(corrplot)

#pdf("corrplot.pdf", height = 10, width = 12)
#par(mar = c(5,5,1,1))
corrplot(cor(comp.data[,-1])[1,1:6,drop=F], 
         type = "upper", #order = "hclust", 
         tl.col = "black", tl.srt = 45, 
         diag = F, addCoef.col = "white")
#dev.off()

#Let's do some text mining to see which words appear most
#load required packages
library(wordcloud)
library(wordcloud2)
library(tm)

text <- detail$ad_name
docs <- Corpus(VectorSource(text),
               readerControl = list(reader=readPlain,
                                    language="pt"))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english")) #where are the portuguese stopwords?

dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
#select words we don't want to be included
df <- df[!(row.names(df) %in% 
             c("apartamento","para","com","nos","apto","dos","metros")),]

#plot the wordcloud
set.seed(1234) # for reproducibility 
wordcloud2(data=df, size=1.6, color='random-dark')

cat(sep = "",
    "Most repeated words are ",
    df$word[1],", ",df$word[2],", and ",df$word[3])

head(df)

#retrieve detail including word "praia" and "mar"
detail.praia <- detail[which(str_detect(detail$ad_name, "praia")==T & match(detail$airbnb_listing_id,rev.list$airbnb_listing_id)),]
detail.nopraia <- detail[which(str_detect(detail$ad_name, "praia")==F & match(detail$airbnb_listing_id,rev.list$airbnb_listing_id)),]
detail.mar <- detail[which(str_detect(detail$ad_name, "mar")==T & match(detail$airbnb_listing_id,rev.list$airbnb_listing_id)),]
detail.nomar <- detail[which(str_detect(detail$ad_name, "mar")==F & match(detail$airbnb_listing_id,rev.list$airbnb_listing_id)),]

#let's see if these listings's revenue is higher
rev.list.praia <- na.omit(rev.list[match(detail.praia$airbnb_listing_id,rev.list$airbnb_listing_id),])
rev.list.nopraia <- na.omit(rev.list[match(rev.list$airbnb_listing_id,detail.nopraia$airbnb_listing_id),])
rev.list.mar <- na.omit(rev.list[match(rev.list$airbnb_listing_id,detail.mar$airbnb_listing_id),])
rev.list.nomar <- na.omit(rev.list[match(rev.list$airbnb_listing_id,detail.nomar$airbnb_listing_id),])

cat("Listings including word 'praia' make ",
    round(sum(rev.list.praia[,2])/sum(rev.list.nopraia[,2])*100,1),
    "% more revenue")
cat("Listings including word 'mar' make ",
    round(sum(rev.list.mar[,2])/sum(rev.list.nomar[,2])*100,1),
    "% more revenue")
