#load required packages
library(tidyverse)

#remove previous data
rm(list=ls())
graphics.off()

#load files
detail <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/data/desafio_details.csv",
                                 encoding = "UTF-8"))
prices <- as.data.frame(read.csv("https://raw.githubusercontent.com/HyperGryphon/desafio-SZ/main/data/desafio_priceav.csv",
                                 encoding = "UTF-8"))

#head(detail)
#head(prices)

#summed revenue by listing
rev.list<-aggregate(prices$price_string[which(prices$occupied==0)],
                    list(prices$airbnb_listing_id[which(prices$occupied==0)]),
                    sum)
colnames(rev.list)<-c("airbnb_listing_id","summed")

#average price by listing
avg.price<-aggregate(prices$price_string[which(prices$occupied==0)], 
                    list(prices$airbnb_listing_id[which(prices$occupied==0)]),
                    mean)
colnames(rev.list)<-c("airbnb_listing_id","mean")

#only select apartments that are in the prices dataset
detail.sel <- semi_join(detail,rev.list,by="airbnb_listing_id")
dim(detail.sel)

#averages for correlation
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
colnames(data) <- c("airbnb_listing_id","total revenue","avg price","nº rooms","nº baths","stars","reviews")

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

head(df)

#uncomment to plot the wordcloud
#set.seed(1234) # for reproducibility 
#wordcloud2(data=df, size=1.6, color='random-dark')

cat(sep = "",
    "Most repeated words are ",
    df$word[1],", ",df$word[2],", and ",df$word[3])

#choose word
w <- "centro"

#retrieve detail including word "praia" and "mar"
detail.inc <- detail[which(str_detect(detail$ad_name, w)==T & match(detail$airbnb_listing_id,rev.list$airbnb_listing_id)),]
detail.not <- detail[which(str_detect(detail$ad_name, w)==F & match(detail$airbnb_listing_id,rev.list$airbnb_listing_id)),]

#let's see if these listings's revenues are higher
rev.list.inc <- na.omit(rev.list[match(detail.inc$airbnb_listing_id,rev.list$airbnb_listing_id),])
rev.list.not <- na.omit(rev.list[match(rev.list$airbnb_listing_id,detail.not$airbnb_listing_id),])

cat(sep = "", "Anuncios que incluem a palavra '", w, "' ganham ",
    round(mean(rev.list.inc[,2])/(mean(rev.list.not[,2]))*100,1)-100,
    "% daqueles que não a incluem")

