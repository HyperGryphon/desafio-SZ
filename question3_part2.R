#load required packages
library(wordcloud)
library(wordcloud2)
library(tm)

#following from question3.R
#Let's do some text mining to see which words appear most

text <- detail$ad_name
docs <- Corpus(VectorSource(text))

#gsub("https\\S*", "", text) 
#gsub("@\\S*", "", text) 
#gsub("amp", "", text) 
#gsub("[\r\n]", "", text)
#gsub("[[:punct:]]", "", text)

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords)

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
