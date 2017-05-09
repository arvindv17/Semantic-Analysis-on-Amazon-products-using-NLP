install.packages("rvest")
install.packages("NLP")
install.packages("Rstem")
install.packages("quanteda")

#Getting text
library(rvest)

basehtml <- "https://www.amazon.com/ASUS-PB278Q-2560x1440-DisplayPort-Monitor/product-reviews/B009C3M7H0/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=avp_only_reviews&showViewpoints=1&sortBy=helpful&pageNumber="
amazonReview <- read_html(paste(basehtml,1,sep = ""))
review <- amazonReview %>%
              html_nodes(".review-text") %>%
              html_text()

for(level in c(2:173)){
  amazonReview <- read_html(paste(basehtml,level,sep = ""))
  review <- c(review,amazonReview %>%
    html_nodes(".review-text") %>%
    html_text())
}

review <- as.data.frame(review)

#Sentiment Analisez
library(NLP)
library(Rstem)
require(quanteda)

positive <- read.csv(file = "~/Dropbox/CSYE7245/Lec9/positive-words.csv", header = F, stringsAsFactors=FALSE)
negative <- read.csv(file = "~/Dropbox/CSYE7245/Lec9/negative-words.csv", header = F, stringsAsFactors=FALSE)
#positive <- read.csv(file = "C:\\Users\\Junhao Li\\Dropbox\\CSYE7245\\Lec9\\positive-words.csv", header = F, stringsAsFactors=FALSE)
#negative <- read.csv(file = "C:\\Users\\Junhao Li\\Dropbox\\CSYE7245\\Lec9\\negative-words.csv", header = F, stringsAsFactors=FALSE)

stem <- function(list){
  for(i in c(1:nrow(list))){
    list[i,1] <- wordStem(String(list[i,1]))
  }
  list <- unique(list[,1] )
}

positive <- stem(positive)
negative <- stem(negative)

for(level in c(1:nrow(review))){
  str <- String(review$review[level])
  tokens <- str[wordpunct_tokenizer(str)]
  stem <- wordStem(tokens)
  positiveCount <- length(intersect(stem,positive))
  negativeCount <- length(intersect(stem,negative))
  review$PositiveWordsCount[level] <- positiveCount
  review$NegativeWordsCount[level] <- negativeCount
  if((positiveCount + negativeCount) == 0){
    positiveCount <- 1 
    negativeCount <- 1
  }
  review$Sentiment_Positive[level] <- positiveCount / (positiveCount + negativeCount) 
  review$Sentiment_Negative[level] <- negativeCount / (positiveCount + negativeCount)
  if(positiveCount >= negativeCount){
    review$result[level] <- "Positive"
  } else {
    review$result[level] <- "Negative"
  }
}

write.csv(review, file = "~/Dropbox/CSYE7245/Final_Project/AmazonReview.csv")

sum(review$result == "Negative")
sum(review$result == "Positive")





#Vectorlize
install.packages("Doc2Vec")








###############################################################
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

lego_movie %>% 
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()

lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

lego_movie %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()