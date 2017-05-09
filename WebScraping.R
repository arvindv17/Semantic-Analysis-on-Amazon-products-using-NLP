install.packages("rvest")
install.packages("NLP")
install.packages("Rstem")
install.packages("quanteda")

setwd("E://Boston//NEU//Fall 2016//Big Data Analytics//Assignments//Final Project")
#Getting text
library(rvest)
library(curl)
url <- "https://www.amazon.com/Amazon-Kindle-Paperwhite-6-Inch-4GB-eReader/product-reviews/B00OQVZDJM/ref=cm_cr_dp_see_all_btm?ie=UTF8&reviewerType=avp_only_reviews&showViewpoints=1&sortBy=recent"
data <- read_html(paste(url,1,sep = ""))
review <- data %>%
  html_nodes(".review-text") %>%
  html_text()

for(level in c(2:173)){
  data <- read_html(paste(url,level,sep = ""))
  review <- c(review,data %>%
                html_nodes(".review-text") %>%
                html_text())
}
review <- as.data.frame(review)


#Sentiment Analisez
library(NLP)
library(Rstem)
require(quanteda)

positive <- read.csv(file = "positive_words.txt", header = F, stringsAsFactors=FALSE)
negative <- read.csv(file = "negative_words.txt", header = F, stringsAsFactors=FALSE)


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


write.csv(review, file = "AmazonReviews.csv")


library(tm)
review_corpus<-Corpus(VectorSource(review[1]))
review_body=Corpus(VectorSource(review))

review_body=tm_map(review_body,content_transformer(tolower))
inspect(review_body)

review = gsub("[[:punct:]]", "", review)
review_corpus = gsub("[[:digit:]]", "", review_corpus)
review_corpus = gsub("http\\w+", "", review_corpus)
review_corpus = gsub("[ \t]{2,}", "", review_corpus)
review_corpus = gsub("^\\s+|\\s+$", "", review_corpus)
review_corpus <- gsub('[[:punct:]]', '', review_corpus)
review_corpus <- gsub('[[:cntrl:]]', '', review_corpus)
review_corpus <- gsub('\\d+', '', review_corpus)

try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

review_corpus = sapply(review_corpus, try.error)
class(review_corpus)
review_corpus = review_corpus[!is.na(review_corpus)]
names(review_corpus) = NULL

termDocumentMatrix1 <- TermDocumentMatrix(review_body, control=list(stemDocument=TRUE))
as.matrix(termDocumentMatrix1)
termDocumentMatrix2 <- TermDocumentMatrix(review_body, control=list(stemming=TRUE))
as.matrix(termDocumentMatrix2)


#####WORD2VEC
library("text2vec")
tokens <- space_tokenizer(review_corpus)
it = itoken(tokens,progressbar = FALSE)
vocab <- create_vocabulary(it)

vocab <- prune_vocabulary(vocab, term_count_min = 5L)

vectorizer <- vocab_vectorizer(vocab, 
                               # don't vectorize input
                               grow_dtm = FALSE, 
                               # use window of 5 for context words
                               skip_grams_window = 5L)
tcm <- create_tcm(it, vectorizer)
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
fit(tcm, glove, n_iter = 20)
word_vectors <- glove$get_word_vectors()
class(word_vectors)
data_frame_word_vectors <- as.data.frame(word_vectors)
dim(word_vectors)

write.csv(word_vectors,file = "wordVectors.csv")


###TEXT2VEC

text_input <- read.csv("AmazonReviews.csv", header = T)
colnames(text_input)
text_input$Review_char <- as.character(text_input$review)
library(text2vec)
nrow(text_input)
make_lower_fun <- tolower
tokenize_function <- word_tokenizer
train_tokens = text_input$review %>%
  make_lower_fun %>%
  tokenize_function

it_train = itoken(train_tokens, 
                  ids = text_input$X,
                  progressbar = FALSE)

vocab = create_vocabulary(it_train)
str(vocab)
# 
vocab <- prune_vocabulary(vocab, term_count_min = 10)
# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab, 
                               grow_dtm = T )

dtm_train  = create_dtm(it_train, vectorizer)
new_model <- TfIdf$new()
text2vec <- fit_transform(dtm_train,new_model)


text_input$result <- as.numeric(factor(text_input$result,levels = "Positive","Negative"))-1
stemmed_result <- cbind(text2vec,text_input$result)
class(stemmed_result)
#build the deep belief network
require(deepnet)
library("deepnet")
library(caTools)
set.seed(101)
sample_size <- floor(0.8*nrow(stemmed_result))
set.seed(101)
train_data <- sample(seq_len(nrow(stemmed_result)),size = sample_size)
training_sample <- as.matrix(stemmed_result[train_data,])
test_sample <- as.matrix(stemmed_result[-train_data,])
class(training_sample)
test_sample_result <- data.matrix(training_sample[,1628])

training_sample_result <- data.matrix(training_sample[,1628])
dnn <-dbn.dnn.train(training_sample, training_sample_result, hidden =c(30,30),numepochs= 3, cd=2)

yy.dnn <- nn.predict(dnn,test_sample)
round(yy.dnn)


str(yy.dnn)
##Confusion Matrix
library(caret)
confusionMatrix(round(yy.dnn),test_sample_result)
length(training_sample_result)
dim(training_sample)


##SVM
library("e1071")
#install.packages("RTextTools")
library(RTextTools)
# Configure the training data
container <- create_container(training_sample, training_sample[,"review"],trainSize = 104,virgin = FALSE )
help(train_model)


# train a SVM Model
model <- train_model(container, algorithm = "SVM", method = "C-classification", kernel="linear", cost=1)


train_data <- sample(seq_len(nrow(text_input)),size = sample_size)
training <- text_input[train_data,]
test <- text_input[-train_data,]

help(svm)

svm.model <- svm(training$result~.,
                 data=training,
                 type="nu-classification",
                 kernel="radial"
)
svm.model
predictedY <- predict(svm.model,test)
