library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(tidytext)
library(tm)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("qdap")


abc_df=read.csv("C:/Users/Mohindran/Downloads/archive (3)/abcnews-date-text.csv")
train_data_df <- read.csv("D:/NEW DOWNLOADS 17 APRL/final_train.csv")
colnames(train_data_df)
test_data_df <- read.csv("D:/NEW DOWNLOADS 17 APRL/final_test.csv")
colnames(train_data_df)


#Converting publish date to date data type
abc_df$publish_date<-as.Date(as.character(abc_df$publish_date),format="%Y%m%d")

#converting headline_text to string
abc_df$headline_text<-as.character(abc_df$headline_text)
str(abc_df)

#To add year and month column to the dataframe , from the publish date column
abc_df$year<-format(abc_df$publish_date,"%Y")
abc_df$month<-format(abc_df$publish_date,"%m")


#Convert the year column to factor datatype
abc_df$year<-as.factor(abc_df$year)


names(abc_df)

#To do this analysis We are using unnest tokens to split the words from each headline to a single words each .
abc_words<-abc_df%>%
  unnest_tokens(word,headline_text)

head(stop_words)


#Removing the stop words
abc_sentiment<-abc_words%>%
  anti_join(stop_words,by="word")


#top 20 words that have been in headlines for most of the time.
abc_sentiment%>%count(word,sort=T)%>%top_n(20)


ggplot(abc_sentiment%>%count(word,sort=T)%>%top_n(20),aes(reorder(word,n),n))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n),color="#0f190f", hjust = -0.05, size = 2)+
  theme_bw()+
  coord_flip()+
  xlab("Number of Occurences")+
  ylab("Words used")+
  ggtitle("Number of Occurences of each word")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


ggplot(abc_sentiment%>%group_by(year,word)%>%summarize(num=n())%>%top_n(3,num)%>%ungroup(),
       aes(x=reorder(word,num),y=num))+
  geom_bar(stat = "identity")+
  theme_bw()+
  coord_flip()+
  xlab("Number of Occurences")+
  ylab("Words used")+
  ggtitle("Number of Occurences of each word")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1))+
  facet_wrap(~year,scales = "free")

stopwords<-data.frame(word=c(stop_words$word,"interview"))

stopwords$word<-as.character(stopwords$word)

abc_sentiment_stopwords<-abc_words%>%
  anti_join(stopwords,by="word")


ggplot(abc_sentiment_stopwords%>%group_by(year,word)%>%summarize(num=n())%>%top_n(3,num)%>%ungroup(),
       aes(x=reorder(word,num),y=num))+
  geom_bar(stat = "identity")+
  theme_bw()+
  coord_flip()+
  xlab("Number of Occurences")+
  ylab("Words used")+
  ggtitle("Number of Occurences of each word")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1))+
  facet_wrap(~year,scales = "free")



abc_bing<-abc_sentiment_stopwords%>%
  inner_join(get_sentiments("bing"),by="word")%>%
  ungroup()

head(abc_bing)


abc_bing%>%
  count(word,sentiment)%>%
  group_by(sentiment)%>%
  top_n(10,n)%>%
  ungroup()%>%
  ggplot(aes(x=reorder(word,n),y=n,fill=sentiment))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  facet_wrap(~sentiment,scales="free")+
  labs(x="number of occurences",y="Words",title="Top 10 positive and negative sentiment words used in headlines using bing lexicon")+
  theme(plot.title = element_text(size = 8, face = "bold"))

abc_nrc<-abc_sentiment_stopwords%>%
  inner_join(get_sentiments("nrc"),by="word")%>%
  ungroup()

table(abc_nrc$sentiment)


abc_nrc%>%
  filter(sentiment %in% c("joy","anger","positive","negative","sadness"))%>%
  group_by(sentiment)%>%
  count(word,sentiment)%>%
  top_n(10,n)%>%
  ungroup()%>%
  
  ggplot(aes(x=reorder(word,n),y=n,fill=sentiment))+
  geom_col(show.legend=F)+
  coord_flip()+
  facet_wrap(~sentiment,scales="free")+
  labs(x="number of occurences",y="Words",title="Top 10 words for each emotion used in headlines using nrc lexicon")+
  theme(plot.title = element_text(size = 8, face = "bold"))




#analyse only the first 1000 headlines just for computational time reasons
news_df_subset <- abc_df[1:1000,,drop=FALSE]
tkn_l <- apply(news_df_subset, 1, function(x) { data.frame(headline_text=x, stringsAsFactors = FALSE) %>% unnest_tokens(word, headline_text)})

#Removing the stop words from the token list.
single_news_tokens <- lapply(tkn_l, function(x) {anti_join(x, stop_words)})

#The sentiment of a headline is computed as based on the sum of positive/negative score of each token
str(single_news_tokens, list.len = 5)
single_news_tokens[[1]]


compute_sentiment <- function(d) {
  if (nrow(d) == 0) {
    return(NA)
  }
  neg_score <- d %>% filter(sentiment=="negative") %>% nrow()
  pos_score <- d %>% filter(sentiment=="positive") %>% nrow()
  pos_score - neg_score
}

sentiments_bing <- get_sentiments("bing")
str(sentiments_bing)

single_news_sentiment_bing <- sapply(single_news_tokens, function(x) { x %>% inner_join(sentiments_bing) %>% compute_sentiment()})
str(single_news_sentiment_bing)

summary(single_news_sentiment_bing)
#Collecting the resulting in a data frame as follows.
single_news_sentiment_bing_df <- data.frame(headline_text=news_df_subset$headline_text, score = single_news_sentiment_bing)
head(single_news_sentiment_bing_df, 10)

sentiments_nrc <- get_sentiments("nrc")
(unique_sentiments_nrc <- unique(sentiments_nrc$sentiment))

compute_pos_neg_sentiments_nrc <- function(the_sentiments_nrc) {
  s <- unique(the_sentiments_nrc$sentiment)
  df_sentiments <- data.frame(sentiment = s, 
                              mapped_sentiment = c("positive", "negative", "negative", "negative",
                                                   "negative", "positive", "positive", "negative", 
                                                   "positive", "positive"))
  ss <- sentiments_nrc %>% inner_join(df_sentiments)
  the_sentiments_nrc$sentiment <- ss$mapped_sentiment
  the_sentiments_nrc
}

nrc_sentiments_pos_neg_scale <- compute_pos_neg_sentiments_nrc(sentiments_nrc)

single_news_sentiment_nrc <- sapply(single_news_tokens, function(x) { x %>% inner_join(nrc_sentiments_pos_neg_scale) %>% compute_sentiment()})
str(single_news_sentiment_nrc)

summary(single_news_sentiment_nrc)

single_news_sentiment_nrc_df <- data.frame(headline_text=news_df_subset$headline_text, score = single_news_sentiment_nrc)
head(single_news_sentiment_nrc_df, 10)


sentiments_afinn <- get_sentiments("afinn")
colnames(sentiments_afinn) <- c("word", "sentiment")
str(sentiments_afinn)


single_news_sentiment_afinn_df <- lapply(single_news_tokens, function(x) { x %>% inner_join(sentiments_afinn)})
single_news_sentiment_afinn <- sapply(single_news_sentiment_afinn_df, function(x) { 
  ifelse(nrow(x) > 0, sum(x$sentiment), NA)
})
str(single_news_sentiment_afinn)


summary(single_news_sentiment_afinn)

single_news_sentiment_afinn_df <- data.frame(headline_text=news_df_subset$headline_text, score = single_news_sentiment_afinn)
head(single_news_sentiment_afinn_df, 10)


compute_congruence <- function(x,y,z) {
  v <- c(sign(x), sign(y), sign(z))
  # if only one lexicon reports the score, we cannot check for congruence
  if (sum(is.na(v)) >= 2) {
    return (NA)
  }
  # removing NA and zero value
  v <- na.omit(v)
  v_sum <- sum(v)
  abs(v_sum) == length(v)
}

compute_final_sentiment <- function(x,y,z) {
  if (is.na(x) && is.na(y) && is.na(z)) {
    return (NA)
  }
  
  s <- sum(x, y, z, na.rm=TRUE)
  # positive sentiments have score strictly greater than zero
  # negative sentiments have score strictly less than zero
  # neutral sentiments have score equal to zero 
  ifelse(s > 0, "positive", ifelse(s < 0, "negative", "neutral"))
}

news_sentiments_results <- data.frame(headline_text = news_df_subset$headline_text, 
                                      bing_score = single_news_sentiment_bing, 
                                      nrc_score = single_news_sentiment_nrc, 
                                      afinn_score = single_news_sentiment_afinn,
                                      stringsAsFactors = FALSE)

news_sentiments_results <- news_sentiments_results %>% rowwise() %>% 
  mutate(final_sentiment = compute_final_sentiment(bing_score, nrc_score, afinn_score),
         congruence = compute_congruence(bing_score, nrc_score, afinn_score))

head(news_sentiments_results, 40)


replace_score_with_sentiment <- function(v_score) {
  v_score[v_score > 0] <- "positive"
  v_score[v_score < 0] <- "negative"
  v_score[v_score == 0] <- "neutral"
  v_score
} 

news_sentiments_results$bing_score <- replace_score_with_sentiment(news_sentiments_results$bing_score)
news_sentiments_results$nrc_score <- replace_score_with_sentiment(news_sentiments_results$nrc_score)
news_sentiments_results$afinn_score <- replace_score_with_sentiment(news_sentiments_results$afinn_score)

news_sentiments_results[,2:5] <- lapply(news_sentiments_results[,2:5], as.factor)

head(news_sentiments_results, 40)


table(news_sentiments_results$bing_score, news_sentiments_results$final_sentiment, dnn = c("bing", "final"))

table(news_sentiments_results$nrc_score, news_sentiments_results$final_sentiment, dnn = c("nrc", "final"))
table(news_sentiments_results$afinn_score, news_sentiments_results$final_sentiment, dnn = c("afinn", "final"))
table(news_sentiments_results$congruence, news_sentiments_results$final_sentiment, dnn = c("congruence", "final"))


#function to clean the text field.
qdap_clean<-function(text){
  text<-tolower(text)
  text<-removePunctuation(text)
  text<-replace_number(text)
  text<-stripWhitespace(text)
  text<-bracketX(text)
  text<-replace_abbreviation(text)
  text<-replace_contraction(text)
  text<-replace_symbol(text)    
  text<-removeWords(text,stopwords("en"))
  return(text)
}
abc_headlines=abc_headlines[1:10000]
abc_headlines

#Applying text clean function to our text vector
clean_headlines<-qdap_clean(abc_headlines)


#Creating vector source from the headlines of the abc news dataset
abc_source<-VectorSource(clean_headlines)
abc_corpus<-VCorpus(abc_source)



abc_corpus[[7]][1]
#Cleaning the corpus before doing further analysis
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords,stopwords("en"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus,stripWhitespace)
  return(corpus)
}

abc_clean_corpus<-clean_corpus(abc_corpus)

#Creating document term matrix
abc_dtm<-DocumentTermMatrix(abc_clean_corpus)

#convert documenttermmatrix to matrix
abc_m<-as.matrix(abc_dtm[1:500,])

#Sort the term frequency and look at the top 20 most frequently occuring words
term_freq<-colSums(abc_m)

term_freq<-sort(term_freq,decreasing=TRUE)

term_freq[1:10]

#Printing a wordcloud
wordcloud(names(term_freq),term_freq,max.words=100,color="blue")


#getting the random sentiment score and appending in the original dataset for further machine learning algorithms
library(sentimentr)
sentiment(abc_headlines)
abc_headlines$sentiments<-sentiment(abc_headlines)$sentiments
abc_headlines$sentiments

headl=abc_df$headline_text[1:10000]
headl
final=sentiment_by(headl)
final

#Convert sentiment data.table to a data frame
sentiment_df <- as.data.frame(final)

#Function that generates a sentiment class based on sentiment score
get_sentiment_class <- function(sentiment_score) {
  
  sentiment_class = "1"
  
  if ( sentiment_score < -0.1) {
    sentiment_class = "0"
  } 
  
  sentiment_class
}

#add a sentiment_class attribute
sentiment_df$sentiment_class <- 
  sapply(sentiment_df$ave_sentiment,get_sentiment_class)

#Print resulting sentiment
sentiments=sentiment_df$sentiment_class
sentiments
cbind(abc_df,sentiments)
prop.table(table(sentiment_df$sentiment_class))

news_sentiments_results <- data.frame(headline_text = abc_df$headline_text, 
                                      sentiments = sentiment_df$sentiment_class, 
                                      stringsAsFactors = FALSE)
news_sentiments_results


#Convert Sentiment to factor
train_data_df$sentiments <- as.factor(train_data_df$sentiments)
head(train_data_df)

#Glimpse at how sentimentss are distributed
table(train_data_df$sentiments)

#Average words in each headline_text
mean(sapply(sapply(train_data_df$headline_text, strsplit, " "), length))

#Preparing Corpus 
#corpus <- Corpus(VectorSource(c(train_data_df$headline_text, test_data_df$headline_text)))

#corpus[1]$content

#Transforming contents of corpuses

#Converting to lower cases
#corpus <- tm_map(corpus, content_transformer(tolower))
#Removing Punctuations, English Stop Words, strip white spaces, and stem each word
#corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus, stemDocument)

#corpus[1]$content

#Putting corpus in the shape of a document matrix
#dtm <- DocumentTermMatrix(corpus)
#dtm
#Removing Sparse Terms
sparse <- removeSparseTerms(abc_dtm, 0.99)
sparse

#Convert this matrix into a data frame
important_words_df <- as.data.frame(as.matrix(sparse))
colnames(important_words_df) <- make.names(colnames(important_words_df))
# split into train and test
important_words_train_df <- head(important_words_df, nrow(train_data_df))
important_words_test_df <- tail(important_words_df, nrow(test_data_df))
# Add to original dataframes
train_data_words_df <- cbind(train_data_df, important_words_train_df)
test_data_words_df <- cbind(test_data_df, important_words_test_df)
# Get rid of the original Text field
train_data_words_df$headline_text <- NULL
test_data_words_df$headline_text <- NULL


library(caTools)
library("caTools") #For Model Preparation
library("e1071")
set.seed(1234)
# first we create an index with 85% True values based on Sentiment
spl <- sample.split(train_data_words_df$sentiments, .85)
# now we use it to split our data into train and test
eval_train_data_df <- train_data_words_df[spl==T,]
eval_test_data_df <- train_data_words_df[spl==F,]

#Fitting a logistic regression model
log_model <- glm(sentiments~., data=eval_train_data_df, family=binomial)
log_pred <- predict(log_model, newdata=eval_test_data_df, type="response")
#Confusion Matrix Table
log_tab <- table(eval_test_data_df$sentiments, log_pred>.5)
log_tab
#Accuracy of logistic regression model in percentage
((590 + 106) / nrow(eval_test_data_df)) * 100

#SVM
svm_model = svm(formula = sentiments ~ ., data = eval_train_data_df, type = 'C-classification', kernel = 'linear')
svm_pred <- predict(svm_model, newdata=eval_test_data_df)
#Confusion Matrix Table
svm_tab <- table(eval_test_data_df$sentiments, svm_pred)
svm_tab
#Accuracy of SVM Classifier Model in percentage
((590 + 99) / nrow(eval_test_data_df)) * 100

#Naive Bayes
nb_model = naiveBayes(formula = sentiments ~ ., data = eval_train_data_df)
nb_pred <- predict(nb_model, newdata=eval_test_data_df)
nb_pred
#Confusion Matrix Table
nb_tab <- table(eval_test_data_df$sentiments, nb_pred)
nb_tab
#Accuracy of Naive Bayes Classifier Model in percentage
print("Accuracy of Naive Bayes")
((572 + 111) / nrow(eval_test_data_df)) * 100


#RANDOM FOREST
library(randomForest)
RF_model = randomForest(sentiments ~ ., data=eval_train_data_df)
predictRF = predict(RF_model, newdata=eval_train_data_df)
#nb_tab1 <- table(eval_test_data_df$sentiments, predictRF)
new=RF_model$confusion
new
((3291+644)/(644+237+1963+3291))*100









