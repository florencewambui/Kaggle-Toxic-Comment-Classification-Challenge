# loading data ####
library(readr)
library(tidyverse)
library(tm)
library(tidytext)
library(wordcloud)
library(stringr)

train <- read_csv("train.csv")
test = read_csv("test.csv")

# glimpse at the data ####
# structure of data: training data has 159671 rows and 8 variables
# a comment can have multiple labels

train$toxicity_level = rowSums(train[,3:8])
table(train$toxicity_level)
table(train$toxicity_level > 0) # only 16225 comments out of 159671 (10% of the rows)
#have at least one level of toxicity

# level of toxicity: toxic
table(train$toxic) # 15294 toxic comments
head(train[train$toxic == 1,], 5)
toxic_comments = train[train$toxic == 1,]
round(prop.table(table(toxic_comments$severe_toxic)) * 100) # only 10% of toxic_comments are severe_toxic
round(prop.table(table(toxic_comments$obscene)) * 100) # 52% of toxic_comments are obscene
round(prop.table(table(toxic_comments$threat)) * 100) # only 3% of toxic_comments are threats
round(prop.table(table(toxic_comments$insult)) * 100) # 48% of toxic_comments are insults
round(prop.table(table(toxic_comments$identity_hate)) * 100) # only 9% of toxic_comments are identity_hate

# words in toxic_comments

clean_corpus_frequent_words = function(x){
  corpus = VCorpus(VectorSource(x))
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, stemDocument)
  dtm = DocumentTermMatrix(corpus)
  dtm_sparse = removeSparseTerms(dtm, 0.99) # retain only
  # words that appear in at least 1% of the toxic_comments
  dtm_frequencies = data.frame(colSums(as.matrix(dtm_sparse)))
  dtm_frequencies = rownames_to_column(dtm_frequencies, "word")
  colnames(dtm_frequencies)[2]= "frequency"
  return(dtm_frequencies)
}


clean_corpus_plot_wordcloud = function(x){
  corpus = VCorpus(VectorSource(x))
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, PlainTextDocument)
  #corpus = tm_map(corpus, stemDocument)
  dtm = DocumentTermMatrix(corpus)
  dtm_sparse = removeSparseTerms(dtm, 0.99) # retain only
  # words that appear in at least 1% of the toxic_comments
  dtm_frequencies = data.frame(colSums(as.matrix(dtm_sparse)))
  dtm_frequencies = rownames_to_column(dtm_frequencies, "word")
  colnames(dtm_frequencies)[2]= "frequency"
  pal <- brewer.pal(9,"OrRd")
  pal <- pal[-(1:4)]
  wordcloud(dtm_frequencies$word, dtm_frequencies$frequency, max.words = 150, colors = pal)
  #return(word_plot)
}



threat_comments = train[train$threat == 1,]
severe_toxic_comments = train[train$severe_toxic == 1,]
obscene_comments = train[train$obscene == 1,]
insult_comments = train[train$insult == 1,]
identity_hate_comments = train[train$identity_hate == 1,]

clean_corpus_plot_wordcloud(severe_toxic_comments$comment_text)
clean_corpus_plot_wordcloud(toxic_comments$comment_text)
clean_corpus_plot_wordcloud(obscene_comments$comment_text)
clean_corpus_plot_wordcloud(insult_comments$comment_text)
clean_corpus_plot_wordcloud(threat_comments$comment_text)
clean_corpus_plot_wordcloud(identity_hate_comments$comment_text)

# severe_toxic, toxic, obscene and insults wordcloud look quite similar with the
# following words popping out: fuck, ass, shit, suck, nigger, fat, moron
# threat wordcloud has words such as kill, die, murder popping out
# identity_hate wordcloud has words such as nigger, fat, jew, faggot, gay popping out

severe_toxic_words = clean_corpus_frequent_words(severe_toxic_comments$comment_text)
toxic_words = clean_corpus_frequent_words(toxic_comments$comment_text)
obscene_words = clean_corpus_frequent_words(obscene_comments$comment_text)
insult_words = clean_corpus_frequent_words(insult_comments$comment_text)
threat_words = clean_corpus_frequent_words(threat_comments$comment_text)
identity_hate_words = clean_corpus_frequent_words(identity_hate_comments$comment_text)



severe_toxic_only_words = severe_toxic_words[!(severe_toxic_words$word %in% toxic_words$word|
                                             severe_toxic_words$word %in% obscene_words$word|
                                             severe_toxic_words$word %in% insult_words$word|
                                             severe_toxic_words$word %in% threat_words$word|
                                             severe_toxic_words$word %in% identity_hate_words$word),]
# severe_toxic_only_words: computer,stick, cum, lick, mouth, rot

toxic_only_words = toxic_words[!(toxic_words$word %in% severe_toxic_words$word|
                                               toxic_words$word %in% obscene_words$word|
                                               toxic_words$word %in% insult_words$word|
                                               toxic_words$word %in% threat_words$word|
                                               toxic_words$word %in% identity_hate_words$word),]
#toxic_only_words: nerd, dumb, die, little, prick
obscene_only_words = obscene_words[!(obscene_words$word %in% severe_toxic_words$word|
                                          obscene_words$word %in% toxic_words$word|
                                          obscene_words$word %in% insult_words$word|
                                          obscene_words$word %in% threat_words$word|
                                          obscene_words$word %in% identity_hate_words$word),]


insult_only_words = insult_words[!(insult_words$word %in% severe_toxic_words$word|
                                         insult_words$word %in% toxic_words$word|
                                         insult_words$word %in% obscene_words$word|
                                         insult_words$word %in% threat_words$word|
                                         insult_words$word %in% identity_hate_words$word),]

threat_only_words = threat_words[!(threat_words$word %in% severe_toxic_words$word|
                                         threat_words$word %in% toxic_words$word|
                                         threat_words$word %in% obscene_words$word|
                                         threat_words$word %in% insult_words$word|
                                         threat_words$word %in% identity_hate_words$word),] # murder, shoot,
# pain, stab, fire, blood, gun, kick, splatter, threaten, violate, slit, shove

identity_hate_only_words = identity_hate_words[!(identity_hate_words$word %in% severe_toxic_words$word|
                                     identity_hate_words$word %in% toxic_words$word|
                                     identity_hate_words$word %in% obscene_words$word|
                                     identity_hate_words$word %in% insult_words$word|
                                     identity_hate_words$word %in% threat_words$word),] # america,
#antisemit, arab, asian, christian, countries, hitler, indian, islam, jesus, jewish, muslim, queer, race, religion, terrorist, women

non_toxic_comments = train[train$toxicity_level == 0,]
non_toxic_words = clean_corpus_frequent_words(non_toxic_comments$comment_text)
clean_corpus_plot_wordcloud(non_toxic_comments$comment_text) #please, thanks, good
non_toxic_words_only = non_toxic_words[!(non_toxic_words$word %in% severe_toxic_words$word|
                                           non_toxic_words$word %in% toxic_words$word|
                                           non_toxic_words$word %in% obscene_words$word|
                                           non_toxic_words$word %in% insult_words$word|
                                           non_toxic_words$word %in% threat_words$word|
                                           non_toxic_words$word %in% threat_words$word),] 

# sentiment analysis ####
unnested_words_sentiments = train %>% 
  unnest_tokens(word, comment_text) %>%
  inner_join(get_sentiments("bing"))
unnested_words_sentiments$sentiment  = if_else(unnested_words_sentiments$sentiment == "positive", 1, -1)
train_sentiments = unnested_words_sentiments %>%
  group_by(id) %>%
  summarise(sentiment_score = sum(sentiment)) %>%
  right_join(train, by = "id") # 22% comments have missing sentiment score

# a look at comments with very high or very low sentiment scores
train$comment_text[train$sentiment_score > 100]                                       
train$comment_text[train$sentiment_score < -500]                                       
# the high positive and negative scores are mainly due to repetition of words and phrases
# remove repeated words
train$no_repeat_words_comments = as.vector(apply(X = train[,2, drop = F], MARGIN = 1, FUN = remove_repeated_words))
#colnames(train)[9] = "no_repeat_words_comments"
unnested_words_sentiments = train %>% 
  unnest_tokens(word, no_repeat_words_comments) %>%
  inner_join(get_sentiments("bing"))
unnested_words_sentiments$sentiment  = if_else(unnested_words_sentiments$sentiment == "positive", 1, -1)


train_sentiments = unnested_words_sentiments %>%
  group_by(id) %>%
  summarise(sentiment_score = sum(sentiment)) %>%
  right_join(train, by = "id") # 22% comments have missing sentiment score

summary(train_sentiments$sentiment_score) # now the sentiment are within a more reasonable range

threat_comments = train_sentiments[train_sentiments$threat == 1,]
severe_toxic_comments = train_sentiments[train_sentiments$severe_toxic == 1,]
toxic_comments = train_sentiments[train_sentiments$toxic == 1,]
obscene_comments = train_sentiments[train_sentiments$obscene == 1,]
insult_comments = train_sentiments[train_sentiments$insult == 1,]
identity_hate_comments = train_sentiments[train_sentiments$identity_hate == 1,]

train_sentiments$toxicity_level = rowSums(train_sentiments[,4:9])

non_toxic_comments = train_sentiments[train_sentiments$toxicity_level == 0,]

summary(severe_toxic_comments$sentiment_score)
summary(toxic_comments$sentiment_score)
summary(obscene_comments$sentiment_score)
summary(insult_comments$sentiment_score)
summary(threat_comments$sentiment_score)
summary(identity_hate_comments$sentiment_score)
summary(non_toxic_comments$sentiment_score)

#Any comment with a toxic level is likely to have a negative sentiment analysis score




                                               