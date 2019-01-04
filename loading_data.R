# loading data ####
library(readr)
library(tidyverse)
library(tm)
library(tidytext)
library(wordcloud)
library(stringr)
library(gridExtra)
library(ngram)


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
remove_repeated_words = function(x){
  x = tolower(x)
  x = gsub("'", "", x)
  x = gsub("[[:punct:]]", " ", x)
  x = gsub("\n", "", x)
  x = gsub("\t", "", x)
  x = gsub("\\s+", " ", x)
  d = unlist(strsplit(x, " "))
  x = paste(unique(d), collapse = " ")
  
}
train$no_repeat_words_comments = as.vector(apply(X = train[,2, drop = F], MARGIN = 1, FUN = remove_repeated_words))
#colnames(train)[9] = "no_repeat_words_comments"
unnested_words_sentiments = train %>% 
  unnest_tokens(word, no_repeat_words_comments) %>%
  inner_join(get_sentiments("bing"))
unnested_words_sentiments$sentiment  = if_else(unnested_words_sentiments$sentiment == "positive", 1, -1)


train = unnested_words_sentiments %>%
  group_by(id) %>%
  summarise(sentiment_score = sum(sentiment)) %>%
  right_join(train, by = "id") # 22% comments have missing sentiment score

summary(train$sentiment_score) # now the sentiment are within a more reasonable range

threat_comments = train[train$threat == 1,]
severe_toxic_comments = train[train$severe_toxic == 1,]
toxic_comments = train[train$toxic == 1,]
obscene_comments = train[train$obscene == 1,]
insult_comments = train[train$insult == 1,]
identity_hate_comments = train[train$identity_hate == 1,]
non_toxic_comments = train[train$toxicity_level == 0,]



summary(severe_toxic_comments$sentiment_score)
summary(toxic_comments$sentiment_score)
summary(obscene_comments$sentiment_score)
summary(insult_comments$sentiment_score)
summary(threat_comments$sentiment_score)
summary(identity_hate_comments$sentiment_score)
summary(non_toxic_comments$sentiment_score)

#Any comment with a toxic level is likely to have a negative sentiment analysis score


sev_plot = ggplot(severe_toxic_comments, aes(x = sentiment_score)) + geom_histogram(fill = "red") + 
  ggtitle("Severe Toxic Sentiment Scores") + scale_x_continuous(breaks = seq(-60, 40, 10), labels = seq(-60, 40, 10))
tox_plot = ggplot(toxic_comments, aes(x = sentiment_score)) + geom_histogram(fill = "red") + 
  ggtitle("Toxic Sentiment Scores") + scale_x_continuous(breaks = seq(-60, 40, 10), labels = seq(-60, 40, 10))
obs_plot = ggplot(obscene_comments, aes(x = sentiment_score)) + geom_histogram(fill = "red") +
  ggtitle("Obscene Sentiment Scores") + scale_x_continuous(breaks = seq(-60, 40, 10), labels = seq(-60, 40, 10))
ins_plot = ggplot(insult_comments, aes(x = sentiment_score)) + geom_histogram(fill = "red") +
  ggtitle("Insult Sentiment Scores") + scale_x_continuous(breaks = seq(-60, 40, 10), labels = seq(-60, 40, 10))
threat_plot = ggplot(threat_comments, aes(x = sentiment_score)) + geom_histogram(fill = "red") +
  ggtitle("Threat Sentiment Scores") + scale_x_continuous(breaks = seq(-60, 40, 10), labels = seq(-60, 40, 10))
ident_plot = ggplot(identity_hate_comments, aes(x = sentiment_score)) + geom_histogram(fill = "red") +
  ggtitle("Identity Hate Sentiment Scores") + scale_x_continuous(breaks = seq(-60, 40, 10), labels = seq(-60, 40, 10))
nontox_plot = ggplot(non_toxic_comments, aes(x = sentiment_score)) + geom_histogram(fill = "red") + 
  ggtitle("Non-toxic Sentiment Scores") + scale_x_continuous(breaks = seq(-60, 40, 10), labels = seq(-60, 40, 10))
grid.arrange(sev_plot, tox_plot, obs_plot, ins_plot, threat_plot, ident_plot, nontox_plot)
# from the plots, majority of toxic comments at various levels had negative sentiment score. Try combining various 
# sentiment analysis methods to create more robust sentiment scores

train$comments_nchar = nchar(train$comment_text)

clean_comments = function(x){
  x = tolower(x)
  x = gsub("'", "", x)
  x = gsub("[[:punct:]]", " ", x)
  x = gsub("\n", "", x)
  x = gsub("\t", "", x)
  x = gsub("\\s+", " ", x)
  
}

train$clean_comments = clean_comments(train$comment_text)
train$number_words = sapply(strsplit(train$clean_comments, "\\s+"), length)
train$number_unique_words = sapply(strsplit(train$no_repeat_words_comments, "\\s+"), length)
train$prop_duplicated_words = 100 - (round((train$number_unique_words/train$number_words), 2) * 100)


threat_comments = train[train$threat == 1,]
severe_toxic_comments = train[train$severe_toxic == 1,]
toxic_comments = train[train$toxic == 1,]
obscene_comments = train[train$obscene == 1,]
insult_comments = train[train$insult == 1,]
identity_hate_comments = train[train$identity_hate == 1,]
non_toxic_comments = train[train$toxicity_level == 0,]

# number_unique_words
sev_plot = ggplot(severe_toxic_comments, aes(x = number_unique_words)) + geom_histogram(fill = "red") + 
  ggtitle("Severe Toxic Number of Unique Words") + scale_x_continuous(breaks = seq(0, 600, 50), labels = seq(0, 600, 50))
tox_plot = ggplot(toxic_comments, aes(x = number_unique_words)) + geom_histogram(fill = "red") + 
  ggtitle("Toxic Number of Unique Words") + scale_x_continuous(breaks = seq(0, 600, 50), labels = seq(0, 600, 50))
obs_plot = ggplot(obscene_comments, aes(x = number_unique_words)) + geom_histogram(fill = "red") +
  ggtitle("Obscene Number of Unique Words") + scale_x_continuous(breaks = seq(0, 600, 50), labels = seq(0, 600, 50))
ins_plot = ggplot(insult_comments, aes(x = number_unique_words)) + geom_histogram(fill = "red") +
  ggtitle("Insult Number of Unique Words") + scale_x_continuous(breaks = seq(0, 600, 50), labels = seq(0, 600, 50))
threat_plot = ggplot(threat_comments, aes(x = number_unique_words)) + geom_histogram(fill = "red") +
  ggtitle("Threat Number of Unique Words") + scale_x_continuous(breaks = seq(0, 600, 50), labels = seq(0, 600, 50))
ident_plot = ggplot(identity_hate_comments, aes(x = number_unique_words)) + geom_histogram(fill = "red") +
  ggtitle("Identity Hate Number of Unique Words") + scale_x_continuous(breaks = seq(0, 600, 50), labels = seq(0, 600, 50))
nontox_plot = ggplot(non_toxic_comments, aes(x = number_unique_words)) + geom_histogram(fill = "red") + 
  ggtitle("Non-toxic Number of Unique Words") + scale_x_continuous(breaks = seq(0, 600, 50), labels = seq(0, 600, 50))
grid.arrange(sev_plot, tox_plot, obs_plot, ins_plot, threat_plot, ident_plot, nontox_plot)

# number_words
sev_plot = ggplot(severe_toxic_comments, aes(x = number_words)) + geom_histogram(fill = "red") + 
  ggtitle("Severe Toxic Number of Words") + scale_x_continuous(breaks = seq(0, 1400, 200), labels = seq(0, 1400, 200))
tox_plot = ggplot(toxic_comments, aes(x = number_words)) + geom_histogram(fill = "red") + 
  ggtitle("Toxic Number of Words") + scale_x_continuous(breaks = seq(0, 1400, 200), labels = seq(0, 1400, 200))
obs_plot = ggplot(obscene_comments, aes(x = number_words)) + geom_histogram(fill = "red") +
  ggtitle("Obscene Number of Words") + scale_x_continuous(breaks = seq(0, 1400, 200), labels = seq(0, 1400, 200))
ins_plot = ggplot(insult_comments, aes(x = number_words)) + geom_histogram(fill = "red") +
  ggtitle("Insult Number of Words") + scale_x_continuous(breaks = seq(0, 1400, 200), labels = seq(0, 1400, 200))
threat_plot = ggplot(threat_comments, aes(x = number_words)) + geom_histogram(fill = "red") +
  ggtitle("Threat Number of Words") + scale_x_continuous(breaks = seq(0, 1400, 200), labels = seq(0, 1400, 200))
ident_plot = ggplot(identity_hate_comments, aes(x = number_words)) + geom_histogram(fill = "red") +
  ggtitle("Identity Hate Number of Words") + scale_x_continuous(breaks = seq(0, 1400, 200), labels = seq(0, 1400, 200))
nontox_plot = ggplot(non_toxic_comments, aes(x = number_words)) + geom_histogram(fill = "red") + 
  ggtitle("Non-toxic Number of Words") + scale_x_continuous(breaks = seq(0, 1400, 200), labels = seq(0, 1400, 200))
grid.arrange(sev_plot, tox_plot, obs_plot, ins_plot, threat_plot, ident_plot, nontox_plot)

# proportion of duplicate words
sev_plot = ggplot(severe_toxic_comments[severe_toxic_comments$prop_duplicated_words >= 0,], aes(x = prop_duplicated_words)) + geom_histogram(fill = "red") + 
  ggtitle("Severe Toxic Proportion of Duplicate Words") + scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10))
tox_plot = ggplot(toxic_comments[toxic_comments$prop_duplicated_words >= 0,], aes(x = prop_duplicated_words)) + geom_histogram(fill = "red") + 
  ggtitle("Toxic Proportion of Duplicate Words") + scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10))
obs_plot = ggplot(obscene_comments[obscene_comments$prop_duplicated_words >= 0,], aes(x = prop_duplicated_words)) + geom_histogram(fill = "red") +
  ggtitle("Obscene Proportion of Duplicate Words") + scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10))
ins_plot = ggplot(insult_comments[insult_comments$prop_duplicated_words >= 0,], aes(x = prop_duplicated_words)) + geom_histogram(fill = "red") +
  ggtitle("Insult Proportion of Duplicate Words") + scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10))
threat_plot = ggplot(threat_comments[threat_comments$prop_duplicated_words >= 0,], aes(x = prop_duplicated_words)) + geom_histogram(fill = "red") +
  ggtitle("Threat Proportion of Duplicate Words") + scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10))
ident_plot = ggplot(identity_hate_comments[identity_hate_comments$prop_duplicated_words >= 0,], aes(x = prop_duplicated_words)) + geom_histogram(fill = "red") +
  ggtitle("Identity Hate Proportion of Duplicate Words") + scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10))
nontox_plot = ggplot(non_toxic_comments[non_toxic_comments$prop_duplicated_words >= 0,], aes(x = prop_duplicated_words)) + geom_histogram(fill = "red") + 
  ggtitle("Non-toxic Proportion of Duplicate Words") + scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10))
grid.arrange(sev_plot, tox_plot, obs_plot, ins_plot, threat_plot, ident_plot, nontox_plot)

# if a comment has more than 60% of duplicated words, then it likely falls under one category of toxic

train$use_2_caps = grepl("[A-Z]{2,}",train$comment_text)
train$use_3_caps = grepl("[A-Z]{3,}",train$comment_text)
train$use_6_caps = grepl("[A-Z]{6,}",train$comment_text)

round(prop.table(table(toxic_comments$use_3_caps)),2)
round(prop.table(table(severe_toxic_comments$use_3_caps)), 2)
round(prop.table(table(obscene_comments$use_3_caps)), 2)
round(prop.table(table(insult_comments$use_3_caps)), 2)
round(prop.table(table(threat_comments$use_3_caps)), 2)
round(prop.table(table(identity_hate_comments$use_3_caps)),2)
round(prop.table(table(non_toxic_comments$use_3_caps)),2)

# proportion of non_toxic comments with at least 3 caps is lower than for toxic comments
round(prop.table(table(toxic_comments$use_6_caps)),2)
round(prop.table(table(severe_toxic_comments$use_6_caps)), 2)
round(prop.table(table(obscene_comments$use_6_caps)), 2)
round(prop.table(table(insult_comments$use_6_caps)), 2)
round(prop.table(table(threat_comments$use_6_caps)), 2)
round(prop.table(table(identity_hate_comments$use_6_caps)),2)
round(prop.table(table(non_toxic_comments$use_6_caps)),2)
# proportion of non_toxic comments with at least 6 caps is significantly lower than for toxic comments


