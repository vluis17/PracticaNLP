library(tidyverse) #data manipilation
library(DT)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(janeaustenr)
library(tidytext)
library(GGally)
library(moments)
library(sentimentr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)
install.packages(c("mnormt", "psych", "SnowballC", "hunspell", 
                   "broom", "tokenizers", "janeaustenr"))
install.packages("tm")
install.packages("wordcloud")
install.packages("crfsuite")


#Guardamos el dataset en reviews
reviews <- read.csv(file = 'C:/Users/luisc/Desktop/Intelligent Systems/archive/reviews.csv')

#Necesitamos que la variable se llame words para el wordcloud
words <- reviews %>% select(c("userName", "score", "content")) %>% unnest_tokens(word, content) %>% filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

#Funcion Afinn
afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))
reviews.afinn <- words %>% inner_join(afinn, by = "word")

#Palabras positivas
positivo <- reviews.afinn %>% group_by(word) %>% summarise(mean_rating = mean(score), score = max(score), count_word = n()) %>%
        filter(mean_rating>mean(mean_rating)) %>% arrange(desc(mean_rating))

wordcloud(words = positivo$word, freq = positivo$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#Palabras negativas
negativo <- reviews.afinn %>% group_by(word) %>% summarise(mean_rating = mean(score), score = max(score), count_word = n()) %>%
        filter(mean_rating<mean(mean_rating)) %>% arrange(mean_rating)

wordcloud(words = negativo$word, freq = negativo$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))