
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)

rm(list=ls())

# DATA IMPORT ####
# - read text
# - tokenize
# - calculate 1-, 2-, 3-grams

# DON'T RUN, takes about 40 minutes

if (FALSE) {

# load English stopwords (from tidytext)
data(stop_words)

# 1grams

process1gramfile <- function(filename, minoccurence=10) {
        filename %>%
        readr::read_lines() %>%
        tibble(text=.) %>%
        unnest_tokens(word,text
                      ,strip_numeric=TRUE
                      ) %>%               # split text into tokens
        anti_join(stop_words) %>%
        filter(grepl("^[[:alpha:]]+$", word)) %>%  # keep tokens that are alphabetic only
        count(word) %>%                            # count occurences
        filter(n>minoccurence)                     # remove those occuring rarely
}

df1 <- bind_rows(
         "data/final/en_US/en_US.twitter.txt" %>% process1gramfile()
        ,"data/final/en_US/en_US.news.txt" %>% process1gramfile()
        ,"data/final/en_US/en_US.blogs.txt" %>% process1gramfile()
        ) %>%
        group_by(word) %>% summarize(n=sum(n)) %>% ungroup() %>%
        mutate(len=str_length(word))           # keep word length for faster search from UI

# n-grams

processngramfile <- function(filename, ngram, minoccurence=10) {
        filename %>%
                readr::read_lines() %>%
                tibble(text=.) %>%
                unnest_tokens(bigram,text
                              ,token="ngrams"
                              ,n=ngram) %>% # split text into tokens
                count(bigram) %>%                            # count occurences
                filter(n>minoccurence)                     # remove those occuring rarely
}

#  about 10m, 547343 rows
df2 <- bind_rows(
                 "data/final/en_US/en_US.twitter.txt" %>% processngramfile(2)
                ,"data/final/en_US/en_US.news.txt" %>% processngramfile(2)
                ,"data/final/en_US/en_US.blogs.txt" %>% processngramfile(2)
        ) %>%
        group_by(bigram) %>% summarize(n=sum(n)) %>% ungroup()

#save(list=c("df1","df2"),file="datatidy2.Rda")


# about 30m, 489996 rows
print(Sys.time())
df3 <- bind_rows(
                 "data/final/en_US/en_US.twitter.txt" %>% processngramfile(3)
                ,"data/final/en_US/en_US.news.txt" %>% processngramfile(3)
                ,"data/final/en_US/en_US.blogs.txt" %>% processngramfile(3)
        ) %>%
        group_by(bigram) %>% summarize(n=sum(n)) %>% ungroup()
print(Sys.time())

save(list=c("df1","df2","df3"),file="datatidy.Rda")

} # if (FALSE)

# DATA IMPORT STAGE2 ####

rm(list=ls())
# load transformed data
load("datatidy.Rda")

# split bigram columns into word1/word2 or word1/word2/word3 columns
df2split <- df2 %>% separate(bigram, c("word1", "word2"), sep = " ")
df3split <- df3 %>% separate(bigram, c("word1", "word2", "word3"), sep = " ")

# keep only what is needed for the model
save(list=c("df1","df2split","df3split"),file="datatidy_split.Rda")

# MODEL ####

rm(list=ls())
load("datatidy_split.Rda")

# test input #
inputtxt <- "many thanks for"

# constants
# how many predictions to keep (for wordcloud)
n_predictions <- 10
# weights for smoothing
# for 2-gram: P(w|w1) = l11*p(w)+l12*p(w|w1)
l11 <- .1
l12 <- 1-l11
# for 3-gram: P(w|w1,w2)=l21*p(w)+l22*p(w|w1)+l23(p(w|w1,w2))
l21 <- .2
l22 <- .3
l23 <- 1-(l21+l22)

# function to cleanup user input and extract last terms
input_cleanup <- function(inputtxt,ngrams=3) {
        inputlist <- tokenizers::tokenize_words(inputtxt, strip_numeric=TRUE, simplify=TRUE)
        # build results 1-gram, 2-gram, 3-gram
        res <- list()
        for (i in 1:ngrams-1) {
                if (length(inputlist)>i) {
                        res[i+1] <- inputlist[length(inputlist)-i]
                }
        }
        return(res)
}

# process input
ngrams<- inputtxt %>% input_cleanup()

# predictions from 2-grams
pred2 <- df2split %>% rename(n_bigram=n) %>%
        filter(word1==ngrams[[1]]) %>%
        left_join(df1,by=c("word2"="word")) %>%
        mutate(
                 n_bigram = as.double(n_bigram)
                ,n = if_else(is.na(n),0.0,as.double(n))
        ) %>%
        mutate(
                 word2_prob=n/sum(n,na.rm = TRUE)
                ,word12_prob=n_bigram/sum(n_bigram,na.rm=TRUE)
        ) %>%
        mutate(word_prob=l11*word2_prob+l12*word12_prob) %>%
        filter(!is.na(word_prob)) %>%
        select(word=word2,word_prob)

pred3 <- df3split %>% rename(n_trigram=n) %>%
        filter(word1==ngrams[[2]] & word2==ngrams[[1]]) %>%
        left_join(df1,by=c("word3"="word")) %>%
        rename(word3_n=n) %>%
        left_join(df2split,by=c("word2"="word1","word3"="word2")) %>%
        rename(word23_n=n) %>%
        mutate(
                 n_trigram = as.double(n_trigram)
                ,word3_n = if_else(is.na(word3_n),0.0,as.double(word3_n))
                ,word23_n = if_else(is.na(word23_n),0.0,as.double(word23_n))
        ) %>%
        mutate(
                 word3_prob=word3_n/sum(word3_n,na.rm = TRUE)
                ,word23_prob=word23_n/sum(word23_n,na.rm = TRUE)
                ,word123_prob=n_trigram/sum(n_trigram,na.rm = TRUE)
        ) %>%
        mutate(
                word_prob = l21*word3_prob+l22*word23_prob+l23*word123_prob
        ) %>%
        filter(!is.na(word_prob)) %>%
        select(word=word3,word_prob)

predictions <- bind_rows(pred2,pred3) %>%
        group_by(word) %>% summarize(word_prob=sum(word_prob,na.rm=TRUE)) %>% ungroup() %>%
        top_n(n_predictions,word_prob)

# are we able to make a prediction using 2/3-grams?
# if not - just guess some words from d1 dictionary according to prob
if (nrow(predictions)==0) {
        predictions <- df1 %>% sample_n(n_predictions,weight=n) %>%
                mutate(word_prob = n/sum(n,na.rm=TRUE)) %>%
                select(word,word_prob)
}

library(wordcloud)
wordcloud(predictions$word,predictions$word_prob)
