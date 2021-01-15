
library(shiny)
library(dplyr)
library(stringr)
library(wordcloud)

load("datatidy_split.Rda")

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

predict_word<-function(inputtxt) {
    # process input
    ngrams<- inputtxt %>% input_cleanup()
    
    # predictions from 2-grams
    if (length(ngrams)>0) {
        pred2 <- df2split %>% rename(n_bigram=n) %>%
            filter(word1==ngrams[[1]]) %>%
            left_join(df1,by=c("word2"="word")) %>%
            mutate(
                 n_bigram = as.numeric(n_bigram)
                ,n = if_else(is.na(n),0.0,as.numeric(n))
            ) %>%
            mutate(
                 word2_prob=n/sum(n,na.rm = TRUE)
                ,word12_prob=n_bigram/sum(n_bigram,na.rm=TRUE)
            ) %>%
            mutate(word_prob=l11*word2_prob+l12*word12_prob) %>%
            filter(!is.na(word_prob)) %>%
            select(word=word2,word_prob)
    }

    if (length(ngrams)>1) {
        pred3 <- df3split %>% rename(n_trigram=n) %>%
            filter(word1==ngrams[[2]] & word2==ngrams[[1]]) %>%
            left_join(df1,by=c("word3"="word")) %>%
            rename(word3_n=n) %>%
            left_join(df2split,by=c("word2"="word1","word3"="word2")) %>%
            rename(word23_n=n) %>%
            mutate(
                 n_trigram = as.numeric(n_trigram)
                ,word3_n = if_else(is.na(word3_n),0.0,as.numeric(word3_n))
                ,word23_n = if_else(is.na(word23_n),0.0,as.numeric(word23_n))
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
    }
    predictions <- tibble(word=character(),word_prob=numeric())
    if (length(ngrams)>0) {
        predictions<-pred2
        if (length(ngrams)>1) {
            predictions<-bind_rows(pred2,pred3)
        }
        if (nrow(predictions)>0) {
            predictions <- predictions %>%
                group_by(word) %>% summarize(word_prob=sum(word_prob,na.rm=TRUE)) %>% ungroup() %>%
                top_n(n_predictions,word_prob)
        }
    }

    # are we able to make a prediction using 2/3-grams?
    # if not - just guess some words from d1 dictionary according to prob
    if (nrow(predictions)==0) {
        predictions <- df1 %>% sample_n(n_predictions,weight=n) %>%
            mutate(word_prob = n/sum(n,na.rm=TRUE)) %>%
            select(word,word_prob)
    }
    return(predictions %>% arrange(desc(word_prob)))
}
    

shinyServer(function(input, output) {

    getPredictions <- reactive({
        return(predict_word(input$sentence))
    })
    
    output$wordPlot<-renderPlot({
        wordcloud(getPredictions()$word,getPredictions()$word_prob)
    })
    
    output$uiOutputPanel <- renderUI({
        #                sentenceToParse <- input$sentence # Just the act of reading input$sentence will cause this block to re-execute
        #                predictions <- predict_word(sentenceToParse)
        word1 <- getPredictions()$word[1]
        button1Click <- paste("$('#sentence').val($('#sentence').val() + ' ", word1, "').trigger('change')", sep='')
        tags$button(type="button", id="word1", word1, class="btn action-button shiny-bound-input",onclick=button1Click)
    })
    
    output$sentenceEntered <- renderText({input$sentence})

})
