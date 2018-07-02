library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)
library(cetcolor)
library(tm)
library(wordcloud)
library(knitr)

survey = read.csv('cleaned_survey.csv', stringsAsFactors=FALSE)

#functions to transform words
map_words <- function(text){
  text %>% 
    VectorSource %>%
    Corpus %>%
    tm_map(tolower) %>%
    tm_map(removeWords, stopwords('english') )
}

comp.wordcloud <- function(text, scale=c(10,1), colors=c(cet_pal(12), '#F00101'), ...){
  
  wordcloud(text %>% map_words, colors=colors,
            scale=scale, random.order=FALSE, ...)
}

ntitle <- function(text, cex=1.5, n_newline=4){
  title(paste0(paste(rep('\n', n_newline), collapse=''), text), cex.main=cex)
}

set.seed(51)
comp.wordcloud(survey$three_words, scale=c(3,1), min.freq=2)
ntitle('How Americans Describe America')

comp.wordcloud((survey %>% filter(trump_support=='Approve'))$three_words, scale=c(3.3,1.1), min.freq=1)
ntitle('How Trump Supporters Describe America\n(n=27)', n_newline=2)


comp.wordcloud((survey %>% filter(trump_support=='Disapprove'))$three_words, scale=c(2.4,0.5), min.freq=1)
ntitle('How Americans who disapprove of Trump\nDescribe America (n=97)', n_newline=1)

comp.wordcloud((survey %>% filter(trump_support=='Neutral'))$three_words, scale=c(3,1.1), min.freq=1)
ntitle('How Americans who have no strong\nopinions of Trump Describe America\n(n=13)', n_newline=1)
