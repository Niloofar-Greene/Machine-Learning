
#load packages
library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)

# Read animal crossing user review data - TSV : Tab Separated Values
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')

user_reviews %>% count(user_name, sort=TRUE)
nrow(data.frame(user_reviews$user_name))

user_reviews %>% head(10) %>% pull(text)
pull(head(user_reviews,10), text)

user_reviews %>% select(date)
select(user_reviews, date)
user_reviews$date

user_reviews %>% unnest_tokens(output=word, input=text) %>%
count(word, sort = TRUE)

# STOP WORDS   
stop_words

user_reviews %>% unnest_tokens(output=word, input=text) %>%
anti_join(stop_words, by="word") %>%
count(word, sort = TRUE)

#Filter : getting rid of words that
#[[:alnum:]] matches alphabets or digits
#[[:alpha:]] matches alphabets
#[[:digit:]] matches digits

review_word <- user_reviews %>% unnest_tokens(output=word, input=text) %>%
anti_join(stop_words, by="word") %>%
# keeps words with at least one alphabetical character so number only are removed   
filter(str_detect(word, "[:alpha:]")) %>% 
distinct() # to avoid double counting for the same user - this removes duplicate rows

#Correlation between words and user_reviews
review_word$word[order(review_word$word)]
sort(review_word$word)

user_who_mention_word <- review_word %>% count(word, name= "users_n") %>%
  filter(users_n>=100)

word_correlation <- review_word %>%
  semi_join(user_who_mention_word, by="word") %>%
  pairwise_cor(item=word, feature = user_name) %>%
  filter(correlation >=0.2)

graph_from_data_frame(d=word_correlation, vertices=user_who_mention_word) %>%
  ggraph(layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name))

# clean up the plot
graph_from_data_frame(d=word_correlation
                      , vertices=user_who_mention_word %>%
                        semi_join(word_correlation, by=c("word"="item1"))) %>%
  ggraph(layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), repel= TRUE) # repel for removing word overlapping
  
# even better
graph_from_data_frame(d=word_correlation
                      , vertices=user_who_mention_word %>%
                        semi_join(word_correlation, by=c("word"="item1"))) %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(alpha= correlation)) +
  geom_node_point() +
  geom_node_text(aes(color=users_n,label=name), repel= TRUE) 

# positive and negative reviews
# function to generate word graph
generate_word_graph <- function(review_word,
                                min_users_n = 100,
                                min_corr = 0.2) {
  
  user_who_mention_word <- review_word %>% count(word, name= "users_n") %>%
    filter(users_n>=min_users_n)
  
  word_correlation <- review_word %>%
    semi_join(user_who_mention_word, by="word") %>%
    pairwise_cor(item=word, feature = user_name) %>%
    filter(correlation >=min_corr)
  
  graph_from_data_frame(d=word_correlation
                        , vertices=user_who_mention_word %>%
                          semi_join(word_correlation, by=c("word"="item1"))) %>%
    ggraph(layout = 'fr') +
    geom_edge_link(aes(alpha= correlation)) +
    geom_node_point() +
    geom_node_text(aes(color=users_n,label=name), repel= TRUE)
}

review_word %>% generate_word_graph(min_users_n=100,min_corr=0.2)
generate_word_graph(review_word,min_users_n=200,min_corr=0.3)

review_word %>% generate_word_graph(min_users_n=50,min_corr=0.4)

# grade is the review point - we imagine anything below 7 is bad review
user_reviews %>% count(grade)

review_word.neg <- review_word %>% filter(grade < 5)

review_word.pos <- review_word %>% filter(grade >= 5)

#graph
review_word.neg %>% 
  generate_word_graph(min_users_n=40,min_corr=0.2)

review_word.pos %>% 
  generate_word_graph(min_users_n=40,min_corr=0.25)


