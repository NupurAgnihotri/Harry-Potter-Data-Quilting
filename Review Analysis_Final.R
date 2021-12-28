library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)

data<-read.csv("/Users/nipunarora/Downloads/Review.csv")
data
review_words <- data %>%
  unnest_tokens(output=word, input=Review) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[:alpha:]")) %>%
  distinct()

mention_words<- review_words %>%
    count(word, name= "users_n") %>%
    filter(users_n >= 3)
  
  word_correlations <- review_words %>%
    semi_join(mention_words, by = "word") %>%
    pairwise_cor(item=word, feature  = Name) %>% 
    filter(correlation >=0.2)
  

  
  generate_word_graph<- function(review_words, minimum_users_n = 3, minimum_correlation = 0.2) 
    {mention_words<- review_words %>%
    count(word, name= "users_n") %>%
    filter(users_n >= minimum_users_n)
  
  word_correlations <- review_words %>%
    semi_join(mention_words, by = "word") %>%
    pairwise_cor(item=word, feature  = Name) %>% 
    filter(correlation >= minimum_correlation)
  
    graph_from_data_frame(d = word_correlations,
                                      vertices = mention_words %>%
                      semi_join(word_correlations, by=c("word" = "item1"))) %>%
      
      ggraph(layout = "fr") +
      geom_edge_link(aes(alpha = correlation)) +
      geom_node_point() +
      geom_node_text(aes(color = users_n, label = name), repel = TRUE) 
    
  }
  review_words %>%
    generate_word_graph(minimum_users_n = 3, minimum_correlation = 0.24)
   
   
data %>% count(Review.Type)

data1 <- data.frame(
  Reviews=c("Positive","Negative","Neutral") ,  
  Users=c(139, 37, 27)
)

# Barplot
ggplot(data1, aes(x=Reviews, y=Users, fill = Reviews)) + 
  geom_bar(stat = "identity") + scale_fill_manual(values = c("#FF7377", "lightblue", "lightgreen") ) 
    
  
 


  
