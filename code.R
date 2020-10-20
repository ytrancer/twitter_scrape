library(twitteR)

consumer_key <- "xxxx"
consumer_secret <- "xxxx"
access_token <- "xxxx"
access_secret <- "xxxx"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

library(tidyverse)
library(tidytext)
library(twitteR)
library(wordcloud2)

fn_twitter <- searchTwitter("keyword_to_search",n=1000,lang="en") # Specify a word to search

fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame

# custom stop words

my_stopwords <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt")))

tweet_words <- fn_twitter_df %>% 
              select(id, text) %>% 
              unnest_tokens(word,text) %>% 
  mutate(word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = NA)) %>% 
  drop_na() %>% 
  anti_join(my_stopwords)

tweet_words %>% count(word,sort=T) %>% 
                slice(1:20) %>% 
                ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + 
                         geom_bar(stat = "identity") + 
                         theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")

word_counts <- tweet_words %>% 
  group_by(word) %>% 
  summarise(Total = n()) %>%  
  arrange(desc(Total)) %>% 
  filter(Total > 1)

wordcloud2(word_counts)
