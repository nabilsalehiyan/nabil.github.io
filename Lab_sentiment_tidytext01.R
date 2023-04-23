# Knowledge Mining: Text mining
# Date: 3/8/2023
# File: Lab_sentiment_tidytext01.R
# Theme: Running sentiment anlaysis using tidytext package
# Data: Twitter data via REST API

install.packages(c("easypackages","rtweet","tidyverse","RColorBrewer","tidytext","syuzhet", "plotly"))
library(easypackages)
libraries("rtweet","tidyverse","RColorBrewer","tidytext","data.table","tidyr", "plotly")

## 
tw <- search_tweets("taiwan", n=100, retryonratelimit = TRUE)
covid_HK <- search_tweets("COVID Hong Kong", n=1000, retryonratelimit = TRUE)

# Plot by time

ts_plot(covid_HK,"mins",cex=.25,alpha=1) +
  theme_bw() +
  theme(text = element_text(family="Palatino"),
        plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0.5)) +
  labs(title = "Frequency of keyword 'COVID Hong Kong' used in last 100,000 Twitter tweets",
       subtitle = "Twitter tweet counts aggregated per minute interval ",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet",hjust = 0.5)

# Preprocess text data
hkcovidtxt= covid_HK$text
# twtxt = tw$text
textDF <- tibble(txt = covid_HK$text)
tidytwt= textDF %>% 
  unnest_tokens(word, txt)
tidytwt <- tidytwt %>%  anti_join(stop_words) # Removing stopwords

tidytwt %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab("Keyword") + ylab("Count") +
  coord_flip() + theme_bw()

tidytwt <- tidytwt %>%
  mutate(linenumber = row_number()) # create linenumber

# Joining bing lexicon using on average tweet of 12 words.

sentiment_tw <- tidytwt %>%          
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 12, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(sentiment_tw, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)+theme_bw()


sentiment_tw$posneg=ifelse(sentiment_tw$sentiment>0,1,ifelse(sentiment_tw$sentiment<0,-1,0))

# Use Plotly library to plot density chart
ggplot(sentiment_tw, aes(sentiment, fill = posneg)) + 
  geom_density(alpha = 0.5, position = "stack") + 
  ggtitle("stacked sentiment density chart")+theme_bw()


bing_word_counts <- tidytwt %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Sentiments toward Taiwan, March 9, 2022",
       x = NULL) +
  coord_flip() + theme_bw()+ theme(strip.text.x = element_text(family="Palatino"), 
                                   axis.title.x=element_text(face="bold", size=15,family="Palatino"),
                                   axis.title.y=element_text(family="Palatino"), 
                                   axis.text.x = element_text(family="Palatino"), 
                                   axis.text.y = element_text(family="Palatino"))

