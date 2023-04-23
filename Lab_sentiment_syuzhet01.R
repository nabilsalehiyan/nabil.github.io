# Knowledge Mining: Text mining
# File: Lab_sentiment_syuzhet01.R
# Theme: Running sentiment anlaysis using syuzhet package
# Data: Twitter data via REST API

# Sample program for using rtweet, syuzhet for sentiment analysis
# Be sure you get Twitter developer account

install.packages(c("easypackages","rtweet","tidyverse","RColorBrewer","tidytext","syuzhet"))
library(easypackages)
libraries("rtweet","tidyverse","RColorBrewer","tidytext","syuzhet")

# Use rtweet to collect Twitter data via API
# Prerequisite: Twitter developer account

## Required package: rtweet
# Create token for direct authentication to access Twitter data

#token <- rtweet::create_token(
#  app = "Your App name",
#  consumer_key <- "YOURCONSUMERKEY",
#  consumer_secret <- "YOURCONSUMERSECRET",
#  access_token <- "YOURACCESSTOKEN",
#  access_secret <- "YOURACCESSSECRET")

## Check token
rtweet::get_token()

# Collect data from Twitter using keyword "Taiwan"
tw <- search_tweets("Taiwan", n=1000)


# Sentiment analysis
twtweets <- iconv(tw$text) # convert text data encoding
tw_sent_nrc <- get_nrc_sentiment(twtweets) # Get sentiment scores using NRC lexicon
barplot(colSums(tw_sent_nrc),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets of "Taiwan"')

tw_sent <- get_sentiment(twtweets) # Get sentiment scores 
plot(tw_sent, pch=20, cex = .3, col = "blue")


