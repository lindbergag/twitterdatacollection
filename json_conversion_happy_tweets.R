#Check whether necessary packages are installed. If not, install. Load required packages
if (!require("ndjson")) install.packages("ndjson")
library(ndjson)
if (!require("tm")) install.packages("tm")
library(tm)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)


# use stream_in() function from ndjson to create data frame/tibble
tweets.data <- stream_in('happytweets.json') %>%
  as_tibble() %>%
  # reduce dataset to the following variables
  select(created_at, text, user.description, user.favourites_count, user.followers_count, user.friends_count, user.location, user.name, user.screen_name, user.statuses_count, entities.user_mentions.0.screen_name, retweeted_status.user.screen_name, entities.urls.0.expanded_url, retweet_count) %>%
  # rename variables
  rename(tweet.datetime = created_at, total.favorites = user.favourites_count, total.followers = user.followers_count, total.friends = user.friends_count, screen.name = user.screen_name, total.statuses = user.statuses_count, tweet.mentions = entities.user_mentions.0.screen_name, retweet.mentions = retweeted_status.user.screen_name, url = entities.urls.0.expanded_url, total.retweets = retweet_count)

# common artifacts that remain after cleaning
other.words <- c("rt", "amp","htt")

# remove all urls
tweets.data$text <- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", tweets.data$text)

# clean data
tweets.data$text <- tweets.data$text %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>%
  removeWords(stopwords("SMART")) %>%
  removeWords(other.words) %>%
  stemDocument() %>%
  stripWhitespace()

# converts tweets.data to csv file
write.csv(tweets.data, file="happytweets.csv")

# arrange data frame according to most tweets by user, descending order
tweets.data <- tweets.data %>%
  arrange(desc(total.statuses))

# explore results
tweets.data %>%
  group_by(screen.name) %>%
  summarize(total_statuses = max(total.statuses)) %>%
  arrange(desc(total_statuses)) %>%
  top_n(50) -> user_list

# create a list of top 50 users to pass to MassMine's twitter-user task
user.list <- paste(as.character(user_list$screen.name[1:50]), collapse=", ")

user.list
