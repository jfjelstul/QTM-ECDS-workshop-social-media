###########################################################################
# Josh Fjelstul, Ph.D.
# QTM-ECDS Workshops, Coding in R Series
# Social Media
###########################################################################

# library
library(stringr)
library(dplyr)
library(tidyr)
library(rtweet)
library(quanteda)
library(topicmodels)

###########################################################################
# the Twitter API (rtweet)
###########################################################################

# create_token(
#   app = "",
#   consumer_key = "",
#   consumer_secret = "",
#   access_token = "",
#   access_secret = ""
# )

# get current rate limits
limits <- rate_limit()

# API query
# only returns tweets from the last 6-9 days
# you can only get 18,000 tweets in a single call
# to get more, you have to re-run the query every time your rate limit resets (15 minutes)
# you can exclude retweets
query_tweets <- search_tweets("data science", n = 100, include_rts = FALSE)

# may return fewer tweets than you ask for
# (1) there are fewer results (in the last 6-8 days) than the number you ask for (specific queries)
# (2) you hit the rate limit
# (3) tweets are suspended or deleted

# you can tell rtweet to automatically re-run the query
# you have to let R run the whole time (may need a dedicated instance of R)
query_tweets <- search_tweets("data science", n = 40000, include_rts = FALSE, retryonratelimit = TRUE)

# take a look at the variables
names(query_tweets)

# stream tweets
# returns a small random sample of all publically available tweets
# can filter by query, user ID (up to 500 users), or location (geo coordinates)
# no information about how big a sample you're getting
stream_tweets <- stream_tweets("", timeout = 30)

# stream tweets from Atlanta for 30 seconds
# requires a Google Maps API key
stream_tweets_atlanta <- stream_tweets(lookup_coords("atlanta, usa"), timeout = 30)

# get a user's friends
# returns a data frame with user IDs
# up to 5000 friends per requested user (most users can only have 5000 friends)
# up to 15 users at a time
# can set to retry when rate limit resets
cnn_friends <- get_friends("cnn")

# get a user's followers
# returns a data frame with user IDs
cnn_followers <- get_followers("cnn", n = 100)

# look up metadata for a set of users
# takes a vector of user IDs or screen names
# returns user metadata and a user's last tweet 
cnn_followers_metadata <- lookup_users(cnn_followers$user_id)

# get a user's timeline
# up to 3,200 tweets for each ID or screen name
# set home = TRUE to get a user's home timeline feed 
# (includes tweets a user posts and tweets posted by accounts that a user follows)
cnn_timeline <- get_timelines("cnn", n = 1000)

# get trends in a location
# takes a WOEID (Yahoo! Where on Earth ID) or lattitude/longitude coordinates
# can exclude hashtags
trends_atlanta <- get_trends("atlanta")

# get a list of places you can get trends for
trends <- trends_available()

# get a user's favorites
# up to 3000 tweets favorited by a user
# will work on multiple users
cnn_favorites <- get_favorites("cnn", n = 100)

###########################################################################
# clean API data
###########################################################################

# get the last 1000 CNN tweets
cnn_timeline <- get_timelines("cnn", n = 1000)

# select variables
cnn_timeline <- select(cnn_timeline, user_id, screen_name, text, 
                       is_retweet, favorite_count, retweet_count, hashtags, 
                       reply_to_screen_name, mentions_screen_name, retweet_screen_name)

# check class
class(cnn_timeline$hashtags)
class(cnn_timeline$mentions_screen_name)

# clean hashtags variable
cnn_timeline$hashtags <- cnn_timeline$hashtags %>% 
  as.character() %>% 
  str_replace("^c", "") %>%
  str_replace_all("[\"()]+", "")

# clean mentions variable
cnn_timeline$mentions_screen_name <- cnn_timeline$mentions_screen_name %>% 
  as.character() %>% 
  str_replace("^c", "") %>%
  str_replace_all("[\"()]+", "")

###########################################################################
# clean tweets using regular expressions
###########################################################################

# set your working directory to the root folder for this workshop

# read in data
cnn_timeline <- read.csv("data/cnn-timeline.csv", stringsAsFactors = FALSE)

# remove numbers
cnn_timeline$text <- str_replace_all(cnn_timeline$text, "[[:digit:]]+", " ")

# remove punctuation
cnn_timeline$text <- str_replace_all(cnn_timeline$text, "[[:punct:]]+", " ")

# remove URLs
cnn_timeline$text <- str_replace(cnn_timeline$text, "http.*", " ")

# remove hashtags (may want to keep these)
cnn_timeline$text <- str_replace(cnn_timeline$text, "@[A-Za-z0-9]+", " ")

# clean spaces
cnn_timeline$text <- str_replace_all(cnn_timeline$text, "[[:space:]]+", " ")

# trim white space
cnn_timeline$text <- str_trim(cnn_timeline$text)

# convert to lower case
cnn_timeline$text <- str_to_lower(cnn_timeline$text)

###########################################################################
# clean tweets using quanteda
###########################################################################

# set your working directory to the root folder for this workshop

# read in data
cnn_timeline <- read.csv("data/cnn-timeline.csv", stringsAsFactors = FALSE)

# create a text corpus
corpus <- corpus(cnn_timeline$text, docnames = cnn_timeline$screen_name)

# tokenize corpus
tokens <- tokens(corpus, ngrams = 1,
                 remove_numbers = TRUE, 
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_twitter = TRUE, 
                 remove_hyphens = TRUE, 
                 remove_url = TRUE)

# create a document-feature matrix
dfm <- dfm(tokens, tolower = TRUE, stem = TRUE, remove = stopwords("en"))

# check dimensions of DFM
dim(dfm)

# run a topic model (Latent Dirichlet Allocation)
out <- LDA(convert(dfm, to = "topicmodels"), k = 10)

# see topics
as.matrix(terms(out, 10))

# add topics to data frame
cnn_timeline$topic <- topics(out)

###########################################################################
# end R script
###########################################################################