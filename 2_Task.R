# Joel Modisette
# January 12, 2022

# This is a working script to accomplish tasks leading to 
# the first deliverable, the Exploratory Analysis Progress Report

# will need the files created from task one

rm(list = ls())
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(ngram)
  library(gt)
  library(ggplot2)
})

# Download and unzip the data to the working directory

#link <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
#rawDataFilename <- "rawData.zip"
#download.file(url = link, destfile = rawDataFilename)
#unzip(zipfile = rawDataFilename)


# Just need the English files

enTwitterFile <- "./final/en_US/en_US.twitter.txt"
enBlogsFile <- "./final/en_US/en_US.blogs.txt"
enNewsFile <- "./final/en_US/en_US.news.txt"

con <- file(enTwitterFile, "r")
enTwitterData <- readLines(con)
close(con)

con <- file(enBlogsFile, "r")
enBlogsData <- readLines(con)
close(con)

con <- file(enNewsFile, "r")
enNewsData <- readLines(con)
close(con)


# Look at File Sizes (2^20 = Mb)

blogs_size   <- file.size(enBlogsFile) / (2^20)
news_size    <- file.size(enNewsFile) / (2^20)
twitter_size <- file.size(enTwitterFile) / (2^20)
file_sizes <- c(blogs_size, news_size, twitter_size)
file_sizes <- c(blogs_size, news_size, twitter_size, sum(file_sizes))

# Read the data files

blogs   <- read_lines(enBlogsFile)
news    <- read_lines(enNewsFile)
twitter <- read_lines(enTwitterFile) 
file_names = c("blogs", "news", "twitter", "Totals")

# Number of Lines per file

blogs_lines   <- length(blogs)
news_lines    <- length(news)
twitter_lines <- length(twitter)
total_lines   <- blogs_lines + news_lines + twitter_lines
file_lines <- c(blogs_lines, news_lines, twitter_lines)
pct_num_lines = round(file_lines/sum(file_lines), 2)
file_lines <- c(file_lines, sum(file_lines))
pct_num_lines <- c(pct_num_lines, sum(pct_num_lines))

# Total Char in each file

blogs_nchar_sum   <- sum(nchar(blogs))
news_nchar_sum    <- sum(nchar(news))
twitter_nchar_sum <- sum(nchar(twitter))
num_char <- c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum)
pct_num_char = round(num_char/sum(num_char), 2)
num_char <- c(num_char, sum(num_char))
pct_num_char <- c(pct_num_char, sum(pct_num_char))

# Total words per file

blogs_words <- wordcount(blogs, sep = " ")
news_words  <- wordcount(news,  sep = " ")
twitter_words <- wordcount(twitter, sep = " ")
num_words <- c(blogs_words, news_words, twitter_words )
pct_num_words = round(num_words/sum(num_words), 2)
num_words <- c(num_words, sum(num_words) )
pct_num_words <- c(pct_num_words, sum(pct_num_words))

# Summarize the stats
summary <- data.frame(file_names, file_sizes, file_lines, num_char, num_words,
                      pct_num_char, pct_num_lines, pct_num_words)
gt(summary)

# these files are large. Sample 5% of them.

#' Read the data files into dataframes
blogs   <- tibble(blogs) %>% rename(text = blogs)
news    <- tibble(news) %>% rename(text = news)
twitter <- tibble(twitter) %>% rename(text = twitter)

set.seed(1001)
sample_pct <- 0.05

blogs_sample <- blogs %>%
  sample_n(., nrow(blogs)*sample_pct)
news_sample <- news %>%
  sample_n(., nrow(news)*sample_pct)
twitter_sample <- twitter %>%
  sample_n(., nrow(twitter)*sample_pct)

#' Create tidy repository
repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)


#only keeping letters and apostrophies - no numbers or symbols

cleaned_repo_sample <- repo_sample %>%
  mutate(text = tolower(text)) %>%
  mutate(text = str_replace_all(text, " ?@\\w+ ?", "")) %>%
  mutate(text = str_replace_all(text, "[^a-z']", " ")) %>% #remove non-alpha characters 
  mutate(text = str_replace_all(text, " {2,}", " ")) #remove double spaces

# Create the n-grams and then filter out all except where count n > 10
# This reduces the searchtime for the app by limiting to around 50k rows

# unigrams first

uni_repo_sample <- cleaned_repo_sample %>% unnest_tokens(word, text) 
uni_repo_sample$source <- as.factor(uni_repo_sample$source)

# bi-grams next
# ok filter these down to all above n> 10, otherwise its just too big to search

bigram_repo_sample <- cleaned_repo_sample %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  drop_na()

bigram_top_ten <- bigram_repo_sample %>% 
  count(bigram) %>% 
  filter(n > 10) %>% 
  arrange(desc(n))

bigram_for_app <- bigram_top_ten %>% 
  separate(bigram, c("word1", "word2"), sep = " ")


# now Trigrams  
# open up the filter here, n > 5 gives a nice 50k row + file

trigram_repo_sample <- cleaned_repo_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  drop_na()

trigram_top_ten <- trigram_repo_sample %>% 
  count(trigram) %>% 
  filter(n > 5) %>% 
  arrange(desc(n))

trigram_for_app <- trigram_top_ten %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
# trigram_repo_sample$source <- as.factor(trigram_repo_sample$source) 

# Quad-grams. We need all of them, do not filter.

quadgram_repo_sample <- cleaned_repo_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
  drop_na()

quadgram_top_ten <- quadgram_repo_sample %>% 
  count(quadgram) %>% 
  filter(n > 5) %>% 
  arrange(desc(n))

quadgram_for_app <- quadgram_top_ten %>% 
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

# this gets saved for rapid searches in the R Shiny app

saveRDS(bigram_for_app, "./JDModisetteShiny/app_data/bigram_for_app.rds")
saveRDS(trigram_for_app, "./JDModisetteShiny/app_data/trigram_for_app.rds")
saveRDS(quadgram_for_app,"./JDModisetteShiny/app_data/quadgram_for_app.rds")
