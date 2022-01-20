# Joel Modisette
# January 5, 2022

# This is a working script to accomplish tasks leading to 
# the first deliverable, the Exploratory Analysis Progress Report


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

# Take a peek

textFile <- "./final/en_US/en_US.twitter.txt"
con <- file(textFile, "r")
readLines(con, 5)
close(con)


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


# Look at File Sizes (Mb) 2^20 gives me size in MB

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

# Number of Lines per file. I want to tally up totals in the final table.

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

# Create the sample repository

repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)


#only keeping letters and apostrophies - no numbers or symbols

cleaned_repo_sample <- repo_sample %>%
  mutate(source = tolower(source)) %>%
  mutate(source = str_replace_all(source, "[^a-z']", " ")) %>% #replace all text with " " except lower case letters a-z 
  mutate(source = str_replace_all(source, " {2,}", " ")) #remove double spaces

# Create the n-grams

# unigrams first

uni_repo_sample <- cleaned_repo_sample %>% unnest_tokens(word, text) 
uni_repo_sample$source <- as.factor(uni_repo_sample$source)
uni_count <- uni_repo_sample  %>% 
  count(source, word, sort = TRUE) %>%
  group_by(source) %>%
  mutate(percent = n/sum(n)*100)

# bi-grams next

bigram_repo_sample <- cleaned_repo_sample %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigram_repo_sample$source <- as.factor(bigram_repo_sample$source) 
# now Trigrams  

trigram_repo_sample <- cleaned_repo_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  drop_na()
trigram_repo_sample$source <- as.factor(trigram_repo_sample$source) 

# Quad-grams

quadgram_repo_sample <- cleaned_repo_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
  drop_na()
quadgram_repo_sample$source <- as.factor(quadgram_repo_sample$source) 



# first plot of just top unigrams as a percentage of total.


uni_count %>% 
  group_by(source) %>%
  top_n(20, percent)  %>%
  ungroup() %>%
  mutate(word = reorder_within(word, -percent, source)) %>%
  ggplot( aes(x = word, y = percent)) +
  geom_col(show.legend = FALSE) +
  facet_grid(~source, scales = 'free') +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("word") +
  ylab("percent of total")

# All uniwords combined distribution, unwind the group stuff.

uni_repo_sample  %>% 
  count( word ) %>%
  mutate(percent = n/sum(n)*100) %>%
  top_n(20, percent)  %>%
  ggplot( aes(x = reorder(word, -percent), y = percent)) +
  geom_bar(stat = "identity") +
  xlab("word") +
  ylab("percent of total")

# All bi words distro

bigram_repo_sample  %>% 
  count( bigram) %>%
  mutate(percent = n/sum(n)*100) %>%
  top_n(20, percent)  %>%
  ggplot( aes(x = reorder(bigram, -percent), y = percent)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("bigram") +
  ylab("percent of total")

# All tri words distro

trigram_repo_sample  %>% 
  count(trigram) %>%
  mutate(percent = n/sum(n)*100) %>%
  top_n(20, percent)  %>%
  ggplot( aes(x = reorder(trigram, -percent), y = percent)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("trigram") +
  ylab("percent of total")

# all quad words distro

quadgram_repo_sample  %>% 
  count(quadgram) %>%
  mutate(percent = n/sum(n)*100) %>%
  top_n(20, percent)  %>%
  ggplot( aes(x = reorder(quadgram, -percent), y = percent)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("quadgram") +
  ylab("percent of total")
