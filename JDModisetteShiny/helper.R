#
# Joel Modisette
# Jan 14, 2021
# Coursera Capstone R Shiny App helper.r


# The optimized search files are saved in ./app_data

bigram_for_app   <-  readRDS("./app_data/bigram_for_app.rds")
trigram_for_app  <-  readRDS("./app_data/trigram_for_app.rds")
quadgram_for_app <-  readRDS("./app_data/quadgram_for_app.rds")

#' Create Ngram Matching Functions
bigram_finder <- function(input_words){
  
  num <- length(input_words)
  out <-  bigram_for_app %>%
             filter(word1==input_words[num]) %>% 
             top_n(1, n) %>%
             filter(row_number() == 1L) %>%
             select(num_range("word", 2)) %>%
             as.character()
  ifelse(out =="character(0)", "I don't have that word, try again.", return(out))
}

trigram_finder <- function(input_words){
  
  num <- length(input_words)
  out <-  trigram_for_app %>%
             filter(word1==input_words[num-1], word2==input_words[num])  %>% 
             top_n(1, n) %>%
             filter(row_number() == 1L) %>%
             select(num_range("word", 3)) %>%
             as.character()
  ifelse(out=="character(0)", bigram_finder(input_words), return(out))
}

quadgram_finder <- function(input_words){
  
  num <- length(input_words)
  out <-  quadgram_for_app%>% 
             filter(word1==input_words[num-2],  word2==input_words[num-1], 
                    word3==input_words[num])  %>% 
             top_n(1, n) %>%
             filter(row_number() == 1L) %>%
             select(num_range("word", 4)) %>%
             as.character()
  ifelse(out=="character(0)", trigram_finder(input_words), return(out))
}

# This is the main function that calls the others above.
anygram_search <- function(input){
  
  input <- data_frame(text = input)
  
  input <- input %>%
    mutate(text = str_replace_all(text, "[^[:alpha:][:space:]]*", ""))
  
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)

  # Call the matching functions
  
  #Ok I couldn't get this to work as I wanted.
#  out <- case_when(
#    input_count == 0 ~ "Please provide a sentence",
#    input_count == 3 ~ quadgram_finder(input_words),
#    input_count == 2 ~ trigram_finder(input_words),
#    TRUE ~             bigram_finder(input_words)
#  )
  
  
   out <- ifelse(input_count == 0, "Please provide a short sentence.",
               ifelse(input_count == 3, quadgram_finder(input_words),
                      ifelse(input_count == 2, trigram_finder(input_words), 
                             bigram_finder(input_words))))
  
  # Output
  return(out)
}
