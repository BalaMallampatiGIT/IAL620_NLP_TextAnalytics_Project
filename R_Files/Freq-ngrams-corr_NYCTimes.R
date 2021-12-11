library(tidyverse)
library(tidytext)
library(ggplot2)
library(tm)
library(stringr)
library(igraph)
library(ggraph)
library(widyr)
scrape.data <- read.csv(file='NYCTimes.csv', stringsAsFactors=FALSE) %>%
  as_tibble()
# removes carrige returns and new lines from text
scrape.data$Text <- gsub("\r?\n|\r", " ", scrape.data$Text)

# removes punctuation
scrape.data$Text <- gsub("[[:punct:]]", "", scrape.data$Text)

# forces entire corpus to lowercase
scrape.data$Text <- tolower(scrape.data$Text)

#removes numbers from text
scrape.data$Text <- removeNumbers(scrape.data$Text)

# remove stop words
scrape.data$Text <- removeWords(scrape.data$Text, stopwords("SMART"))

#scrape.data$Text
# remove additional words
other.words <- c("it's", "l","time", "ms", "minutes", "times", "million")

scrape.data$Text <- removeWords(scrape.data$Text, other.words)

# removes additional remaining whitespace
scrape.data$Text <- stripWhitespace(scrape.data$Text)

clean.data <- scrape.data %>%
  unnest_tokens(word, Text)

clean.data <- clean.data %>%
  count(word, sort = TRUE)

url.words <- scrape.data %>%
  unnest_tokens(word, Text) %>%
  count(URL, word, sort = TRUE)

total.words <- url.words %>%
  group_by(URL) %>%
  summarize(total = sum(n))

url.words <- left_join(url.words, total.words)

url.words <- url.words %>%
  bind_tf_idf(word, url, n)

url.words <- url.words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# compare tf-idf to raw word frequencies for top 30 words
unique(url.words$word[1:30])
clean.data$word[1:30]

# raw bigrams
url.bigrams <- scrape.data %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)

url.bigrams %>%
  count(bigram, sort = TRUE)

# tf-idf bigrams
bigrams.separated <- url.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram.tf.idf <- url.bigrams %>%
  count(URL, bigram) %>%
  bind_tf_idf(bigram, URL, n) %>%
  arrange(desc(tf_idf))

clean.data$word[1:30]
unique(url.words$word[1:30])
bigram.tf.idf$bigram[1:30]

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# bring in your text data
viz.bigrams <- scrape.data %>%
  count_bigrams()

# filter out rare combinations, as well as digits
viz.bigrams %>%
  filter(n > 9,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  
  # display graph
  visualize_bigrams()


# transform data to one-token-per-document-per-row
clean.data2 <- scrape.data %>%
  unnest_tokens(word, Text)

# count words co-occurring in web pages
word_cors <- clean.data2 %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, URL, sort = TRUE)

# explore top word correlations
word_cors

word_cors %>%
  filter(item1 == "prize")


word_cors %>%
  filter(item1 %in% c("prize", "nobel", "chun", "bureau", "labor", "child")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation,fill=item1)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2021)
unique(word_cors$correlation)
word_cors %>%
  filter(correlation > .7) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightgreen", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
