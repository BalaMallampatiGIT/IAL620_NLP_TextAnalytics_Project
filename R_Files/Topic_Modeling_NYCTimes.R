library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(ldatuning)
library(lubridate)

##############################################################################################
# please watch the following video for a description of LDA:
# https://youtu.be/DWJYZq_fQ2A
#
# this tutorial translates code examples from chapter 6 of *Text Mining with R*
#
# if you have trouble installing 'topicmodels' package on linux or mac:
# sudo apt-get install gsl-bin libgsl0-dev
#
# See also:
# https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# https://mran.microsoft.com/snapshot/2016-08-05/web/packages/ldatuning/vignettes/topics.html
##############################################################################################

# read in csv file as tibble/data frame
scrape.data <- read.csv(file='NYCTimes.csv', stringsAsFactors=FALSE)

# remove list of non-stop words that are common and hold no semantic value
other.words <- c("Advertisement","Supported", "by", "SUBSCRIBER", "NEWSLETTER", "Wirecutter", "reader-supported",
                 "Times", "games", "day", "You", "he", "his", "Are", "they",
                 "has", "person", "jacob", "person", "thoma", "minute", "noun")

# clean data
clean.data <- scrape.data$Text %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>%
  removeWords(stopwords("SMART")) %>%
  removeWords(other.words) %>%
  stripWhitespace()

# convert vector of scraped text to document term matrix using TM package
scrape.dtm <- VCorpus(VectorSource(clean.data)) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace) %>%
  DocumentTermMatrix()

#######################################################################
# examples using scraped data and chapter 6.1.1 of Text Mining with R
#######################################################################

# create a 2-topic LDA model and set seed so that output is predictable
scrape.lda <- LDA(scrape.dtm, k = 2, control = list(seed = 620))

# explore
scrape.lda

# extract per topic word probabilities
scrape.topics <- tidy(scrape.lda, matrix = "beta")

# explore
scrape.topics

# organize data according to 'topic' for figure 6.2
scrape.top.terms <- scrape.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# vizualize top 2 topics and their top 10 terms
# figure 6.2 in Text Mining with R
scrape.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# calculate beta spread for figure 6.3
beta.spread <- scrape.topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

# visualize words with greatest beta spread between 2 topics
# figure 6.3 in Text Mining with R
beta.spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill=factor(term))) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

###############################################################
# decide number of topics intuitively or based on outside data
###############################################################

# reusing code from chapter 6.1.1 in Text Mining with R

# set seed and select number of topics with 'k'
scrape.lda <- LDA(scrape.dtm, k = 28, control = list(seed = 620))

# extract per topic word probabilities
scrape.topics <- tidy(scrape.lda, matrix = "beta")

# organize data according to 'topic'
scrape.top.terms <- scrape.topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# visualize top 5 terms for each topic
scrape.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

########################################
# deciding number of topics empirically
########################################

# calculate metrics according to models
result <- FindTopicsNumber(
  scrape.dtm,
  topics = seq(from = 2, to = 40, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 14L,
  verbose = TRUE
)

# visualize results
FindTopicsNumber_plot(result)