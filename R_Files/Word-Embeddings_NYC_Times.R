library(tidyverse)
library(tm)
library(text2vec)

##########################################################
# explanation of GloVE:
# https://youtu.be/InCWrgrUJT8
#
# explanation of word2vec:
# https://youtu.be/QyrUentbkvw
#
# original GloVe paper:
# https://nlp.stanford.edu/projects/glove/
#
# tutorial used for this code example:
# http://text2vec.org/glove.html
#
# additional readings:
# https://www.skynettoday.com/overviews/neural-net-history
###########################################################

# read in csv file as tibble/data frame
scrape.data <- read.csv(file='NYCTimes.csv', stringsAsFactors=FALSE)

names(scrape.data)
###################
# pre-process text
###################

# use toString to transform vector of text documents to a single string of words
bulk.text <- toString(scrape.data$Text)

# use TM package to clean the dataset
clean.text <- bulk.text %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>%
  removeWords(stopwords("SMART")) %>%
  stripWhitespace()

# Create iterator over tokens
tokens = space_tokenizer(clean.text)

# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)

# reduce vocabulary to words with a minimum frequency of 5
vocab = prune_vocabulary(vocab, term_count_min = 5L)

# show number of words in vocab
length(vocab$term)

#####################################
# construct term co-occurance matrix
#####################################

# Use our filtered vocabulary
vectorizer = vocab_vectorizer(vocab)

# use window of 5 for context words
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

# fitting our model using all available cores for processing
glove = GlobalVectors$new(rank = 50, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)

# explore
dim(wv_main)

# take a sum of main and context vector
wv_context = glove$components
word_vectors = wv_main + t(wv_context)

#######################################
# explore context for individual words
#######################################

cos_sim = sim2(x = word_vectors, y = word_vectors["books", , drop = FALSE], method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

###############################
# test word contexts/analogies
###############################

test.word = word_vectors["books", , drop = FALSE] -
  word_vectors["brush", , drop = FALSE] +
  word_vectors["chair", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = test.word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)