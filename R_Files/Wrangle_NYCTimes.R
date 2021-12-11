####################################################
# Basic Janitorial and Wrangling Tutorial
# Based on chapter 1 from *Text Mining with R*
####################################################

library(tidyverse)
library(tidytext)
library(ggplot2)
#install.packages("plotly") #Plotly package installation for interactive reporting
library(plotly) #Library to load plots

####################################################
# Tidytext method of data janitorial work with text
# from Chapter 1 of *Text Mining with R*
####################################################

# read in csv file as tibble/data frame from NYC timesdata
scrape.data <- read.csv(file='NYCTimes.csv', stringsAsFactors=FALSE)

clean.data <- as_tibble(scrape.data) #Converted to R Tibble
 
drop_na(clean.data) # Drop any NA rows
dim(clean.data) #Dimensions of NYC times data 64 rows and 5columns
str(clean.data) #NYCTimes data structure to check column types

# transform table into one-word-per-line tidytext format from Text column data
clean.data <- clean.data %>%
  unnest_tokens(word, Text)

# most frequent words
clean.data %>%
  count(word, sort = TRUE)

# remove stop words
data(stop_words)
clean.data <- clean.data %>%
  anti_join(stop_words)

# check result of stop word removal
clean.data %>%
  count(word, sort = TRUE)

# remove numbers -- NOT from *Text Mining with R*
nums <- clean.data %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()

clean.data <- clean.data %>%
  anti_join(nums, by = "word")

# remove other words -- NOT from *Text Mining with R* #below words not for analysis
uni_sw <- data.frame(word = c("it's", "l","time", "ms", "minutes", "times", "million"))

clean.data <- clean.data %>%
  anti_join(uni_sw, by = "word") #Removed other words from dataframe

# visualize top words in corpus
clean.dataPlot <- clean.data %>%
  count(word, sort = TRUE) %>%
  filter(n > 80) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n,fill=word), ) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplotly(clean.dataPlot) #Interactive plot with top words
