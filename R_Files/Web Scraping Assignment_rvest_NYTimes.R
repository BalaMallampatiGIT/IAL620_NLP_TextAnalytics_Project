# Whenever you see libraries listed at the top of code, you should always make sure that you have them installed. To install a library--or a 'package' as they are often called--use the install.packages() function in R.
library(rvest)
library(tidyverse)
library(stringr) # to modify string 

# This is the primary URL from which you will extract other URLs containing content of interest
main.url <- read_html("https://www.nytimes.com/") # Taken nytimes for webscrap

# Using the selector gadget, identify the URLs of interest on the page, and then copy the xpath for pasting into th html_nodes function. By piping the output into html_attr() using "href," we collect just the URLs from the links identified using the selector gadget.
scrape.list <- html_nodes(main.url,xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "css-v967z2", " " ))]') %>%
  # Another variation without the pipe: scrape.list <- html_attr(scrape.list, "href")
  # By using the %>% we avoid the needless copy/paste to modify the variable containing our data
  html_attr("href")

scrape.list <- unique(scrape.list) # removed duplicate URLs
scrape.list # verified all URLs



# Creates an empty vector that will be filled data by the 'for loop' below for Title, Author and Text
page.title <- vector()
page.date <- vector()
page.text <- vector()


# The for loop visits each URL in scrape.list and then collects the text content from each page, creating a new list
for (i in seq_along(scrape.list)) {
  new.url <- read_html(scrape.list[i])
  
  #Collects text content from pages
  text.add <- html_nodes(new.url, xpath='//p') %>%
    html_text()
  
  #Collapses all the separate <p> text content into one string of text
  text.add <- paste(text.add, collapse=" ")%>%str_trim()
  
  
  #Collects the title from pages
  text.title <- new.url %>% html_element("title")%>%
    html_text()%>%str_trim()
  
  #Collects the date from pages
  date.add <- html_nodes(new.url, 'time') %>%
    html_text()%>%str_replace_all("\n","")%>%str_trim()
  
  date.add <- ifelse(is.null(date.add),'No Date', date.add) # If any nulls replaced with NA
  
  page.text <- c(page.text, text.add)
  page.title <- c(page.title, text.title)
  page.date <- c(page.date, date.add)
}


# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data <- tibble('URL'=scrape.list, 'Title'=page.title, 'Date'=page.date, 'Text'=page.text)


# Save dataframe as a CSV file
write.csv(scrape.data, 'NYCTimes.csv')
