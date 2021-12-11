################
### Original ###
################
# install.packages("RSelenium")
# install.packages("rvest")
# install.packages("tidyverse")
library(RSelenium)
library(tidyverse)
library(rvest)
binman::list_versions("chromedriver")

# Step 1: Navigate to the URL
#https://www.latlong.net/.

# Step 2: Let RSelenium Type in the Necessary Fields
driver <- rsDriver(port = 4569L, browser=c("chrome"), chromever = "95.0.4638.54")
remote_driver <- driver[["client"]]
remote_driver$open()
remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")


# Sys.sleep(5) # give the page time to fully load
# html <- remote_driver$getPageSource()[[1]]

address_element <- remote_driver$findElement(using = 'class', value = 'width70')

address_element$sendKeysToElement(list("Lombard Street, San Francisco"))

button_element <- remote_driver$findElement(using = 'class', value = "button")

button_element$clickElement()

#Step 3: Scrape the Coordinates From the Website
out <- remote_driver$findElement(using = "class", value="coordinatetxt")
lat_long <- out$getElementText()
street_names <- c("Lombard Street, San Francisco", 
                  "Santa Monica Boulevard", 
                  "Bourbon Street, New Orleans", 
                  "Fifth Avenue, New York", 
                  "Richards Street, Vancouver")
get_lat_lon <- function(street_names) {
  remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")
  final <- c()
  for(i in 1:length(street_names)) {
    
    remote_driver$refresh()
    Sys.sleep(1)
    
    address_element <- remote_driver$findElement(using = 'class', value = 'width70')
    
    address_element$sendKeysToElement(list(street_names[i]))
    button_element <- remote_driver$findElement(using = 'class', value = "button")
    
    button_element$clickElement()
    Sys.sleep(3)
    
    out <- remote_driver$findElement(using = "class", value = "coordinatetxt")
    output <- out$getElementText()
    final <- c(final, output)
    
  }
  
  return(final)
}
vector_out <- get_lat_lon(street_names)

data.frame(street_names, purrr::flatten_chr(vector_out)) %>%
  dplyr::mutate(., vector_out = stringr::str_remove_all(vector_out, "\\(|\\)")) %>%
  tidyr::separate(., vector_out, into = c("latitude", "longitude"), sep = ",")
# street_names purrr..flatten_chr.vector_out.   latitude    longitude
# 1 Lombard Street, San Francisco       (37.799999, -122.434402)  37.799999  -122.434402
# 2        Santa Monica Boulevard       (-27.867491, 153.353973) -27.867491   153.353973
# 3   Bourbon Street, New Orleans        (29.964100, -90.060791)  29.964100   -90.060791
# 4        Fifth Avenue, New York           (0.000000, 0.000000)   0.000000     0.000000
# 5    Richards Street, Vancouver           (0.000000, 0.000000)   0.000000     0.000000



#####################################################################
rD <- rsDriver(port = 4570L, browser=c("chrome"), chromever = "95.0.4638.54")
remDr <- rD[["client"]]
remDr$navigate("https://www.fcc.gov/media/engineering/dtvmaps")

zip <- "30308"
remDr$findElement(using = "id", value = "startpoint")$sendKeysToElement(list(zip))
# other possible ("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text")

remDr$findElements("id", "btnSub")[[1]]$clickElement()

#Extracting data from HTML
Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

signals <- read_html(html) %>% # parse HTML
  html_nodes("table.tbl_mapReception") %>% # extract table nodes with class = "tbl_mapReception"
  .[3] %>% # keep the third of these tables
  .[[1]] %>% # keep the first element of this list
  html_table(fill=T) # have rvest turn it into a dataframe
View(signals)

names(signals) <- c("rm", "callsign", "network", "ch_num", "band", "rm2") # rename columns

signals <- signals %>%
  slice(2:n()) %>% # drop unnecessary first row
  filter(callsign != "") %>% # drop blank rows
  select(callsign:band) # drop unnecessary columns

head(signals)

read_html(html) %>% 
  html_nodes(".callsign") %>% 
  html_attr("onclick")

read_html(html) %>% 
  html_nodes(".callsign") %>% 
  html_attr("onclick") %>% 
  str_extract("(?<=RX Strength: )\\s*\\-*[0-9.]+")


strength <- read_html(html) %>% 
  html_nodes(".callsign") %>% 
  html_attr("onclick") %>% 
  str_extract("(?<=RX Strength: )\\s*\\-*[0-9.]+")

signals <- cbind(signals, strength)
signals

#Extensions: Iteration
# Step 1: submitting a new zip code and preparing for iteration
zip <- "27511"
remDr$findElement(using = "id", value = "startpoint")$sendKeysToElement(list(zip))

remDr$findElement("id", "startpoint")$clearElement()
zip <- "27511111"
remDr$findElement(using = "id", value = "startpoint")$sendKeysToElement(list(zip))
remDr$findElements("id", "btnSub")[[1]]$clickElement()

alert <- try(remDr$getAlertText(), silent=T) # check if there is an alert window

if(class(alert) != "try-error") { # if an alert window is present, do the following
  
  signals <- data.frame(callsign = NA, network = NA, ch_num = NA, band = NA, strength = NA, cont.strength = NA)
  remDr$acceptAlert()
  remDr$findElement("id", "startpoint")$clearElement()
  
} else { # if no alert, continue on as normal
  
  # normal scraping procedure code here
  
}


#Putting it into a function
# install.packages('zipcodeR')
# install.packages('devtools')
# devtools::install_github("gavinrozzi/zipcodeR")          
library(zipcodeR)
download_zip_data(force=FALSE)
zips.df <- search_city("San Francisco", "CA")#read.csv("zip_code_data.csv") # csv of zip codes

rD <- rsDriver(browser="chrome", port=4571L, chromever = "95.0.4638.54")
remDr <- rD[["client"]]

remDr$navigate("https://www.fcc.gov/media/engineering/dtvmaps")

scrape.zips <- function(zip){ # our scraping function
  
  remDr$findElement("id", "startpoint")$sendKeysToElement(list(zip))
  remDr$findElements("id", "btnSub")[[1]]$clickElement()
  
  alert <- try(remDr$getAlertText(), silent=T)
  
  if(class(alert) != "try-error") {
    
    signals <- data.frame(callsign = NA, network = NA, ch_num = NA, band = NA, strength = NA, cont.strength = NA)
    remDr$acceptAlert()
    remDr$findElement("id", "startpoint")$clearElement()
    
  } else {
    Sys.sleep(2)
    
    html <- remDr$getPageSource()[[1]]
    
    cont.strength <- read_html(html) %>% 
      html_nodes(".callsign") %>% 
      html_attr("onclick") %>% 
      str_extract("(?<=RX Strength: )\\s*\\-*[0-9.]+")
    
    signals <- read_html(html) %>%
      html_nodes("table.tbl_mapReception") %>%
      .[3] %>%
      .[[1]] %>%
      html_table(fill=T)
    
    names(signals) <- c("rm", "callsign", "network", "ch_num", "band", "rm2")
    
    signals <- signals %>%
      slice(2:n()) %>%
      filter(callsign != "") %>%
      select(callsign:band)
    
    strength <- read_html(html) %>%
      html_nodes("table.tbl_mapReception:nth-child(3) .ae-img") %>%
      html_attr("src")
    
    if(length(strength)==0) { strength <- "none" }
    if(length(cont.strength)==0) { cont.strength <- "none" }
    
    signals <- cbind(signals, strength) %>% cbind(cont.strength)
    
    signals <- mutate(signals, strength = strength %>% str_extract("strength."))
  }
  
  remDr$findElement("id", "startpoint")$clearElement()
  
  return(signals)
  
  Sys.sleep(runif(1, 1, 3))
  
}

scrape_safe <- function(zip){
  
  result <- try(scrape.zips(zip))
  
  if (class(result) == "try-error") { # if there is any error caught, return a blank dataframe and keep going
    cat("Error encountered for zip:", zip, "\n")
    return(data.frame()) 
    Sys.sleep(runif(1, 1, 3))
  } else { # if no error, keep going as normal to next zip
    return(result)
  }
}
  
  
zips.df <- zips.df %>%
  group_by(zip) %>%
  do(scrape_safe(.$zip))