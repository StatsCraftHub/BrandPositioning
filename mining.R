#####Scrapping data from Amazon########
# Install / Load relevant packages
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(rvest, dplyr, tidyr, stringr)
# product code
prod_code <- "B07DJD1RTM"
url <- paste0("https://www.amazon.in/Test-Exclusive-740/dp/", prod_code)
doc <- read_html(url)
#obtain the text in the node, remove "\n" from the text, and remove white space
prod <- html_nodes(doc, "#productTitle") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  trimws()
prod
# Function to scrape elements from Amazon reviews
scrape_amazon <- function(url, throttle = 0){
  # Install / Load relevant packages
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, purrr)
  # Set throttle between URL calls
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  # obtain HTML of URL
  doc <- read_html(url)
  # Parse relevant elements from HTML
  comments <- doc %>%
    html_nodes(".a-expander-partial-collapse-content span") %>%
    html_text() 
  
  head <- doc %>%
    html_nodes(".a-text-bold span") %>%
    html_text() 
  gsub("\n\n \\s*|found this helpful.*", "", .) %>%
    gsub("One", "1", .) %>%
    map_chr(~ str_split(string = .x, pattern = " ")[[1]][1]) %>%
    as.numeric()
  # Combine attributes into a single data frame
  df <- data.frame(comments, stringsAsFactors = F)
  return(df)
}
scrape_amazon(url)
# load DT packege
pacman::p_load(DT)

# run scraper function
url <- "https://www.amazon.in/Test-Exclusive-740/dp/B07DJD1RTM/?pageNumber=1"
reviews <- scrape_amazon(url)
# display data
str(reviews)
# Set # of pages to scrape. Note: each page contains 8 reviews.
pages <- 100

# create empty object to write data into
reviews_all <- NULL

# loop over pages
for(page_num in 1:pages){
  url <- paste0("https://www.amazon.in/Test-Exclusive-740/dp/,prod_code,"/?pageNumber=", page_num)
  reviews <- scrape_amazon(url, throttle = 3)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}
str(reviews_all)