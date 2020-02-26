library(rvest)
library(xml2)
library(dplyr)

## Reddit example

## take a look at this webpage, a specific subreddit
url <- 'https://www.reddit.com/r/ireland/comments/f78u3q/drug_dealer_loses_codes_for_536m_bitcoin_accounts/'

reddit_wbpg <- xml2::read_html(url)
reddit_wbpg %>% 
  rvest::html_node('title') %>%
  rvest::html_text()

# let's get the comments
reddit_wbpg %>% 
  rvest::html_nodes('p._1qeIAgB0cPwnLhDF9XSiJM') %>%
  rvest::html_text()

## what about all recent subreddits
reddit_ireland <- xml2::read_html('https://www.reddit.com/r/ireland/new/')
## their times
times <- reddit_ireland %>%
  rvest::html_nodes('a._3jOxDPIQ0KaOWpzvSQo-1s') %>%
  rvest::html_text()

times
## their urls
urls <- reddit_ireland %>%
    rvest::html_nodes('a._3jOxDPIQ0KaOWpzvSQo-1s') %>%
    rvest::html_attr('href')
  
urls

reddit_ireland_df <- tibble::tibble(Pages = urls, post_time = times)
## only want ones that have occurred in the past hour
ire_recent <- reddit_ireland_df[grep("minute|now", reddit_ireland_df$post_time),]

## now let's look at the comments in each subreddit
titles <- c()
comments <- c()
for(page in ire_recent$Pages){
  
  reddit_temp_data <- xml2::read_html(page)
  body <- reddit_temp_data %>% 
    rvest::html_nodes('p._1qeIAgB0cPwnLhDF9XSiJM') %>%
    rvest::html_text()
    
  comments = append(comments, body)
  
  title <- reddit_temp_data %>%
    rvest::html_node("title") %>%
    rvest::html_text()
  titles = append(titles, rep(title,each=length(body)))
  
}

## putting it all together
reddit_ire_hour <- tibble(Subreddit=titles, Comments=comments)
reddit_ire_hour %>% 
  select(Comments) %>%
  head()
  
