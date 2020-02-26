

file_name <- paste0('Output/reddit_ireland/',format(Sys.time(), "%Y%m%d_%H%M%S_"), "data_set.Rdata")

reddit_ireland_scraper <- function(url){
  reddit_ireland <- xml2::read_html(url)
  
  times <- reddit_ireland %>%
    rvest::html_nodes('a._3jOxDPIQ0KaOWpzvSQo-1s') %>%
    rvest::html_text()
  
  times
  
  urls <- reddit_ireland %>%
    rvest::html_nodes('a._3jOxDPIQ0KaOWpzvSQo-1s') %>%
    rvest::html_attr('href')
  
  urls
  
  reddit_ireland_df <- tibble::tibble(Pages = urls, post_time = times)
  ire_recent <- reddit_ireland_df[grep("minute|now", reddit_ireland_df$post_time),]
  
  titles <- c()
  comments <- c()
  for(i in reddit_ireland_df$Pages){
    
    reddit_temp_data <- xml2::read_html(i)
    body <- reddit_temp_data %>% 
      rvest::html_nodes('p._1qeIAgB0cPwnLhDF9XSiJM') %>%
      rvest::html_text()
    
    comments = append(comments, body)
    
    title <- reddit_temp_data %>%
      rvest::html_node("title") %>%
      rvest::html_text()
    titles = append(titles, rep(title,each=length(body)))
    
  }
  
  reddit_ire_hour <- tibble(Subreddit=titles, Comments=comments)
  reddit_ire_hour
}

reddit_ireland_scraper('https://www.reddit.com/r/ireland/new/') %>%
  save(., file = file_name)

ireland_today <- reddit_ireland_scraper('https://www.reddit.com/r/ireland/new/')
ireland_today %>% head()
library(sentimentr)
scores <- sentimentr::sentiment(ireland_today$Comments) %>% head()

mean(scores$sentiment)
