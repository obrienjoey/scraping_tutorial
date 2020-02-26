library(tidyverse)

https://cwur.org/2019-2020.php %>%
  
temp <- xml2::read_html('https://www.donedeal.ie/cars/Volkswagen/Passat?source=private&year_from=2010')
rvest::html_nodes(temp, 'div#searchResultsPanel')


sandp_stocks <- xml2::read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>%
  rvest::html_nodes('table#constituents') %>%
  rvest::html_table() %>%
  dplyr::first() %>%
  tibble::as_tibble(.name_repair = 'universal') 

tickers <- sandp_stocks %>%
              select(Symbol)

stock_resol <- '1d' # stock resolution
no_days <- '1wk' # history to get

base_url <- 'https://query1.finance.yahoo.com/v8/finance/chart/'

for(ticker in tickers$Symbol[1]){
  
  full_url <- paste(base_url, ticker, '?range=', no_days, '&interval=', stock_resol, sep ='')
  data <- full_url %>%
    jsonlite::fromJSON() #%>%
  
  tidy_data <- as.data.frame(data$chart$result$timestamp[[1]],
                         data$chart$result$indicators$quote[[1]]$close[[1]])
    
  #select(result)
}

  
data %>% purrr::pluck(1)


xml2::read_html('https://www.autotrader.ie/search/result/cars/make/volkswagen/model/passat/min-year/2011/max-year/2011/simple-year/2011/page/1/limit/30') %>%
  rvest::html_nodes('li')
  #rvest::html_nodes('.advert ch-car module') #%>% rvest::html_text(trim=TRUE)
a[50]  


