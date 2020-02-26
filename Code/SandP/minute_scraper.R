library(tidyverse)

### what stocks are in the S&P 500
sandp_stocks <- xml2::read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>%
  rvest::html_nodes('table#constituents') %>%
  rvest::html_table() %>%
  dplyr::first() %>%
  tibble::as_tibble(.name_repair = 'universal') 

### what are their symbols??
tickers <- sandp_stocks %>%
  select(Symbol)

### this is the Yahoo chart https://finance.yahoo.com/quote/AAPL?p=AAPL

### what about the 1m data? https://query1.finance.yahoo.com/v8/finance/chart/MMM?range=1d&interval=1m

stock_resol <- '1m' # stock resolution
no_days <- '1d' # history to get

base_url <- 'https://query1.finance.yahoo.com/v8/finance/chart/'

dir_name <- paste("Output/SandP/stocks", Sys.Date(), sep = "_")
dir.create(dir_name)
i <- 1
for(ticker in tickers$Symbol){
  full_url <- paste(base_url, ticker, '?range=', no_days, '&interval=', stock_resol, sep ='')
  res <- try(data <- full_url %>%
               jsonlite::fromJSON())
  if(inherits(res, "try-error"))
  {
    #error handling code, maybe just skip this iteration using
    i <- i + 1
    next
  }
  tidy_data <- tibble(timestamp = data$chart$result$timestamp[[1]],
                      close = data$chart$result$indicators$quote[[1]]$close[[1]])
  file_name <- paste0(dir_name,'/',ticker,".csv")
  write.csv(tidy_data,file_name)
  
  print(paste('Finished: ',i/nrow(tickers)))
  i <- i + 1
}


