library(tidyverse)
library(ggplot2)
### 6 nations example

xml2::read_html('https://en.wikipedia.org/wiki/Ireland_national_rugby_union_team') %>%
  rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[8]/tbody/tr/td/table') %>%
  rvest::html_table(fill = TRUE) %>%
  purrr::pluck(1) %>%
  dplyr::as_tibble() %>%
  dplyr::rename('DOB' = `Date of birth (age)`, 'Club' = `Club/province`) %>%
  dplyr::mutate(Age = as.integer(substr(DOB, nchar(DOB)-2, nchar(DOB)-1)),
                Nation = rep('IRE', nrow(.))) %>%
  dplyr::select(-c(DOB))

### okay let's do the English team now
xml2::read_html('https://en.wikipedia.org/wiki/England_national_rugby_union_team') %>%
  rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[8]/tbody/tr/td/table') %>%
  rvest::html_table(fill = TRUE) %>%
  purrr::pluck(1) %>%
  dplyr::as_tibble() %>%
  dplyr::rename('DOB' = `Date of birth (age)`, 'Club' = `Club/province`) %>%
  dplyr::mutate(Age = as.integer(substr(DOB, nchar(DOB)-2, nchar(DOB)-1)),
                Nation = rep('IRE', nrow(.))) %>%
  dplyr::select(-c(DOB))

### Error?

details <- tibble::as_tibble(list(names = c('Irish', 'England', 'French', 'Welsh', 'Scottish', 'Italy'),
                                 table_no = c('8','9','8','5','9','16'),
                                 short_name = c('IRE','ENG','FRA','WAL','SCO','ITA')))

df <- tibble::tibble(Player = character(), Position = character(),
                     Caps = integer(), Club = character(),
                     Age = integer(), Nation = character())

scraper_team <- function(df,team,table_no,short_name){
  data <- xml2::read_html(paste('https://en.wikipedia.org/wiki/',team,'_national_rugby_union_team', sep = '')) %>%
    rvest::html_nodes(xpath = paste('//*[@id="mw-content-text"]/div/table[',table_no,']/tbody/tr/td/table')) %>%
    rvest::html_table(fill = TRUE) %>%
    purrr::pluck(1) %>%
    dplyr::as_tibble() %>%
    dplyr::rename('DOB' = `Date of birth (age)`, 'Club' = `Club/province`) %>%
    dplyr::mutate(Age = as.integer(substr(DOB, nchar(DOB)-2, nchar(DOB)-1)),
                  Nation = rep(short_name, nrow(.))) %>%
    dplyr::select(-c(DOB))
  
  df <- df %>% rbind(data)
  return(df)
}

for(ii in 1:6){
  df <- scraper_team(df,details$names[ii],details$table_no[ii],details$short_name[ii])
  print(ii/6)
}  

df %>% head()
df %>% tail()

### example analysis

df %>% 
  group_by(Nation) %>%
  summarise(TC = sum(Caps)) %>%
  ggplot(aes(x = Nation, y = TC, fill = as.factor(Nation))) +
  geom_bar(stat = 'Identity', color = 'black', show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = 'Total Caps for Each Team') +
  ylab('') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

df %>% group_by(Nation, Position) %>%
  summarise(MeanCP = mean(Caps)) %>%
  ggplot(aes(fill = Position, y = MeanCP, x = Nation)) +
  geom_bar(position="dodge", color = 'black', stat="identity") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1") +
  ylab('') +
  ggtitle('Average Caps by Position') +
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

df %>% group_by(Nation) %>%
  summarise(AA = mean(Age)) %>%
  ggplot(aes(x = Nation, y = AA, fill = as.factor(Nation))) +
  geom_bar(stat = 'Identity', color = 'black', show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  ylab('') +
  theme_minimal() +
  ggtitle('Average Age for Each Team') +
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
