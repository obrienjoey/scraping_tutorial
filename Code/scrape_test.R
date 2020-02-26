library(tidyverse)
library(ggplot2)
### 6 nations example

temp <- xml2::read_html('https://en.wikipedia.org/wiki/Ireland_national_rugby_union_team') %>%
  rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[9]/tbody/tr/td/table') %>%
  rvest::html_table(fill = TRUE) %>%
  purrr::pluck(1) %>%
  dplyr::as_tibble() %>%
  dplyr::rename('DOB' = `Date of birth (age)`, 'Club' = `Club/province`) %>%
  dplyr::mutate(Age = as.integer(substr(DOB, nchar(DOB)-2, nchar(DOB)-1)),
                Nation = rep('IRE', nrow(.))) %>%
  dplyr::select(-c(DOB))

details <- dplyr::as_tibble(list(names = c('Irish', 'England', 'French', 'Welsh', 'Scottish', 'Italy'),
                     table_no = c('9','9','8','5','9','16'),
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
}  

### example analysis

df %>% group_by(Nation) %>%
  summarise(TC = sum(Caps)) %>%
  ggplot(aes(x = Nation, y = TC, fill = as.factor(Nation))) +
  geom_bar(stat = 'Identity', color = 'black', show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = 'Total Caps for Each Team') +
  ylab('') +
  theme_minimal() +
  theme(plot.title.position = 'plot',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
df %>% group_by(Nation, Position) %>%
  summarise(MeanCP = mean(Caps)) %>%
  ggplot(aes(fill = Position, y = MeanCP, x = Nation)) +
  geom_bar(position="dodge", color = 'black', stat="identity") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1") +
  ylab('') +
  ggtitle('Average Caps by Position') +
  theme(plot.title.position = 'plot',
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
  theme(plot.title.position = 'plot',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

df %>% group_by(Nation) %>%
  summarise( 
    n=n(),
    AA=mean(Age),
    sd=sd(Age)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  ggplot(aes(x = Nation, y = AA, fill = as.factor(Nation))) +
  geom_bar(stat = 'Identity', color = 'black', show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  ylab('') +
  theme_minimal() +
  geom_errorbar(aes(x = Nation , ymin=AA-ic, ymax=AA+ic), width=0.4, 
                colour="black", alpha=0.9, size=0.5) +
  labs(title = "Average Age for Each Team",
       subtitle = "with confidence intervals") +
  theme(plot.title.position = 'plot',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
### nobel prize

empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

df_np <- xml2::read_html('https://stats.areppim.com/listes/list_nobelxprize.htm') %>%
  rvest::html_nodes(xpath = '//*[@id="physics"]') %>%
  rvest::html_table(fill = TRUE) %>%
  purrr::pluck(1) %>%
  tibble::as_tibble(., .name_repair = janitor::make_clean_names) %>%
  dplyr::mutate(full_name = paste(name, name_2, sep =' ')) %>%
  dplyr::rename('affil' = 'affiliation_at_the_time_of_the_award') %>%
  dplyr::select(-c(name, name_2)) %>%
  mutate_each(list(empty_as_na)) 

df_np %>% group_by(affil) %>%
  count(sort = TRUE)
 
### fpl example
temp <- jsonlite::fromJSON('https://fantasy.premierleague.com/api/leagues-classic/314/standings/?page_new_entries=1&page_standings=10&phase=1')%>%
  purrr::pluck(3) %>%
  purrr::pluck(3) %>%
  select(-c(player_name,entry_name))

tb <- as_tibble(matrix(nrow = 0, ncol = length(names(temp)), dimnames = list(NULL, names(temp))))
for(i in 1:10){
  data <- jsonlite::fromJSON(paste('https://fantasy.premierleague.com/api/leagues-classic/314/standings/?page_new_entries=1&page_standings=',i,'&phase=1', sep = '')) %>%
    purrr::pluck(3) %>%
    purrr::pluck(3) %>%
    select(-c(player_name,entry_name))
  
  tb <- rbind(tb, data)
}
tb
