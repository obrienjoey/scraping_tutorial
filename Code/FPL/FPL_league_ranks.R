library(tidyverse)
### make a fantasy table from details

temp <- jsonlite::fromJSON('https://fantasy.premierleague.com/api/leagues-classic//904977/standings/') %>%
  purrr::pluck(3) %>%
  purrr::pluck(3)

team_names <- temp$entry_name
user_id <- temp$entry

df <- tibble(user_points = integer(), team_names = character(), GW = integer())

for(i in 1:length(user_id)){
  user <- user_id[i]
  team <- team_names[i]
  data <- jsonlite::fromJSON(paste('https://fantasy.premierleague.com/api/entry/',user,'/history/', sep = '')) %>%
    purrr::pluck(1) %>%
    select(total_points) %>%
    mutate(user_id = rep(user, nrow(.)), team_names = rep(team, nrow(.)),
           GW = seq(1,nrow(.),1)) %>%
    select(-user_id)
  df <- df %>% rbind(data)
}

rm(team_names)

df #%>% pivot_wider(names_from = GW, values_from = total_points) %>%
#rename_if(is.numeric, list( ~paste0('GW', .) ))

df <- df %>% 
  group_by(GW)%>%
  mutate(rank = dense_rank(desc(total_points)))# %>%

ggplot(df, aes(x = GW, y = rank)) +
  geom_point(color = 'black', size = 2.2) +
  geom_line(aes(color = team_names, alpha = 1), size = 1.5) +
  geom_point(aes(color = team_names), size = 2) +
  scale_y_reverse(breaks = 1:length(user_id)) +
  geom_text(data = df %>% filter(GW == max(df$GW)),
            aes(label = as.factor(team_names), x = GW + 1) , hjust = 0, size = 3) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(1,max(df$GW),2), minor_breaks = 1:max(df$GW),
                     labels = seq(1,max(df$GW),2), limits = c(1,max(df$GW) + 9),
                     expand = c(0.01,0.01)) +
  labs(y = '', x = 'Gameweek', title = 'MACSI Classic',
       subtitle = '2019/20 Season') +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        axis.text.y=element_blank(),
        plot.title.position = 'plot',
        axis.title.x = element_text(hjust=0.375),
        plot.title = element_text(face = 'bold'))

ggsave('Output/FPL/MACSI_ranks.jpeg', width = 6, height = 4, units = 'in', dpi = 400)
