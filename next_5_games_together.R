

player_data <- read_csv("C:/Users/liebe/Downloads/query_result_2021-05-23T14_12_07.518339Z.csv") 
team_data <- read_csv("C:/Users/liebe/Downloads/query_result_2021-05-23T14_14_02.64791Z.csv") 

# create lineups for each team and game per season
player_data %>% 
  mutate(player_name = tolower(player_name)) %>% 
  group_by(season, team, match_id, game_id) %>% 
  summarise(lineup = paste0(player_name[order(player_name)], collapse = "-")) %>% 
  left_join(team_data) %>% 
  group_by(season, team, match_id, lineup) %>% 
  filter(game_number == max(game_number)) %>% 
  summarise(series_win = win) %>% 
  group_by(season, team, lineup) %>% 
  mutate(games_left = n() - (1:n()),
         games_together = n(),
         games_so_far = 1:n(),
         five_rolling_wins = (zoo::rollsumr(series_win, k = 5, fill = NA))) %>% 
  group_by(season, team) %>% 
  mutate(last_game = match_id == max(match_id)) %>% 
  group_by(season, team, lineup) %>% 
  mutate(last_lineup = any(last_game)) %>% 
  filter(!is.na(five_rolling_wins),!last_lineup ) %>% 
  group_by(five_rolling_wins) %>%
  summarise(games_left = median(games_left))
  ggplot(aes(games_so_far, five_rolling_wins))+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_jitter()+
  labs(y = "wins in next 5 games")
