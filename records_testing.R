
game_q %>%
  # filter(MODE_ID == 1) %>%
  select(-START_SEC, -DURATION, -SERIES_ID) %>%
  mutate(TEAM_A_WIN = TEAM_A_SCORE > TEAM_B_SCORE) %>%
  collect() %>%
  pivot_longer(c(-GAME_ID, -MODE_ID, -MAP_ID,-TEAM_A_WIN),
               names_to = "team",
               values_to = 'score') %>%
  mutate(side = ifelse(team == "TEAM_A_SCORE", 'blue', 'red'),
         win = case_when(
           (team == "TEAM_A_SCORE" & TEAM_A_WIN) |
             (team == "TEAM_B_SCORE" & !TEAM_A_WIN) ~ TRUE,
           TRUE ~ FALSE
         )) %>%
  group_by(MODE_ID,MAP_ID, side) %>%
  summarise(win = sum(win),
            loss = n() - win,
            score = mean(score)) %>%
  left_join(map_q %>% collect(), by = "MAP_ID")
