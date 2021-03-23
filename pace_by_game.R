
source(here::here("get_boxscores_helper.R"))
head(all_players_box)

all_players_box %>%
  filter(TEAM_ID == 6) %>%
  filter(player %in% c("vivid","assault", "silly", "apathy")) %>%
  mutate(pace_engs = engs - (0.3/60*(kills-deaths)),
         epm_adj = pace_engs/DURATION*600) %>%
  group_by(MODE_ID, MAP_ID, player) %>%
  mutate(rolling_pace = zoo::rollmeanr(epm_adj, k = 10, fill = NA),
         n = 1:n()) %>%
  mutate(scaled_pace = rolling_pace/mean(rolling_pace, na.rm = T)) %>%
  filter(MODE_ID  == 1, MAP_ID != 42) %>%
  left_join(map_q %>% collect()) %>%
  ggplot(aes(n, scaled_pace, color = player)) +
  geom_hline(yintercept = 1) +
  geom_line()+
  facet_wrap(~MAP_NAME)+
  theme_bw() +
  labs(x = "GAME NO.",
       y = "Pace, 1.0 = player average",
       title = "10-game average Pace",
       subtitle = "1.0 is player average, not team")