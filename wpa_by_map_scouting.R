
source("helper_get_hillside_data.R")
# score_fit <- readRDS("hp_win_model.RDS")
hp_data <- get_HP_data(40)

hp_data %>%
  group_by(GAME_ID, TEAM_ID, MAP_ID, hill, hill_no) %>%
  summarise(score = max(score)-min(score),
            rotated = any(rotated)) %>%
  group_by(GAME_ID, TEAM_ID, MAP_ID) %>%
  mutate(p4_p3 = score + lead(score)) %>%
  filter(MAP_ID == 42, hill_no == 3, hill < 9, rotated) %>%
  group_by(TEAM_ID) %>%
  summarise(score = mean(p4_p3, na.rm = T),
            n = n())

hp_data %>%
  filter(GAME_ID == 1401) %>%
  group_by(GAME_ID, hill, TIME_S) %>%
  mutate(opp_score = sum(score)-score,
         diff = score - opp_score,
         ttw = 250 - pmax(score, opp_score)
         ) %>%
  ungroup() %>%
  mutate(win_prob = predict(score_fit, ., "prob")$.pred_TRUE) %>%
  ggplot(aes((TIME_S-5)/60, win_prob, color = factor(TEAM_ID)))+geom_line()

hp_data %>%
  filter(GAME_ID == 1401) %>%
  group_by(GAME_ID, TEAM_ID, MAP_ID, hill, hill_no) %>%
  summarise(score = max(score)) %>%
  group_by(GAME_ID, hill) %>%
  mutate(opp_score = sum(score)-score,
         diff = score - opp_score,
         ttw = 250 - pmax(score, opp_score)
  ) %>%
  ungroup() %>%
  mutate(win_prob = predict(score_fit, ., "prob")$.pred_TRUE) %>%
  group_by(GAME_ID, TEAM_ID) %>%
  mutate(win_change = win_prob - lag(win_prob, default = 0.5)) %>%
  filter(win_change > 0) %>%
  ggplot(aes(hill, win_change, fill = factor(TEAM_ID)))+geom_col(position = 'dodge')
  filter(MAP_ID < 50) %>%
  group_by(TEAM_ID, MAP_ID, hill_no) %>%
  summarise(change = mean(win_change, na.rm = T)) %>%
  ggplot(aes(hill_no, change))+
  geom_col()+
  facet_wrap(~MAP_ID)

### Look at dist of outcomes for a team on each map mode
library(ggridges)

hp_data %>%
  group_by(GAME_ID, TEAM_ID, MAP_ID, hill, hill_no) %>%
  summarise(score = max(score)-min(score)) %>%
  group_by(GAME_ID, hill) %>%
  mutate(opp_score = sum(score)-score,
         diff = score - opp_score,
         ttw = 250 - pmax(score, opp_score)
  ) %>%
  ungroup() %>%
  mutate(win_prob = predict(score_fit, ., "prob")$.pred_TRUE) %>%
  group_by(GAME_ID, TEAM_ID) %>%
  mutate(win_change = win_prob - lag(win_prob, default = 0.5)) %>%
  filter( TEAM_ID== 6, abs(win_change) < 0.15) %>%
  left_join(map_q %>% collect()) %>%
  group_by(MAP_NAME, TEAM_ID) %>%
  mutate(count = length(unique(GAME_ID))) %>%
  mutate(hill_no = as.factor(hill_no),
         map_n = paste0(MAP_NAME, " (n=", count,")")) %>%
  ggplot()+
  geom_density_ridges(aes(y = hill_no, x = win_change, fill = hill_no),
                      alpha = 0.6,
                      vline_size = 1, vline_color = "red",
                      quantile_lines = TRUE, quantiles = 0.5) +
  theme_bw()+
  facet_wrap(~map_n, scales = 'free_y')+
  geom_vline(xintercept = 0, lwd = 1, alpha = 0.6)+
  theme(legend.position = 'none') +
  scale_x_continuous(label = scales::percent)+
  labs(x = "Win Prob Added",
       y = "Hill",
        title = "Win Probability Added by Hill",
        subtitle = "Not penalizing teams for giving up time that doesn't hurt win%")
