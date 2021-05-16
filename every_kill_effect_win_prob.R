### Using Win probabilty model to see most impactful gunfights
## look at win% at time of death vs 30 seconds (??) later

## use win% to see expected add to wp by winning - loss by dying

library(tidyverse)
library(odbc)
library(DBI)
library(tidyverse)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "callofduty",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)
ref_q <- tbl(con, "REFERENCE")
series_q <- tbl(con, 'SERIES')
game_q <- tbl(con, 'GAME')
score_q <- tbl(con, 'SCORE')
map_q <- tbl(con, "MAP")
team_q <- tbl(con, "TEAM")
mode_q <- tbl(con, "MODE")
killfeed_q <- tbl(con, "KILLFEED")

## Get all win probs for score at every second
score_q %>%
  left_join(game_q %>%
              select(MAP_ID, MODE_ID, GAME_ID, A_SCORE = TEAM_A_SCORE,
                     B_SCORE = TEAM_B_SCORE), by = "GAME_ID") %>%
  collect() %>%
  pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE), names_to = "team", values_to = "score") %>%
  group_by(GAME_ID, team) %>%
  filter(score >= 0,
         TIME_S >= 6,
         score < 250,
         # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
         !cumany(score == 249)
  ) %>%
  # left_join(hill_num, by = "")
  mutate(win = ifelse(team == "TEAM_A_SCORE", A_SCORE > B_SCORE, B_SCORE > A_SCORE),
         hill = pmax((TIME_S-5) %/% 60, 0)+1,
         seconds_into_hill = ((TIME_S-5) %% 60)+1,
         total_hills = ifelse(MAP_ID %in% c(43,44, 46,49), 5, 4),
         hill_no = hill-(((hill-1)%/%total_hills)*total_hills)) %>%
  select(GAME_ID,MAP_ID, TIME_S, team, score, win, total_hills) %>%
  group_by(GAME_ID, TIME_S) %>%
  mutate(opp_score = sum(score)-score) ->
  data

## now get timestamps for every kill and death by game_id, time_s
killfeed_q %>%
  # filter(GAME_ID == 767) %>%
  # group_by(GAME_ID) %>%
  left_join(game_q %>% select(-TEAM_A_SCORE, -TEAM_B_SCORE)) %>%
  filter(MODE_ID == 1) %>%
  collect() %>%
  pivot_longer(c(PLAYER_A, PLAYER_B),
               names_to = "type",
               values_to = "player") ->
  pbp_long

pbp_long %>%
  mutate(PLAYER_TEAM = ifelse(type == "PLAYER_A", TEAM_A_ID, TEAM_B_ID)) %>%
  select(GAME_ID, TIME_S,PLAYER_TEAM, player, type) %>%
  mutate(type = recode(type, PLAYER_A = "KILL", PLAYER_B = "DEATH")) %>%
  arrange(GAME_ID, player, TIME_S) ->
  kills_and_deaths

## get team_id of A and B
data %>%
  left_join(game_q %>%
              left_join(series_q) %>%
              select(GAME_ID, A_ID, B_ID) %>%
              collect()) %>%
  mutate(TEAM_ID = ifelse(team == "TEAM_A_SCORE", A_ID, B_ID)) %>%
  left_join(kills_and_deaths,
            by = c("GAME_ID", "TIME_S", "TEAM_ID" = "PLAYER_TEAM")) %>%
  group_by(GAME_ID) %>%
  filter(any(type == "KILL")) %>%
  mutate(diff = score - opp_score,
         ttw = 250 - pmax(score, opp_score)) %>%
  ungroup() %>%
  mutate(win_prob = predict(score_fit, ., type = 'prob')$.pred_TRUE) %>%
  group_by(GAME_ID, TEAM_ID) %>%
  mutate(next30 = lead(win_prob, 30)) %>%
  filter(TIME_S < total_hills*60) ->
  pp_data

## now compare win_prob and win_prob30 on deaths and kills
pp_data %>%
  filter(type %in% c("DEATH")) %>%
  mutate(win_change = next30 - win_prob) %>%
  group_by(MAP_ID) %>%
  mutate(bin = cut(TIME_S, seq(0, max(TIME_S), 10)),
         bin_num = (TIME_S %/% 10)*10) %>%
  group_by(MAP_ID, bin_num, type) %>%
  summarise(win_change = mean(win_change)*2,
            n = n()) %>%
  left_join(map_q %>% collect()) %>%
  ggplot(aes(bin_num, win_change))+
  geom_col(size = 1, fill = '#3d1866')+
  facet_wrap(~MAP_NAME, scale = 'free_x')+
  geom_vline(xintercept =seq(60,300, 60), alpha = 0.3, lwd = 1)+
  geom_hline(yintercept = 0)+
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = element_blank(), y = 'Change in Win% w/ DEATH',
       title = "How much does a death cost?",
       subtitle = "Change in Win% 30s after a death")+
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pp_data %>% filter(TEAM_ID == 6, GAME_ID > 950) %>%
  ggplot(aes(TIME_S, win_prob, color = as.factor(TEAM_ID)))+
  geom_line(size=1)+
  facet_wrap(~GAME_ID) +
  theme_bw() +
  labs(x = element_blank(), y = element_blank())+
  theme(legend.position = 'none',
        axis.text = element_blank(),
        axis.ticks = element_blank())

## get win% change start to end of each hill
get_HP_data(30) -> data
data %>%
  group_by(GAME_ID, TIME_S) %>%
  mutate(opp_score = sum(score)-score) %>%
  group_by(GAME_ID) %>%
  mutate(diff = score - opp_score,
         ttw = 250 - pmax(score, opp_score)) %>%
  filter(TEAM_ID == 6) %>%
  ungroup() %>%
  mutate(win_prob = predict(score_fit, ., type = 'prob')$.pred_TRUE) %>%
  group_by(GAME_ID, TEAM_ID,MAP_ID, hill, hill_no) %>%
  summarise(start_win = win_prob[1],
            end_win = win_prob[n()],
            score_added = max(score)-min(score)) %>%
  group_by(GAME_ID) %>%
  filter(hill != max(hill), hill <= 10, MAP_ID != 42) %>%
  mutate(win_added = end_win - start_win) %>%
  arrange(GAME_ID, hill) %>%
  left_join(map_q %>% collect()) %>%
  group_by(MAP_NAME,  hill) %>%
  summarise(avg_win_added = mean(win_added),
            win_sd = sd(win_added),
            avg_score_added = mean(score_added),
            score_sd = sd(score_added)) %>%
  ggplot() +
  geom_col(aes(as.factor(hill), avg_win_added, fill = avg_win_added), color = 'black')+
  # geom_errorbar(aes(as.factor(hill), ymin = avg_win_added-win_sd, ymax = avg_win_added+win_sd)) +
  facet_wrap(~MAP_NAME, scales = 'free_x')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_distiller(palette = "RdYlBu", direction = 1)+
  theme_bw()+
  labs(x = "Hill",
       y = "Win% Added per Hill")+
  theme(legend.position = 'none')
  
