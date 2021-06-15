# functions for records dash
## Trying to recreate the excel from last year
library(odbc)
library(DBI)
library(tidyverse)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "callofduty",
                      username    = 'admin',
                      password    = "laguerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)
ref_q <- tbl(con, "REFERENCE")
series_q <- tbl(con, 'SERIES')
game_q <- tbl(con, 'GAME')
score_q <- tbl(con, 'SCORE')
map_q <- tbl(con, "MAP")
team_q <- tbl(con, "TEAM")
mode_q <- tbl(con, "MODE")

## prepare the data
hardpoint_games <- 
  game_q %>%
  filter(MODE_ID == 1) %>%
  left_join(series_q,
            by = "SERIES_ID") %>%
  filter(DATE >= "2021-02-15") %>%
  collect()

hardpoint_scores <- 
  score_q %>%
  filter(GAME_ID %in% !!hardpoint_games$GAME_ID) %>%
  collect() %>%
  inner_join(hardpoint_games %>% select(-TEAM_A_SCORE, -TEAM_B_SCORE),
             by = "GAME_ID")

hardpoint_scores %>%
  select(MAP_ID, GAME_ID, TEAM_A_SCORE, TEAM_B_SCORE, TIME_S, A_ID, B_ID) %>%
  pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE), names_to = "team", values_to = "score") %>%
  mutate(TEAM_ID = ifelse(team == "TEAM_A_SCORE", A_ID, B_ID)) %>%
  group_by(GAME_ID) %>%
  # mutate(TIME_S = TIME_S - 5) %>%
  filter(score >= 0,
         TIME_S >= 6,
         score < 250,
         # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
         !cumany(score == 249)
  ) %>%
  group_by(team, .add = T) %>%
  filter(score == cummax(score)) %>%
  # left_join(hill_num, by = "")
  mutate(hill = pmax((TIME_S-6) %/% 60, 0)+1,
         seconds_into_hill = ((TIME_S-6) %% 60)+1,
         new_hill = ifelse(hill > lag(hill), hill, NA),
         total_hills = ifelse(MAP_ID %in% c(43,44, 46, 49, 51), 5, 4),
         hill_no = hill-(((hill-1)%/%total_hills)*total_hills)) %>%
  group_by(GAME_ID, team, hill) %>%
  mutate(rotated = (hill > 1 & 
                      score - min(score) >= 5 & 
                      seconds_into_hill == 9),
         rotated = cumany(rotated)) %>%
  group_by(GAME_ID, team) %>%
  mutate(gain = score - lag(score), 
         running_gain = zoo::rollsumr(gain, k = 10, fill = 0),
         running_120 = zoo::rollsumr(gain, k = 120, fill = 0),
         ended = (running_gain > 5 & 
                    seconds_into_hill == 58)) %>%
  filter(score == cummax(score)) %>%
  group_by(GAME_ID, TIME_S) %>%
  mutate(broken = running_gain == 0 & rotated & sum(running_gain) > 0) %>%
  group_by(GAME_ID, team, hill) %>%
  mutate(broken = cumany(broken),
         hold = !cumany(broken) & rotated) %>%
  group_by(GAME_ID, hill) %>%
  mutate(broke = cumany(broken) & !broken) %>%
  group_by(GAME_ID, team, hill, hill_no, broken) %>%
  mutate(first_broken = (row_number() == 1 & broken)*seconds_into_hill) %>%
  group_by(GAME_ID, team, hill, hill_no, broke) %>%
  mutate(first_broke = (row_number() == 1 & broke)*seconds_into_hill) ->
  pp_data

##
pp_data %>%
  filter(GAME_ID == 474) %>%
  ggplot()+geom_line(aes(TIME_S, rotated*100,  color = team))+
  geom_line(aes(TIME_S, running_gain, color = team))+
  geom_line(aes(TIME_S, broke*50, color = team)) +
  geom_line(aes(TIME_S, (seconds_into_hill==1)*50)) +
  ggtitle("Title")+
  theme_bw() +
  theme(panel.border = element_rect(color = '#3d1866', fill = NA),
        axis.text = element_text(face = 'bold', color = '#3d1866', size = 16),
        axis.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        legend.text = element_text(face = 'bold', color = '#3d1866', size = 12),
        legend.title = element_text(face = 'bold', color = '#3d1866', size = 14),
        title = element_text(face = 'bold', color = '#3d1866', size = 17),
        text = element_text(family = 'mono'))


pp_data %>%
  left_join(game_q %>% left_join(series_q) %>% select(GAME_ID, DATE) %>% collect(), "GAME_ID") %>%
  filter(DATE > "2021-02-01") %>%
  group_by(GAME_ID, TEAM_ID, hill, hill_no, MAP_ID) %>%
  summarise(rotated = any(rotated),
            broken = any(broken),
            ended = any(ended),
            first_broken  = sum(first_broken),
            broke = any(broke),
            first_broke = sum(first_broke),
            score = max(score)-min(score)
            ) %>%
  group_by(GAME_ID, hill, hill_no, MAP_ID) %>%
  mutate(opp_score = sum(score)-score,
         opp_rotated = any(rotated) & !rotated,
         plus_minus = score- opp_score) %>%
  group_by(GAME_ID) %>%
  filter(hill != max(hill)) ->
  rotation_data

rotation_data %>%
  filter(TEAM_ID == 6, MAP_ID == 41) %>%
  group_by(MAP_ID, hill_no, rotated) %>%
  summarise(rotation_score = mean(score)-mean(opp_score),
            rot_pts = mean(score),
            hills = n()) %>%
  ggplot(aes(hill_no, rotation_score, fill = rotated))+
  geom_col(position = 'dodge') +
  facet_wrap(~MAP_ID)

rotation_data %>%
  filter(TEAM_ID == 6, MAP_ID == 41) %>%
  group_by(MAP_ID, breakoff = (hill == 1), hill_no) %>%
  summarise(rotation_score = mean(score)-mean(opp_score),
            rot_pts = mean(score),
            hills = n())
# %>%
  ggplot(aes(hill, rotation_score))+
  geom_col(position = 'dodge') +
  facet_wrap(~MAP_ID)


rotation_data %>%
  filter(TEAM_ID == 6) %>%
  filter(rotated, MAP_ID < 50, MAP_ID > 40) %>%
  group_by(MAP_ID, hill_no) %>%
  summarise(break_time = mean(first_broken + (!broken)*60),
            us = TRUE, n = n()) %>%
  bind_rows(rotation_data %>%
              filter(TEAM_ID != 6) %>%
              filter(rotated, MAP_ID < 50, MAP_ID > 40) %>%
              group_by( MAP_ID, hill_no) %>%
              summarise(break_time = mean(first_broken + (!broken)*60),
                        n= n(),
                        us = FALSE)) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  ggplot(aes(hill_no, break_time,size = n, color = us))+
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~MAP_NAME, scales = 'free') +
  scale_color_manual(values = c('TRUE' = '#60269e', 'FALSE' = "#595959"),
                     labels = c("TRUE" = "LAG", "FALSE" = "THEM"),
                     name = element_blank())+
  theme_minimal()+
  theme(axis.text = element_text(size = 14))+
  ggtitle("Time until broken when rotated")
  
rotation_data %>%
  filter(TEAM_ID == 6) %>%
  filter(opp_rotated) %>%
  group_by(MAP_ID, hill_no) %>%
  summarise(break_time = mean(first_broke + (first_broke == 0)*60),
            us = TRUE, n = n()) %>%
  # bind_rows(rotation_data %>%
  #             filter(opp_rotated) %>%
  #             group_by(TEAM_ID, MAP_ID, hill_no) %>%
  #             summarise(break_time = mean(first_broke + (first_broke == 0)*60)) %>%
  #             filter(TEAM_ID != 6) %>%
  #             group_by(MAP_ID, hill_no) %>%
  #             summarise(break_time = mean(break_time),
  #                       us = FALSE)) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  ggplot(aes(as.factor(hill_no), break_time, fill = us, width = n/50))+
  geom_col(position = 'dodge') +
  facet_wrap(~MAP_NAME, scales = 'free_x') +
  scale_fill_manual(values = c('TRUE' = '#60269e', 'FALSE' = "#595959"))+
  theme_minimal()+
  ggtitle("Time to break when opponent rotated")


## score on hill after other team outscored
rotation_data %>%
  group_by(GAME_ID, TEAM_ID) %>%
  mutate(last_against = lag(plus_minus),
         last_hill = lag(hill_no)) %>%
  filter(TEAM_ID == 6) %>%
  group_by( MAP_ID, hill_no, last = last_against < 0) %>%
  summarise(m = mean(plus_minus)) %>%
  left_join(rotation_data %>%
              group_by(GAME_ID, TEAM_ID) %>%
              mutate(last_against = lag(plus_minus),
                     last_hill = lag(hill_no)) %>%
              filter(TEAM_ID != 6) %>%
              group_by( MAP_ID, hill_no, last = last_against < 0) %>%
              summarise(m = mean(plus_minus)),
            by = c("MAP_ID", "hill_no", "last"),
            suffix = c('.us','.them')) %>%
  mutate(diff = m.us - m.them) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  filter(MAP_ID != 50, !is.na(last)) %>%
  ggplot(aes(hill_no, diff, fill = last))+
  geom_col(position = 'dodge') +
  facet_wrap(~MAP_NAME)+
  geom_hline(yintercept = 0)+
  ggtitle("+/- based on previous hill score")+
  labs(x = "", y = "+/-") +
  # scale_fill_discrete()+
  scale_fill_manual(values = c("#aaaaaa",'#3d1866'),
                    name = "Outscored on \nprevious hill?")+
  theme_bw() +
  theme(panel.border = element_rect(color = '#3d1866', fill = NA),
        axis.text = element_text(face = 'bold', color = '#3d1866', size = 16),
        axis.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        legend.text = element_text(face = 'bold', color = '#3d1866', size = 12),
        legend.title = element_text(face = 'bold', color = '#3d1866', size = 14),
        title = element_text(face = 'bold', color = '#3d1866', size = 17))

rotation_data %>%
  filter(rotated, MAP_ID == 49, score < 20) %>%
  group_by(GAME_ID, TEAM_ID) %>%
  mutate(last_against = lag(opp_score),
         last_hill = lag(hill_no)) %>%
  filter(hill_no == 2) %>%
  left_join(game_q %>% collect(), "GAME_ID") %>%
  left_join(series_q %>% collect(), "SERIES_ID") %>%
  left_join(ref_q %>% collect(), "REF_ID") %>%
  arrange(desc(DATE)) %>%
  select(A_ID, B_ID,hill, score,opp_score, REF_VALUE)
  group_by(MAP_ID, hill_no, last_against > 40) %>%
  summarise(m = mean(score)) %>%
  ggplot(aes(hill_no, m, fill = `last_against > 40`))+
  geom_col(position = 'dodge') +
  facet_wrap(~MAP_ID)

  
rotation_data %>%
  filter(TEAM_ID == 6, MAP_ID == 43, hill_no == 4, rotated) %>%
  left_join(game_q %>% collect(), "GAME_ID") %>%
  left_join(series_q %>% collect(), "SERIES_ID") %>%
  left_join(ref_q %>% collect(), "REF_ID") %>%
  arrange(desc(DATE)) %>%
  view()
  select(hill, hill_no, A_ID, B_ID,score, REF_VALUE)

rotation_data %>%
