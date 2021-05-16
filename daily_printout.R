


## getting data from last month vs last week
# types of data we want:
# 1. hill scores + win pcts
# 2. side win% for ctl
# 3. player stats
# 4. team stats - epm, 

## get hill + side data
source("helper_get_hillside_data.R")
get_HP_data(days_ = 7) %>%
  group_by(TEAM_ID, GAME_ID, MAP_ID, hill, hill_no) %>%
  summarise(score = max(score)-min(score)) %>%
  group_by(GAME_ID, MAP_ID, hill, hill_no) %>%
  mutate(opp_score = sum(score)-score,
         hill_win = score > opp_score) %>%
  filter(TEAM_ID == 6) %>%
  group_by(TEAM_ID, MAP_ID, hill_no) %>%
  summarise(avg_score = mean(score),
            avg_opp_score = mean(opp_score),
            hill_win_pct = mean(hill_win),
            wins = sum(hill_win),
            losses = n() - wins,
            period = 'week') ->
  week_hp_records

get_HP_data(days_ = 31) %>%
  group_by(TEAM_ID, GAME_ID, MAP_ID, hill, hill_no) %>%
  summarise(score = max(score)-min(score)) %>%
  group_by(GAME_ID, MAP_ID, hill, hill_no) %>%
  mutate(opp_score = sum(score)-score,
         hill_win = score > opp_score) %>%
  filter(TEAM_ID == 6) %>%
  group_by(TEAM_ID, MAP_ID, hill_no) %>%
  summarise(avg_score = mean(score),
            avg_opp_score = mean(opp_score),
            hill_win_pct = mean(hill_win),
            period = 'month') ->
  month_hp_records

week_hp_records %>%
  bind_rows(month_hp_records) %>%
  pivot_longer(c(avg_score, avg_opp_score, hill_win_pct),
               names_to = 'stat') %>%
  group_by(TEAM_ID, MAP_ID, hill_no, stat) %>%
  mutate(delta = (value-(sum(value)-value))/(sum(value)-value)) %>%
  filter(period == "week",
         delta < -0.20,
         stat %in% c("avg_score","hill_win_pct"),
         stat == "hill_win_pct") %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  mutate(printout = paste0(
    "On ",
    MAP_NAME,
    " P",
    hill_no,
    ", we ",
    case_when(stat == "hill_win_pct" ~ "outscored opps ",
              TRUE ~ "scored "),
    scales::percent(delta, accuracy = 0.1),
    ifelse(delta > 0, " more ", " less "),
    case_when(stat == "hill_win_pct" ~ "often this week",
              TRUE ~ "points per hill this week")
  )) %>%
  ungroup() %>%
  select(printout, wins, losses)
  
## control 
get_CTL_games(days_ = 7) %>%
  filter(round < 5) %>%
  group_by(MAP_ID, TEAM_ID, offense) %>%
  summarise(round_win_pct = mean(gain)) %>%
  filter(TEAM_ID == 6) %>%
  rename(side = offense) %>%
  mutate(side = ifelse(side, "OFF", "DEF"),
         period  = "week") %>%
  left_join(get_CTL_games(days_ = 7)  %>%
              group_by(GAME_ID, MAP_ID, TEAM_ID) %>%
              summarise(game_win = sum(gain)==3) %>%
              group_by(TEAM_ID, MAP_ID) %>%
              summarise(win = sum(game_win),
                        loss = n()-win),
            by = c("MAP_ID", "TEAM_ID")) ->
  week_ctl_records

get_CTL_games(days_ = 31) %>%
  filter(round < 5) %>%
  group_by(MAP_ID, TEAM_ID, offense) %>%
  summarise(round_win_pct = mean(gain)) %>%
  filter(TEAM_ID == 6) %>%
  rename(side = offense) %>%
  mutate(side = ifelse(side, "OFF", "DEF"),
         period  = "month") %>%
  left_join(get_CTL_games(days_ = 31)  %>%
              group_by(GAME_ID, MAP_ID, TEAM_ID) %>%
              summarise(game_win = sum(gain)==3) %>%
              group_by(TEAM_ID, MAP_ID) %>%
              summarise(win = sum(game_win),
                        loss = n()-win),
            by = c("MAP_ID", "TEAM_ID")) ->
  month_ctl_records

week_ctl_records %>%
  bind_rows(month_ctl_records) %>%
  pivot_longer(c(round_win_pct),
               names_to = 'stat') %>%
  group_by(TEAM_ID, MAP_ID, side, stat) %>%
  mutate(delta = (value-(sum(value)-value))/(sum(value)-value)) %>%
  filter(period == "week", delta < -0.25) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  mutate(printout = paste0(
    "On ",
    MAP_NAME,
    " CTL ",
    side,
    ", we won ",
    scales::percent(delta, accuracy = 0.1),
    ifelse(delta > 0, " more ", " less "),
    "rounds"
  )) %>%
  ungroup() %>%
  select(printout, win, loss)

get_HP_data(days_ = 2) %>%
  # ungroup() %>%
  # count(MAP_ID)
  group_by(TEAM_ID, GAME_ID, MAP_ID, hill, hill_no) %>%
  summarise(score = max(score)-min(score)) %>%
  group_by(TEAM_ID, GAME_ID, MAP_ID) %>%
  mutate(last_hill = lag(score, default = 0),
         two_hill = score + last_hill) %>%
  group_by(GAME_ID, MAP_ID, hill, hill_no) %>%
  mutate(opp_score = sum(score)-score,
         hill_win = score > opp_score,
         opp_two_hill = sum(two_hill)-two_hill) %>%
  filter(TEAM_ID == 6) %>%
  group_by( MAP_ID, hill_no) %>%
  summarise(avg_score = mean(score),
            avg_two_hill = mean(two_hill),
            avg_opp_score = mean(opp_score),
            avg_opp_two_hill = mean(opp_two_hill),
            hill_win_pct = mean(hill_win),
            wins = sum(hill_win),
            losses = n() - wins,
            period = 'day') %>%
  filter((MAP_ID == 42 & hill_no == 4) |
           (MAP_ID == 41 & hill_no == 2) |
           (MAP_ID == 49 & hill_no == 2) |
           (MAP_ID == 44 & hill_no == 4)
           ) %>%
  left_join(map_q %>% collect()) %>%
  select(MAP_NAME, hill = hill_no, avg_score, avg_two_hill, wins, losses)
