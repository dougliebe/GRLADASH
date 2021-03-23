source("load_data_rds.R")

base <- score_q %>%
  left_join(game_q %>% 
              filter(MAP_ID != 46) %>%
              mutate(TEAM_A_WIN = TEAM_A_SCORE > TEAM_B_SCORE) %>%
              select(-TEAM_A_SCORE, -TEAM_B_SCORE), by = "GAME_ID") %>%
  select(SERIES_ID, GAME_ID,MAP_ID, TEAM_A_SCORE, TEAM_B_SCORE, TIME_S, TEAM_A_WIN) %>%
  collect() %>%
  pivot_longer(c(-SERIES_ID, -GAME_ID, -MAP_ID, -TIME_S, -TEAM_A_WIN), 
               names_to = 'team',
               values_to = 'score') %>%
  left_join(series_q %>%
              select(-REF_ID) %>%
              collect(), by = "SERIES_ID") %>%
  mutate(team_id = ifelse(team == "TEAM_A_SCORE", A_ID, B_ID)) %>%
  group_by(GAME_ID) %>%
  filter(score >= 0,
         TIME_S > 4, 
         score < 250,
         # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
         !cumany(score == 249),
         !is.na(MAP_ID),
         
  ) %>%
  filter(max(score) > 200) %>%
  group_by(GAME_ID, team_id) %>%
  filter(score == cummax(score)) %>%
  group_by(GAME_ID, team) %>%
  mutate(gain = score - lag(score),
         hill = pmax((TIME_S-5) %/% 60, 0)+1,
         seconds_into_hill = ((TIME_S-5) %% 60)+1,
         new_hill = ifelse(hill > lag(hill), hill, NA),
         total_hills = ifelse(MAP_ID %in% c(43,44, 46,49), 5, 4),
         hill_no = hill-(((hill-1)%/%total_hills)*total_hills),
         WIN = ifelse((team=="TEAM_A_SCORE" & TEAM_A_WIN) | 
                        (team=="TEAM_B_SCORE" & !TEAM_A_WIN), T, F)) %>%
  filter(gain >= 0, gain < 5) %>%
  group_by(GAME_ID, team, hill) %>%
  mutate(rotated = (hill > 1 & 
                      score - min(score) >= 5 & 
                      seconds_into_hill == 10),
         rotated = cumany(rotated)) %>%
  group_by(GAME_ID, team) %>%
  mutate(running_gain = zoo::rollsumr(gain, k = 10, fill = 0)) %>%
  group_by(GAME_ID, TIME_S) %>%
  mutate(broken = seconds_into_hill > 10 & running_gain < max(mean(running_gain),5) & rotated,
         broke = any(broken) & !broken)

## Avg best score per hill
base %>%
  group_by(GAME_ID, MAP_ID,team, hill, hill_no) %>%
  summarise(score = sum(gain)) %>%
  group_by(GAME_ID, MAP_ID,  hill, hill_no) %>%
  summarise(top_score = max(score)) %>%
  group_by(MAP_ID, hill_no) %>%
  summarise(m= median(top_score),
            n = n()) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  ggplot(aes(hill_no, m))+geom_col()+
  facet_wrap(~MAP_NAME, scales = 'free_x')

## hill wins by winner 
base %>%
  group_by(GAME_ID, MAP_ID, WIN, team_id, hill, hill_no) %>%
  summarise(score = sum(gain)) %>%
  group_by(GAME_ID, MAP_ID, hill, hill_no) %>%
  mutate(top_score = (score == max(score))) %>%
  filter(WIN == T) %>%
  group_by(MAP_ID, hill_no) %>%
  summarise(m= mean(top_score),
            n = n()) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  ggplot(aes(hill_no, m))+geom_col()+
  facet_wrap(~MAP_NAME, scales = 'free_x')

# hill score by winners vs losers
base %>%
  group_by(GAME_ID, MAP_ID, WIN, team, hill, hill_no) %>%
  summarise(score = sum(gain)) %>%
  group_by(MAP_ID, WIN,  hill) %>%
  summarise(m= median(score),
            n = n()) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  filter(hill < 10) %>%
  ggplot(aes(as.factor(hill), m, fill = WIN))+
  geom_col(position = "dodge")+
  facet_wrap(~MAP_NAME, scales = 'free_x')

base %>%
  group_by(GAME_ID, MAP_ID, WIN, team, hill, hill_no) %>%
  summarise(score = sum(gain),
            rotated = any(rotated)) %>%
  group_by(MAP_ID,hill_no, rotated) %>%
  summarise(m= median(score),
            b = mean(score >= 30),
            n = n()) %>%
  filter(rotated == T) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  ggplot(aes(hill_no, b))+
  geom_col(position = "dodge")+
  facet_wrap(~MAP_NAME, scales = 'free_x')+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ggtitle("Prob. to get 30+ on a rotation") +
  ylab("30+ points if rotated, %") +
  theme_classic()

## Look at 2-hill combos
base %>%
  group_by(GAME_ID, MAP_ID, team, hill, hill_no) %>%
  summarise(score = sum(gain)) %>%
  group_by(GAME_ID, MAP_ID, team) %>%
  mutate(two_hill = score + lag(score)) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  group_by(GAME_ID, MAP_NAME,  hill, hill_no) %>%
  # ungroup() %>%
  # pivot_wider(c(-score),names_from = WIN, values_from = two_hill) %>%
  # mutate(diff = `TRUE`-`FALSE`) %>%
  # group_by(MAP_NAME,hill_no) %>%
  summarise(max_two = max(two_hill)) %>%
  group_by(MAP_NAME, hill_no) %>%
  summarise(two_hill = median(max_two, na.rm = T)) %>%
  # filter(MAP_NAME == "Crossroads") %>%
  ggplot(aes(hill_no, two_hill))+
  geom_point(position = "dodge")+
  facet_wrap(~MAP_NAME, scales = 'free_x')

## Look at LAG scores over time
base %>%
  group_by(DATE, GAME_ID, MAP_ID, team_id, hill, hill_no) %>%
  summarise(score = sum(gain),
            hold = any(rotated) & score >= 20) %>%
  mutate(us = ifelse(team_id == 6,1,0)) %>%
  group_by(MAP_ID, us, hill_no) %>%
  mutate(m = zoo::rollmeanr(score, k = 20, fill = NA)) %>%
  # summarise(m= median(score),
  #           b = mean(hold),
  #           n = n()) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  filter(DATE >= "2020-01-15", us == 1) %>%
  ggplot(aes(GAME_ID, m, color = as.factor(hill_no)))+
  geom_line(size = 1, alpha = 0.2)+
  geom_smooth(method = 'loess', se = F)+
  facet_wrap(~MAP_NAME, scales = 'free_x')+
  theme_classic() +
  ggtitle("Average Points per Hill since 1/15/21",
          subtitle = "20-game average")+
  scale_fill_manual(values = c('0' = 'gray', '1' = '#60269e')) + 
  # theme(legend.position = 'none')+
  ylab("PPH")

# Look at only LAG scores
base %>%
  group_by(DATE, GAME_ID, MAP_ID, team_id, hill, hill_no) %>%
  summarise(score = sum(gain),
            hold = any(rotated) & score >= 20) %>%
  filter(DATE >= "2021-01-01",team_id == 6) %>%
  # mutate(us = ifelse(team_id == 6,1,0)) %>%
  group_by(MAP_ID, hill_no) %>%
  summarise(m= median(score),
            n = n()) %>%
  # filter(us == 1) %>%
  bind_rows(base %>%
              group_by(GAME_ID, MAP_ID, WIN, team, hill, hill_no) %>%
              summarise(score = sum(gain)) %>%
              group_by(MAP_ID, WIN,  hill_no) %>%
              summarise(m= median(score),
                        n = n()) %>% filter(WIN)) %>%
  mutate(WIN = replace_na(WIN, FALSE)) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  group_by(WIN, MAP_ID) %>%
  filter(MAP_ID %in% c(41:49)) %>%
  mutate(m = m/sum(m)) %>%
  ggplot(aes(hill_no, m, fill = as.factor(WIN)))+
  geom_col(position = "dodge")+
  facet_wrap(~MAP_NAME, scales = 'free_x')+
  theme_classic() +
  ggtitle("Proportion of Points per Hill",
          subtitle = "LAG vs Winner")+
  scale_fill_manual(values = c("TRUE" = 'gray', 'FALSE' = '#60269e')) + 
  theme(legend.position = 'none')+
  ylab("% of Total")

## hill score by team 
base %>%
  group_by(GAME_ID,SERIES_ID, MAP_ID, WIN, team_id, hill, hill_no) %>%
  summarise(score = sum(gain)) %>%
  # group_by(MAP_ID,team_id, hill_no) %>%
  # summarise(m = median(score),
  #           n = n()) %>%
  filter(MAP_ID == 44, hill_no == 5, team_id != 6) %>%
  arrange(desc(score)) %>%
  left_join(series_q %>% select(SERIES_ID, REFERENCE_ID) %>% collect(), by = "SERIES_ID") %>%
  left_join(ref_q %>% collect(), by = "REFERENCE_ID")

## Look at LAG scores before and after date
base %>%
  group_by(DATE, GAME_ID, MAP_ID, team_id, hill, hill_no) %>%
  summarise(score = sum(gain),
            hold = any(rotated) & score >= 20) %>%
  filter(team_id==6) %>%
  mutate(relative_time = ifelse(DATE >= "2021-01-23", "AFTER", "BEFORE")) %>%
  group_by(MAP_ID, relative_time, hill_no) %>%
  # mutate(m = zoo::rollmeanr(score, k = 20, fill = NA)) %>%
  summarise(m= median(score),
            b = mean(hold),
            n = n()) %>%
  left_join(map_q %>% collect(), by = "MAP_ID") %>%
  arrange((relative_time)) %>%
  mutate(relative_time = factor(relative_time, levels = c('BEFORE', "AFTER"))) %>%
  filter( !is.na(MAP_NAME)) %>%
  ggplot(aes(hill_no, m, fill = as.factor(relative_time)))+
  geom_col(position = "dodge")+
  # geom_errorbar(data = base %>%
  #              group_by(DATE, GAME_ID, MAP_ID, team_id, hill, hill_no) %>%
  #              summarise(score = sum(gain),
  #                        hold = any(rotated) & score >= 20) %>%
  #              # filter(team_id==6) %>%
  #              # mutate(relative_time = ifelse(DATE >= "2020-12-10", "AFTER", "BEFORE")) %>%
  #              group_by(MAP_ID, hill_no) %>%
  #              # mutate(m = zoo::rollmeanr(score, k = 20, fill = NA)) %>%
  #              summarise(m= median(score),
  #                        b = mean(hold),
  #                        n = n()) %>%
  #              left_join(map_q %>% collect(), by = "MAP_ID"),
  #              stat = "hline" ) +
  facet_wrap(~MAP_NAME, scales = 'free')+
  theme_classic() +
  ggtitle("Points per hill before and after 1/23")+
  scale_fill_manual(values = c('BEFORE' = 'gray', 'AFTER' = '#60269e')) + 
  theme(legend.title = element_blank())+
  ylab("PPH")


