### Predicting expected value of each hill
### Using gaem-state predictors

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

## Get data to train/test models
data <- score_q %>%
  left_join(game_q %>%
              select(MAP_ID, MODE_ID, GAME_ID, A_SCORE = TEAM_A_SCORE,
                     B_SCORE = TEAM_B_SCORE), by = "GAME_ID") %>%
  collect() %>%
  pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE), names_to = "team", values_to = "score") %>%
  group_by(GAME_ID, team) %>%
  filter(score >= 0,
         # TIME_S > 4, 
         score < 250,
         MAP_ID < 50,
         # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
         !cumany(score == 249),
         MODE_ID == 1,
         score == cummax(score)
  ) %>%
  # left_join(hill_num, by = "")
  mutate(win = ifelse(team == "TEAM_A_SCORE", A_SCORE > B_SCORE, B_SCORE > A_SCORE),
         hill = pmax((TIME_S-5) %/% 60, 0)+1,
         seconds_into_hill = ((TIME_S-5) %% 60)+1,
         total_hills = ifelse(MAP_ID %in% c(43,44, 46,49), 5, 4),
         hill_no = hill-(((hill-1)%/%total_hills)*total_hills)) %>%
  select(GAME_ID, TIME_S, team, score, win, seconds_into_hill) %>%
  group_by(GAME_ID, TIME_S) %>%
  mutate(opp_score = sum(score)-score)
  # group_by(GAME_ID, team, hill, hill_no) %>%
  # mutate(getting_last_10 = any(score - lag(score) > 0 & 
  #                                seconds_into_hill == 55)) %>%
  # group_by(GAME_ID,MAP_ID, team, win) %>%
  # summarise(scored = max(score)-min(score),
  #           start_score = min(score)) %>%
  # group_by(GAME_ID,MAP_ID, hill, hill_no) %>%
  # mutate(opp_start_score = sum(start_score)-start_score)
  # group_by(MAP_ID, hill_no, at_old) %>%
  # summarise(score = mean(score)) %>%
  # group_by(MAP_ID, hill_no) %>%
  # mutate(diff = score/mean(score)) %>%
  # filter(at_old == T) %>%
  # group_by(MAP_ID) %>%
  # summarise(m =mean(diff))
  # ggplot(aes(hill_no, score, fill = at_old))+geom_col(position= 'dodge')+
  # facet_wrap(~MAP_ID)

pp_data <-
  data %>%
  ungroup() %>%
  filter(TIME_S > 6) %>%
  mutate(diff = score - opp_score,
         win = as.factor(win),
         ttw = 250 - pmax(score, opp_score),
         seconds_left = 60-seconds_into_hill) %>%
  # filter(diff >= 0) %>%
  select(win, diff,ttw, seconds_left)

library(tidymodels)
set.seed(1)
splits <- initial_split(pp_data)
train <- training(splits)
test <- testing(splits)
folds <- vfold_cv(train, v = 10)
val_set <- validation_split(train)
# test %>% 
#   ungroup() %>%
#   count(MAP_ID) %>% 
#   mutate(prop = n/sum(n))

## define a recipe for our model
score_rec <-
  recipe(win ~ diff+ttw, data = train) %>%
  # step_interact(terms=~diff:seconds_left) %>%
  step_interact(terms=~diff:ttw) %>%
  # update_role(GAME_ID, MAP_ID, team, hill, hill_no, start_score, opp_start_score,
  #             new_role = "ID") %>% 
  step_zv(all_predictors())

summary(score_rec)
# lm_mod <- 
#   linear_reg() %>% 
#   set_engine("lm")
glm_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

score_wflow <-
  workflow() %>%
  add_model(glm_mod) %>%
  add_recipe(score_rec)

score_fit <-
  score_wflow %>%
  fit(data = train)

## extract the model or fit 
score_fit %>%
  pull_workflow_fit() %>%
  tidy()


predict(score_fit, test)

score_wflow %>%
  fit_resamples(folds) %>%
  collect_metrics()

score_wflow %>%
  last_fit(splits) %>%
  collect_predictions() %>%
  # bind_cols(test) %>%
  ggplot(aes(.pred_TRUE, fill = win))+geom_density(alpha = 0.3)



### Make a model to predict win% given score

data <- score_q %>%
  left_join(game_q %>%
              mutate(TEAM_A_WIN = TEAM_A_SCORE > TEAM_B_SCORE) %>%
              select(MAP_ID, MODE_ID, GAME_ID, TEAM_A_WIN), by = "GAME_ID") %>%
  collect() %>%
  mutate(to_win = 250 - pmax(TEAM_A_SCORE,TEAM_B_SCORE), 
         pm = TEAM_A_SCORE - TEAM_B_SCORE) %>%
  pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE), names_to = "team", values_to = "score") %>%
  group_by(GAME_ID) %>%
  filter(score >= 0,
         # TIME_S > 4, 
         score < 250,
         MAP_ID < 46,
         # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
         !cumany(score == 249),
         MODE_ID == 1,
         score == cummax(score)
  ) %>%
  mutate(WIN = (team == "TEAM_A_SCORE" & TEAM_A_WIN) | (team == "TEAM_B_SCORE" & !TEAM_A_WIN),
         PM = ifelse(team == "TEAM_A_SCORE", pm, -pm)) %>%
  select(GAME_ID, SCORE_ID, PM, to_win, WIN) %>%
  mutate(WIN = factor(WIN))


library(tidymodels)
set.seed(1)
splits <- initial_split(data)
train <- training(splits)
test <- testing(splits)
folds <- vfold_cv(train, v = 10)

## define a recipe for our model
win_rec <-
  recipe(WIN ~ to_win + PM, data = train) %>%
  # update_role(GAME_ID, MAP_ID, team, hill, hill_no, start_score, opp_start_score,
  #             new_role = "ID") %>% 
  step_zv(all_predictors())

summary(win_rec)
glm_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

score_wflow <-
  workflow() %>%
  add_model(glm_mod) %>%
  add_recipe(win_rec)

score_fit <-
  score_wflow %>%
  last_fit(folds)
  fit(data = train)

## extract the model or fit 
score_fit %>%
  pull_workflow_fit() %>%
  tidy()


predict(score_fit, test, type = 'prob')

resample_fit <-
  score_wflow %>%
  fit_resamples(folds) %>%
  select_best('roc_auc')
resample_fit %>%
  collect_predictions() %>% 
  roc_curve(class, .pred_TRUE) %>% 
  autoplot()

score_wflow %>%
  last_fit(splits) %>%
  collect_predictions() %>%
  bind_cols(test %>% select(PM, to_win)) %>%
  ggplot(aes(.pred_TRUE, color = WIN))+geom_histogram()









