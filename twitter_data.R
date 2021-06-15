library(rtweet)
library(httr)
library(odbc)
library(DBI)
library(tidyverse)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "twitter",
                      username    = 'admin',
                      password    = "laguerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)
# create token named "twitter_token"
twitter_token <- rtweet::create_token(app = 'LAGUERRILLAS',
                                      access_token = "475506931-pbutGEk6uTFATF2EB0Di4RFWCPxZYt0aEE4zLm0u",
                                      access_secret = "Kt9RKqnuwvt5mR3y8TaRhZK9YU6M9YrepIVUxr6kinGZX",
                                      consumer_key = 'u6rCegqZlxFccujYLGMskH269',
                                      consumer_secret = 'Bl2e4xnTsU9lFkFPoyqtD7YZE1OHFXFHM44DTSAyOI25Ae3UTu')


team_handles <- list(
  LAG = "laguerrillas",
  ATL = "ATLFaze",
  MIN = "rokkr",
  FLA = "mutineers",
  SEA = "SeattleSurge",
  DAL = "DallasEmpire",
  TOR = "TorontoUltra",
  PAR = "Parislegion",
  NYC = "subliners", 
  LON = "RoyalRavens",
  LAT = "LAThieves",
  CHI = "opticCHI"
)


## how many total follows does cnn have?
lookup_users(team_handles %>% unlist()) ->
  current_followers

out <- tibble()
for(team in team_handles) {
  recent_followers <- get_followers(team, n = 10000)
  creation_dates_recent <- lookup_users(recent_followers$user_id)$account_created_at
  creation_dates_recent %>%
    as_tibble() %>%
    mutate(id = 1:n()) %>%
    arrange(desc(id)) %>% 
    mutate(max_date = lubridate::as_datetime(cummax(as.integer(value))),
           bigger = value > lag(value),
           date = lubridate::as_date(max_date),
           diff = as.numeric(max_date-value)/60/60/24) %>%
    group_by(date) %>% 
    summarise(followers = current_followers[tolower(current_followers$screen_name) ==
                                           tolower(team),]$followers_count -
                (max(id)),
              n = n()) %>%
    filter(n > 10) ->
    date_followers_team
  
    date_followers_team %>%
      right_join(tibble(team = team, 
                        date = seq.Date(min(date_followers_team$date),
                                        max(date_followers_team$date), by = 'day'))) %>% 
      arrange(date) %>%
      fill(followers, .direction = 'down') %>%
      bind_rows(out) ->
      out
    
}

DBI::dbWriteTable(con, "FOLLOWERS", out %>% select(-n), row.names = F, overwrite = T)

## get user IDs of accounts following CNN
lag_flw <- get_followers("laguerrillas", n = 10000)

## lookup data on those accounts
lag_flw_data <- lookup_users(lag_flw$user_id)

## created dates of recent follows
lag_flw_data$account_created_at ->
  accounts_created

creation_dates_recent %>%
  as_tibble() %>%
  mutate(id = 1:n()) %>%
  arrange(desc(id)) %>% 
  mutate(max_date = lubridate::as_datetime(cummax(as.integer(value))),
         bigger = value > lag(value),
         date = lubridate::as_date(max_date)) %>% 
  group_by(date) %>% 
  summarise(gained = max(current_followers - id) - min(current_followers - id) ) %>% 
  arrange(desc(gained)) %>% 
  ggplot(aes(gained))+geom_histogram()
  # summarise(mean(bigger, na.rm = T))
  ggplot(aes(date, current_followers-id))+
  geom_line()
