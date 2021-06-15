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

## to update DB with more recent dates' followers
tbl(con, "FOLLOWERS") %>%
  group_by(team) %>% 
  summarise(date = max(date)) %>% 
  collect() %>% 
  filter(date < Sys.Date()) ->
  needs_updated
  

## get recent tweets for each team
rtweet::get_timeline(team_handles %>% unlist(),n = 100) ->
  teams_tweets
