library(tidyverse)

trailing_zero <- function(x, max_len) sapply(x, function(y) ifelse(nchar(y) >= max_len, y, paste0(rep('0', max_len - nchar(y)), y)))

elo_df <- read.csv('https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv', stringsAsFactors = F) %>% 
  rename(gameday = date, game_type = playoff, location = neutral) %>% 
  rename_with(function(x) paste0('home_', gsub('1','', x)), contains('1')) %>% 
  rename_with(function(x) paste0('away_', gsub('2','', x)), contains('2')) %>% 
  group_by(season) %>% 
  mutate(
    gameday = as.Date(gameday),
    weekday = format(gameday, '%A'), 
    week1_wed = min(gameday) - as.numeric(format(min(gameday), '%u')) + 3,
    week = as.numeric(ceiling((gameday - week1_wed)/7)),
    week = ifelse(gameday == '2012-09-05', 1, week),
    week1_wed = NULL,
    game_id = paste(season, trailing_zero(week, 2), away_team, home_team, sep = '_'),
    game_type = case_when(
      game_type == 's' ~ 'SB',
      game_type == 'c' ~ 'CON',
      game_type == 'd' ~ 'DIV',
      game_type == 'w' ~ 'WC',
      game_type == '' ~ 'REG'
    ),
    location = ifelse(location == 0, 'Home', 'Neutral'),
    result = home_score - away_score,
    total = home_score + away_score
  ) %>%
  ungroup %>% 
  relocate(game_id) %>% 
  relocate(c(game_type, week, gameday, weekday), .after = season) %>% 
  relocate(c(home_score, away_score, location, result, total), .after = away_team)

double_games <- function(df) {
  home_tms <- df %>% 
    rename_with(function(x) gsub('home_','team_', x), contains('home_')) %>% 
    rename_with(function(x) gsub('away_','opp_', x), contains('away_')) %>% 
    rename(team = team_team, opp = opp_team)
  
  away_tms <- df %>% 
    rename_with(function(x) gsub('away_','team_', x), contains('away_')) %>% 
    rename_with(function(x) gsub('home_','opp_', x), contains('home_')) %>% 
    rename(team = team_team, opp = opp_team) %>% 
    mutate(
      result = -1 * result,
      location = ifelse(location == 'Home', 'Away', location)
    ) %>% 
    relocate(home_tms %>% names)
  
  rbind(away_tms, home_tms) %>% arrange(gameday) %>% return
}

elo_df %>% 
  double_games %>% 
  group_by(season, team) %>% 
  mutate(
    wins = cumsum(ifelse(result > 0, 1, 0)),
    losses = cumsum(ifelse(result < 0, 1, 0)),
    ties = cumsum(ifelse(result == 0, 1, 0))
  ) %>%
  ungroup %>% 
  filter(wins == 11 & losses == 0 & ties == 0) %>% 
  select(season, team, team_elo_post, team_qbelo_post) %>% 
  arrange(-team_elo_post)

