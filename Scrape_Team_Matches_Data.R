library(tidyverse)
library(lubridate)
library(rvest)
library(janitor)

setwd("~/Desktop/Volleyball")
team_to_id <- read_csv("team_to_id.csv") 
year_to_id <- read_csv("year_to_id.csv")

get_team_id <- function(team)
{
  ret <- team_to_id$ID[team_to_id$Team == team]
  if(length(ret) != 1){ stop("Cound not find team")}
  ret
}

get_year_id <- function(year)
{
  ret <- year_to_id$ID[year_to_id$Year == year]
  if(length(ret) != 1){ stop("Cound not find year")}
  ret
}

get_matches_url <- function(team, year)
{
  paste0("https://stats.ncaa.org/player/game_by_game?game_sport_year_ctl_id=",
  get_year_id(year),"&org_id=",get_team_id(team),"&stats_player_seq=-100")
}

## returns a df with team stats for each match over a given season
get_match_stats <- function(team_name, year)
{
  url <- get_matches_url(team_name, year)
  page <- curl::curl(url) %>% read_html()
  
  matches <- page %>% html_nodes(xpath = '//*[@id="game_breakdown_div"]/table') %>% html_table(fill=TRUE)
  
  if(length(matches) == 0) {return()}
  
  ## data cleaning
  matches_new <- matches[[1]] %>% slice(3:n())  %>% remove_empty(which = c("cols")) %>% 
    row_to_names(row_number = 1) %>% clean_names() %>% 
    mutate_all(na_if,"") %>% fill(c(date, result)) %>% mutate_at(vars(-date, -opponent, -result),  replace_na, '0') %>% 
    mutate(date = mdy(date), 
           team_score = as.numeric(substr(result, 1, 1)),
           opponent_score = as.numeric(substr(result, 3, 3)),
           home_away = case_when(startsWith(opponent, "@")  ~ "A", grepl("@",opponent) ~ "N", TRUE ~ "H"), 
           opponent = gsub(" @.*,.*[.]?", "", opponent), 
           opponent = gsub("@ ","",opponent), 
           WinLoss = case_when(team_score < opponent_score ~ "L", team_score > opponent_score ~ "W")) %>% select(-result) %>% 
    rename(result = WinLoss) %>% mutate(team = team_name)  %>% rename(opponent_info = opponent) %>% 
    mutate(opponent = case_when(opponent_info == "Defensive Totals" ~ lag(opponent_info, n=1), TRUE ~ opponent_info)) %>% 
    select(date, team, opponent, home_away, result, team_score, opponent_score, everything(), -starts_with("ms"), -starts_with("attend"), -starts_with("mp")) %>% clean_names() %>% 
    mutate_at(vars(-date, -opponent, -home_away, -result, -team, -opponent_info), ~str_replace(., "/", "")) %>% mutate_at(vars(-date, -team, -opponent, -home_away, -result, -opponent_info), as.numeric) %>% 
    mutate(total_blocks = block_solos + .5*block_assists)
  
  ## seperate into team stats vs opponent stats
  teamside <- matches_new %>% filter(opponent_info != "Defensive Totals") %>% select(-opponent_info)
  
  opponentside <- matches_new %>% filter(opponent_info == "Defensive Totals") %>% select(-opponent_info) %>% select(-home_away) %>% rename_with(.cols = kills:total_blocks, function(x){paste0("opponent_", x)})
  
  ## join them together
  joinedmatches <- inner_join(teamside, opponentside, by = c("date", "team", "opponent", "result", "team_score", "opponent_score", "s"))
  
  match_stats <- joinedmatches %>% 
    mutate(total_points = kills + opponent_r_err + opponent_s_err + opponent_bhe + opponent_errors,
           opponent_total_points = opponent_kills + r_err + bhe + errors,
           game_total_points = total_points + opponent_total_points) %>% 
    ## make everything a ratio of per point, rather than raw totals
    mutate_at(vars(kills:opponent_total_points, -ends_with("hit_pct"), -starts_with("pct"), 
                   -starts_with("opponent_pct")), function(x) {x/(.$total_points + .$opponent_total_points)}) %>% 
    mutate(Season = year) %>% 
    select(date, Season, everything())
  match_stats 
  
  
}

## make a list of each team, carrying each year
team_match_stats <- data.frame()
for(team in team_to_id$Team)
{
  if(team %in% team_match_stats$team) {next()}
  for(year in year_to_id$Year)
  {
    df <- get_match_stats(team, year)
    print(paste0("Adding ", team, " stats from ", year))
    team_match_stats <- bind_rows(team_match_stats, df)
  }
}

## data cleaning
team_match_stats <- team_match_stats %>% 
  mutate(hit_pct = coalesce(hit_pct, pct),
         opponent_hit_pct = coalesce(opponent_hit_pct, opponent_pct)) %>% 
  select(-contains("ret_att"), -pct, -opponent_pct, -ends_with("tb")
                                                ) %>% 
  filter(opponent %in% team_match_stats$team)



## correlation matrix
cor_mat <- team_match_stats_useful %>% ungroup() %>% select(-(date:home_away), -game_number) %>% 
  cor(use = "pairwise.complete.obs")
  

## now based on the correlations, we select the stats that actually matter
team_match_stats_useful <- 
  team_match_stats %>% select(date:home_away, game_total_points,ends_with("total_points"),
                              ends_with("hit_pct"),
                              ends_with("kills"), ends_with("assists"), r_err,digs,
                              total_blocks) %>% 
  group_by(team,Season) %>% 
  mutate(game_number = row_number(team)) %>% 
  ungroup()


### now we will see how long it takes each var to stabilize. This will
## be to determine how much weight to put on priors

## gets the average of certain stats over n number of games 
## NOTE: This will fail if no variable called team, Season, game_number, game_total_points
## lag = True to use lag function, False to use lead
average_of_n_games <- function(data, n, lag, vars_to_exclude = c())
{
  averages <- data.frame()
  for(i in 1:n)
  {
    last_ith <- data %>% 
      select(-any_of(vars_to_exclude)) %>% 
      mutate_at(vars(-game_total_points, -game_number,-Season, -team), 
                ~if_else(rep(lag,nrow(data)),
                        lag(.,i), lead(.,i)))
    
      averages <- rbind( last_ith, averages) %>% 
        group_by(team, Season, game_number) %>% 
        summarise(
         across(-game_total_points, ~weighted.mean(.,w = game_total_points,
                     na.rm =T)),
         game_total_points = sum(game_total_points))

  }
  averages
}


## returns a named vector of the correlation of each variable. 
## data1 and data2 must have each variable in vars  
get_cor_by_var <- function(data1, data2, vars)
{
  cor <- c()
  for (var in vars)
  {
    cor[var] <- cor(data1[[var]], data2[[var]],
                    use = "pairwise.complete.obs")
  }
  cor
}

vars <- names(team_match_stats_useful %>% select(
  -(date:game_total_points), -game_number
))

vars_used <- c()
var_stability <- data.frame(var = c(), games = c())
for(i in 1:10)
{
  average_prev <- average_of_n_games(team_match_stats_useful, i, T,  c("date", "opponent", "home_away"))
  average_next <- average_of_n_games(team_match_stats_useful, i, F,  c("date", "opponent", "home_away"))
  cors <- get_cor_by_var(average_prev, average_next, setdiff(vars, vars_used))
  ## take all vars with cor > 0.5
  new_vars <- names(cors[cors > 0.5])
  if(length(new_vars) > 0)
  {
    var_stability <- rbind(var_stability, 
                          data.frame(var = new_vars,
                                     games = i))
    vars_used <- union(vars_used, new_vars)
    
  }
}

## now take out the variables that are unstable after 10 games
team_match_stats_useful <- team_match_stats_useful %>% 
  select(date:game_total_points, game_number, all_of(vars_used))

## take out variables that correlate with each other
team_match_stats_useful <- team_match_stats_useful %>% select(
  -kills, -block_assists, -opponent_total_points)

vars_used <- intersect(vars_used, team_match_stats_useful %>% names())

team_match_stats_useful %>% write_csv("team_match_stats_used.csv")

## adjust for opponent
yearly_averages <- 
  team_match_stats_useful %>% 
  group_by(Season) %>% 
  summarise_at(vars(all_of(vars_used)), ~weighted.mean(., w = game_total_points))


## get relative data in dataset, based on an average dataset. Must have same variables
get_relative_set <- function(dataset, average_dataset, by)
{
  joined_set <- dataset %>% inner_join(average_dataset, by = by,
                                       suffix = c("", "_avg"))
  relative_set <- matrix(nrow = nrow(dataset), ncol = ncol(dataset),
                         dimnames = list(NULL, names(dataset))) %>% as.data.frame()
  for (var in names(dataset))
  {
    var_avg <- paste0(var, "_avg")
    if (!(var_avg %in% names(joined_set)) )
    {
      relative_set[[var]] <- dataset[[var]]
    }
    else
    {
      relative_set[[var]] <- (joined_set[[var]] - joined_set[[var_avg]])/joined_set[[var_avg]]
    }
  }
  relative_set
}


## adjusts data based on opponent data. Dataset must have variable named opponent
adjust <- function(dataset, opponent_avg_set, by)
{
  joined_set <- dataset %>% left_join(opponent_avg_set, by = c(by, "Season"),
                                       suffix = c("", "_opp"))
  adjusted_set <- matrix(nrow = nrow(dataset), ncol = ncol(dataset),
                         dimnames = list(NULL, names(dataset))) %>% as.data.frame()
  for (var in names(dataset))
  {
    var_opp <- paste0(var, "_opp")
    if (!(var_opp %in% names(joined_set)) )
    {
      adjusted_set[[var]] <- joined_set[[var]]
    }
    else
    {
      adjusted_set[[var]] <- joined_set[[var]] - 
        replace_na(joined_set[[var_opp]]*joined_set[[var]], 0)
    }
  }
  adjusted_set

}

## repeat process 3 times for ideal results
team_stats_adj <- team_match_stats_useful
for(i in 1:3)
{
  opp_yearly_averages <- team_stats_adj %>% 
    group_by(opponent, Season) %>% 
    summarise_at(vars(all_of(vars_used)), 
                 ~weighted.mean(., w = game_total_points))
  
  relative_stats <- get_relative_set(opp_yearly_averages, yearly_averages, by = c("Season"))
  
  team_stats_adj <- adjust(team_stats_adj, relative_stats, by = c("opponent"))

  
}

## get rid of bad outliers
team_stats_adj <- team_stats_adj %>% 
  filter_at(vars(all_of(vars_used), -hit_pct), all_vars(. < 1 & . > 0 ))

## now we adjust for home and away

home_away_averages <- team_stats_adj %>% 
  filter(home_away %in% c("H", "A")) %>% 
  group_by(home_away, Season) %>% 
  summarise_at(vars(all_of(vars_used)), 
               ~weighted.mean(., w = game_total_points))

relative_stats <- get_relative_set(home_away_averages, y