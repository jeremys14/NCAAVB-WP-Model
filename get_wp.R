library(tidyverse)

stats_adj <- read_csv("stats_adj.csv")
reg_components <- read_csv("reg_components.csv")
rownames(reg_components) <- reg_components$stat
pred_model <- read_rds("model.rds")
home_away_mod <- read_rds("home_away_mod.rds")
## this is the adjustment we will use for home vs away
home_adj <- coefficients(home_away_mod)[2]




## function that regresses a certain stat based on reg_components
## value = value of stat
## n = number of points
regress_stat <- function(stat_name, value, n)
{
  row_to_use <- reg_components[stat_name,]
  df <- (row_to_use["total_points"]*row_to_use["average"]  + value*n)/
    (row_to_use["total_points"] + n)
  df
  
}
  

regressed_stats <- 
  stats_adj %>% 
  group_by(team, Season) %>% 
  summarise(across(all_of(reg_components$stat), 
                   ~regress_stat(cur_column(), 
                                 weighted.mean(., game_total_points), sum(game_total_points))))
# Identify which columns are data frames
is_df_col <- sapply(regressed_stats, function(x) is.data.frame(x))
regressed_stats <- regressed_stats %>%
  mutate(across(
    names(regressed_stats)[is_df_col],
    ~ .x[[1]]
  ))

### calculates the probability of a team winning a set, given the probability 
## of the team winning a point
## N represents the number of points needed to win 
## should usually be 15 or 25, but this function should theoretically work for any number
get_p.set <- function(p.point, N = 25)
{
  # The probability of winning a set before extra points is a negative binomial distribution with
  # r(probability of success) = N and X <= N-2 (probability of failure)
  # In other words, we are calculating the probability the opponent has N-2 or 
  ## fewer points when the team gets N. 
  
  p.set <- pnbinom(N - 2, N, p.point)
  
  ## now we have to account for extra points (e.g., the probability each team has 24 after 48 points)
  p.xp <- dbinom(N-1, (N-1)*2, p.point)
  
  ##now we have to calculate the probability of the team winning the set 
  ## given we are in extra points
  ## Essentially, there are 3 scenarios: 
  #*The team wins 2 points in a row
  #*The opponent wins 2 points in a row
  #*We return to the same scenario
  ## This is a markov chain
  p.winxp <- p.point^2 / (1 - 2*p.point*(1-p.point))
  
  ## now combine to get final prob
  p.set + p.xp*p.winxp
  
}
  
## function to calculate the win probability of a specific matchup
calc_wp <- function(currteam, opponent, year, home_away)
{
  if(! home_away %in% c("H", "A", "N")) 
  {
    stop("Invalid option for home_away variable!")
  }
  data <- regressed_stats %>% 
    filter(Season == year, team == currteam | team == opponent ) 
  
  if(nrow(data) == 0) {stop("Could not find data!")}
  
  ## use the model to predict each team
  predicted.set <- predict(pred_model, newdata = data)
  names(predicted.set) <- data$team
  
  ## now calculate the probability of a point
  p.point <- predicted.set[currteam]/(predicted.set[currteam] + predicted.set[opponent])
  
  ## adjust for home_away
  p.point <- p.point + case_when(
    home_away == "H" ~ home_adj,
    home_away == "A" ~ -home_adj,
    .default = 0
  )
  
  ## get probability of winning a set
  p.set <- get_p.set(p.point, 25)
  
  ## now the probability of winning the match is:
  ## P(team wins at least 3/4 sets) + P(team wins 2/4 sets and wins set to 15)
  
  pbinom(2, 4, p.set, lower.tail = F) + dbinom(2,4,p.set)*get_p.set(p.point, 15)
  
  
}