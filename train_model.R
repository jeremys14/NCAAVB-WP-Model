library(tidyverse)
library(caret)
library(gbm)

stats_adj <- read_csv("adjusted_stats.csv")


logit <- function(x)
{
  log(x/ ( 1-x))
}

## select only the variable we use
dataset <- stats_adj %>% 
  select(total_points, hit_pct, assists, r_err, total_blocks, digs)
## adjust variables to make them unbounded
dataset <- dataset %>% 
  mutate_at(vars(contains("hit_pct")),atanh) %>% 
  mutate_at(vars(!contains("hit_pct")), logit)


## try a few different models

ctrl <- trainControl(
  method = "cv",
  number = 5
)

## gradient boosting
gbm <- train(
  total_points ~ .,
  data = dataset,
  method = "gbm",
  metric = "RMSE",
  trControl = ctrl,
  preProcess  = c("center", "scale"),
  trace = F)

## random forest
rf <- train(
  total_points ~ .,
  data = dataset,
  method = "rf",
  metric = "RMSE",
  trControl = ctrl,
  preProcess  = c("center", "scale"),
  trace = F)


## neural netowrk
nn <- train(
  total_points ~ .,
  data = dataset,
  method = "nnet",
  metric = "RMSE",
  trControl = ctrl,
  preProcess  = c("center", "scale"),
  trace = F)

## after evaluating metrics (RMSE, R-squred), we have determined that the gbm model fits the best

final_model <- gbm
final_model %>% write_rds("model.rds")


## now we find the home/away adjustment. 
home_away <- stats_adj %>% 
  select(total_points, home_away) %>% 
  ## filter out neutral sites
  filter(home_away %in% c("H", "A"))

home_away_mod <- lm(total_points ~ home_away, data = home_away)



home_away_mod %>% write_rds("home_away_model.csv")


