library(tidyverse)

stats_adj <- read_csv("adjusted_stats.csv")
var_stability <- read_csv("var_stability.csv")


averages <- stats_adj %>% 
  summarise_at(vars(hit_pct, assists, r_err, total_blocks, digs),
               ~weighted.mean(.,game_total_points)) 

avg_tot_points <- mean(stats_adj$game_total_points)


rownames(averages) <- c("average")
rownames(var_stability) <- var_stability$var
  

## get regression component for each stat
reg_components <- as.data.frame(t(averages)) %>% 
  rownames_to_column("stat") %>% 
  mutate(total_points = var_stability[stat, "games"]*avg_tot_points)

## properly format
reg_components$total_points <- as.numeric(reg_components$total_points[[1]])

reg_components %>% write_csv("regression_components.csv")



