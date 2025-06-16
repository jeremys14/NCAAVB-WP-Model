# NCAAVB-WP-Model
A model that predicts the probability of a team winning a given NCAA Women's Volleyball match, given a collection of stats


The Scrape_Team_Matches_Data.R is where we gather the data from The NCAA stats database. In this file, we scrape data, clean it, and determine our predictive variables. 

year_to_id.csv and team_to_id.csv are spreadsheets containing the id of each team and each season used by the NCAA in their database

train_model.R, as named, is where we train a model to determine the probability of a team winning a point. We tried a handful of different model families, but eventually settled on a gradient boosting model. 

In regress_vars.R, we determine the regression component for each predictive variable. The amount of weight given is determined by how many games it takes for the stat to stabilize (calculated in Scrape_Team_Matches_Data.R). 

In get_wp.R we actually apply the model to make a prediction. Using the dataset created in Scrape_Team_Matches_Data.R, you can call the calc_wp function to calculate the probability of a team winning a match, given the opponent, year, and site (home vs away vs neutral).
![image](https://github.com/user-attachments/assets/1cc6e6d9-fc79-45d3-beee-9408cb24f2dd)


