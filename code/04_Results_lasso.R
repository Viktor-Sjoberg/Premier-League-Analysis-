setwd("/Users/viktorsjoberg/Desktop/high-dimensional/Final project")
library(glmnet)
library(readr)
library(gt)

model_glmnet <- read_rds("fit/model_glmnet_lasso.rds")
pl_18_19_fin_x_s <- read_rds("data/pl_18_19_fin_x_s.rds")
pl_18_19_fin_y <- read_rds("data/pl_18_19_fin_y.rds")
teams_18_19 <- read_rds("data/teams_18_19.rds")
pl_18_19 <- read_csv("data/england-premier-league-2018-to-2019.csv")
best_lambda <- read_rds("values/best_lambda_lasso.rds")
best_threshold1 <- read_rds("values/best_threshold1_lasso.rds")
best_threshold2 <- read_rds("values/best_threshold2_lasso.rds")

predict_pl_18_19 <- predict(model_glmnet, newx  = pl_18_19_fin_x_s, s = best_lambda, type = "class" )

predict_pl_18_19_factor <- factor(
  cut(predict_pl_18_19, breaks = c(-Inf, best_threshold1, best_threshold2, Inf), labels = c("1", "2", "3"))
)

table <- cbind(teams_18_19, predict_pl_18_19_factor)



calculate_points_pre <- function(df) {
  # Points for home games
  home_points <- ifelse(df$predict_pl_18_19_factor == "1", 3, ifelse(df$predict_pl_18_19_factor == "2", 1, 0))
  # Points for away games
  away_points <- ifelse(df$predict_pl_18_19_factor == "3", 3, ifelse(df$FTR == "2", 1, 0))
  
  # Sum points for each team
  home_sum <- aggregate(home_points, by=list(df$HomeTeam), FUN=sum)
  away_sum <- aggregate(away_points, by=list(df$AwayTeam), FUN=sum)
  
  # Merge the two sets of points and sum them up for each team
  names(home_sum) <- c("Team", "Points")
  names(away_sum) <- c("Team", "Points")
  
  # Combine home and away points
  all_points <- merge(home_sum, away_sum, by="Team", all=TRUE)
  all_points$Points <- rowSums(all_points[, c("Points.x", "Points.y")], na.rm = TRUE)
  
  # Select only the relevant columns
  league_table <- all_points[, c("Team", "Points")]
  
  # Sort the league table in descending order of points
  league_table <- league_table[order(-league_table$Points),]
  
  return(league_table)
}

# Calculate the league table
league_table_pre <- calculate_points_pre(table)
Position <- c(1:20)
league_fin <- cbind(Position, league_table_pre)


calculate_points <- function(df) {
  # Points for home games
  home_points <- ifelse(df$FTR == "H", 3, ifelse(df$FTR == "D", 1, 0))
  # Points for away games
  away_points <- ifelse(df$FTR == "A", 3, ifelse(df$FTR == "D", 1, 0))
  
  # Sum points for each team
  home_sum <- aggregate(home_points, by=list(df$HomeTeam), FUN=sum)
  away_sum <- aggregate(away_points, by=list(df$AwayTeam), FUN=sum)
  
  # Merge the two sets of points and sum them up for each team
  names(home_sum) <- c("Team", "Points")
  names(away_sum) <- c("Team", "Points")
  
  # Combine home and away points
  all_points <- merge(home_sum, away_sum, by="Team", all=TRUE)
  all_points$Points <- rowSums(all_points[, c("Points.x", "Points.y")], na.rm = TRUE)
  
  # Select only the relevant columns
  league_table <- all_points[, c("Team", "Points")]
  
  # Sort the league table in descending order of points
  league_table <- league_table[order(-league_table$Points),]
  
  return(league_table)
}

# Calculate the league table
league_act <- calculate_points(pl_18_19)
Position_act <- c(1:20)
league_table_fin_act <- cbind(Position_act, league_act)
league_table_fin_act$Points_act <- league_table_fin_act$Points
league_table_fin_act <- select(league_table_fin_act, -c("Points"))

merged_data <- merge(league_table_fin_act, league_fin, by = "Team")

# Calculate the differences and add as new columns
merged_data$Position_diff <- merged_data$Position_act - merged_data$Position
merged_data$Points_diff <- merged_data$Points_act - merged_data$Points

# Create a new table with the columns you need
final_table <- merged_data[, c("Position_act","Points_act", "Team", "Position_diff", "Points_diff")]

final_table_ordered <- final_table[order(final_table$Position_act), ]

# View the reordered table
final_table_ordered_gt <- gt(final_table_ordered)
print(final_table_ordered_gt)
gt_output2 <- "img/lasso_table_results.png"
gtsave(final_table_ordered_gt, gt_output2)
