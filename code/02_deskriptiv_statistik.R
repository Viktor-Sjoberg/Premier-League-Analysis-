setwd("/Users/viktorsjoberg/Desktop/high-dimensional/Final project")
library(gt)
library(dplyr)
library(ggplot2)
library(readr)

pl_17_18 <- read_csv("data/england-premier-league-2017-to-2018.csv")


########## Leauge Table ########## 
# Initialize a data frame to store the league table
teams <- unique(c(pl_17_18$HomeTeam, pl_17_18$AwayTeam))
league_table <- data.frame(Team = teams, Points = integer(length(teams)), stringsAsFactors = FALSE)

# Function to calculate points for each team
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
league_table <- calculate_points(pl_17_18)
Position <- c(1:20)
league_table_fin <- cbind(Position, league_table)
league_table_gt <- gt(league_table_fin)

# Display the league table
print(league_table_gt)
gt_output2 <- "img/league_table_gt.png"
gtsave(league_table_gt, gt_output2)

########## Goals ########## 

# Aggregate goals for each team at home and away
home_goals <- pl_17_18 %>% group_by(HomeTeam) %>% summarise(TotalHomeGoals = sum(FTHG))
away_goals <- pl_17_18 %>% group_by(AwayTeam) %>% summarise(TotalAwayGoals = sum(FTAG))

# Combine home and away goals to get total goals for each team
total_goals <- home_goals %>% 
  rename(Team = HomeTeam) %>% 
  full_join(away_goals %>% rename(Team = AwayTeam), by = "Team") %>%
  mutate(TotalGoals = TotalHomeGoals + TotalAwayGoals) %>%
  arrange(desc(TotalGoals)) %>%
  top_n(10, TotalGoals)

# Create the bar plot with ggplot
top_goals_plot <- ggplot(total_goals, aes(x = reorder(Team, TotalGoals), y = TotalGoals, fill = Team)) +
  geom_bar(stat = "identity") +
  labs(x = "Team", y = "Total Goals Scored", title = "Top 10 Goal Scoring Teams in the Season") +
  geom_text(aes(label = TotalGoals), vjust = 0.5, hjust = 1, color = "black", size = 3.5, position = position_dodge(width = 0.9)) +
  theme_minimal() +
  coord_flip() + # Flip the coordinates to make it a horizontal bar plot
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("img/top_goals_plot.png", plot = top_goals_plot, width = 12, height = 6, bg = "white")



## GOal difference
home_goals <- pl_17_18 %>% group_by(HomeTeam) %>% summarise(TotalHomeGoals = sum(FTHG))
away_goals <- pl_17_18 %>% group_by(AwayTeam) %>% summarise(TotalAwayGoals = sum(FTAG))

# Aggregate goals conceded for each team at home and away
home_goals_conceded <- pl_17_18 %>% group_by(HomeTeam) %>% summarise(TotalHomeGoalsConceded = sum(FTAG))
away_goals_conceded <- pl_17_18 %>% group_by(AwayTeam) %>% summarise(TotalAwayGoalsConceded = sum(FTHG))

# Combine home and away goals to get total goals for each team
total_goals_scored <- home_goals %>% 
  rename(Team = HomeTeam) %>% 
  full_join(away_goals %>% rename(Team = AwayTeam), by = "Team") %>%
  mutate(TotalGoalsScored = TotalHomeGoals + TotalAwayGoals)

# Combine home and away goals conceded to get total goals conceded for each team
total_goals_conceded <- home_goals_conceded %>% 
  rename(Team = HomeTeam) %>% 
  full_join(away_goals_conceded %>% rename(Team = AwayTeam), by = "Team") %>%
  mutate(TotalGoalsConceded = TotalHomeGoalsConceded + TotalAwayGoalsConceded)

# Combine total goals scored and conceded to calculate goal difference
goal_difference <- total_goals_scored %>%
  full_join(total_goals_conceded, by = "Team") %>%
  mutate(GoalDifference = TotalGoalsScored - TotalGoalsConceded) %>%
  select(Team, TotalGoalsScored, TotalGoalsConceded, GoalDifference) %>%
  arrange(desc(GoalDifference))

# View the goal difference table
print(goal_difference)

goal_difference_gt <- gt(goal_difference)
gtsave(goal_difference_gt, "img/goal_difference_plot.png")

home_points <- ifelse(pl_17_18$FTR == "H", 3, ifelse(pl_17_18$FTR == "D", 1, 0))
away_points <- ifelse(pl_17_18$FTR == "A", 3, ifelse(pl_17_18$FTR == "D", 1, 0))



## Team performance home vs away 
# Calculate home and away points separately
home_points_table <- aggregate(home_points, by=list(pl_17_18$HomeTeam), FUN=sum)
away_points_table <- aggregate(away_points, by=list(pl_17_18$AwayTeam), FUN=sum)
names(home_points_table) <- c("Team", "HomePoints")
names(away_points_table) <- c("Team", "AwayPoints")

# Combine home and away points
team_performance <- merge(home_points_table, away_points_table, by="Team", all=TRUE)

# Combine home and away goals scored and conceded
team_performance <- team_performance %>%
  left_join(total_goals_scored, by = "Team") %>%
  left_join(total_goals_conceded, by = "Team")

# Calculate Win-Loss-Draw count for Home and Away
team_performance$HomeWins <- pl_17_18 %>%
  filter(FTR == "H") %>%
  group_by(HomeTeam) %>%
  summarise(Wins = n()) %>%
  rename(Team = HomeTeam) %>%
  right_join(team_performance, by = "Team") %>%
  .$Wins

team_performance$HomeLosses <- pl_17_18 %>%
  filter(FTR == "A") %>%
  group_by(HomeTeam) %>%
  summarise(Losses = n()) %>%
  rename(Team = HomeTeam) %>%
  right_join(team_performance, by = "Team") %>%
  .$Losses

# Calculate AwayWins (Wins away from home)
team_performance$AwayWins <- pl_17_18 %>%
  filter(FTR == "A") %>%
  group_by(AwayTeam) %>%
  summarise(Wins = n()) %>%
  rename(Team = AwayTeam) %>%
  right_join(team_performance, by = "Team") %>%
  .$Wins

# Calculate AwayDraws (Draws away from home)
team_performance$AwayDraws <- pl_17_18 %>%
  filter(FTR == "D") %>%
  group_by(AwayTeam) %>%
  summarise(Draws = n()) %>%
  rename(Team = AwayTeam) %>%
  right_join(team_performance, by = "Team") %>%
  .$Draws

team_performance$HomeDraws <- pl_17_18 %>%
  filter(FTR == "D") %>%
  group_by(HomeTeam) %>%
  summarise(Draws = n()) %>%
  rename(Team = HomeTeam) %>%
  right_join(team_performance, by = "Team") %>%
  .$Draws

# Calculate AwayLosses (Losses away from home, i.e., when the home team wins)
team_performance$AwayLosses <- pl_17_18 %>%
  filter(FTR == "H") %>%
  group_by(AwayTeam) %>%
  summarise(Losses = n()) %>%
  rename(Team = AwayTeam) %>%
  right_join(team_performance, by = "Team") %>%
  .$Losses

# Select and arrange columns for better readability
team_performance <- team_performance %>%
  select(Team, HomePoints, AwayPoints, TotalGoalsScored, TotalGoalsConceded, 
         HomeWins, HomeDraws, HomeLosses, AwayWins, AwayDraws, AwayLosses)

# Print the team performance table
print(team_performance)

team_performance_gt <- gt(team_performance)

gtsave(team_performance_gt, gt_output2)



## Match-level Statistics
# Total number of matches
total_matches <- nrow(pl_17_18)

# Total Wins, Draws, and Losses
total_home_wins <- sum(pl_17_18$FTR == "H")
total_away_wins <- sum(pl_17_18$FTR == "A")
total_draws <- sum(pl_17_18$FTR == "D")

# Total goals scored in the league
total_goals_scored <- sum(pl_17_18$FTHG) + sum(pl_17_18$FTAG)

# Average goals per match
average_goals_per_match <- total_goals_scored / total_matches

# Frequency of match outcomes
frequency_home_win <- total_home_wins / total_matches
frequency_away_win <- total_away_wins / total_matches
frequency_draw <- total_draws / total_matches

# Print the match-level statistics
print(paste("Total Matches:", total_matches))
print(paste("Total Home Wins:", total_home_wins))
print(paste("Total Away Wins:", total_away_wins))
print(paste("Total Draws:", total_draws))
print(paste("Total Goals Scored:", total_goals_scored))
print(paste("Average Goals Per Match:", round(average_goals_per_match, 2)))
print(paste("Frequency of Home Wins:", round(frequency_home_win, 2)))
print(paste("Frequency of Away Wins:", round(frequency_away_win, 2)))
print(paste("Frequency of Draws:", round(frequency_draw, 2)))




## Some visualizations
# Data preparation for match outcomes
match_outcomes <- data.frame(
  Outcome = c("Home Wins", "Away Wins", "Draws"),
  Frequency = c(total_home_wins, total_away_wins, total_draws)
)

# Bar plot for match outcomes
ggplot(match_outcomes, aes(x = Outcome, y = Frequency, fill = Outcome)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Match Outcomes", x = "Outcome", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("img/match_outcomes_plot.png", width = 8, height = 4, bg = "white")

# Data preparation for average goals per match
avg_goals_data <- data.frame(
  Category = "Average Goals Per Match",
  Value = average_goals_per_match
)



## Correlation
library(ggplot2)
library(reshape2)
library(dplyr)

# Load your dataset
# You should have your dataset 'pl_17_18' loaded in R

# Select only numerical columns from your dataset
numerical_data <- pl_17_18 %>% select_if(is.numeric)

# Calculate correlation matrix
cor_matrix <- cor(numerical_data, use = "complete.obs")

# Get the lower triangle of the correlation matrix
cor_matrix[upper.tri(cor_matrix)] <- NA

# Melt the correlation matrix
melted_cor_matrix <- melt(cor_matrix, na.rm = TRUE)

# Create heatmap with a legend and clearer intersections
heatmap_plot <- ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") + # Adding a white border around the tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") + # Re-enabling the legend
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(filename = "heatmap.png", plot = heatmap_plot, path = "img", width = 10, height = 8, dpi = 300)
