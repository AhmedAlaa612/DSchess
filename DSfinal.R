library(tidyr)
data <- read.csv("games.csv", header = TRUE)
head(data)
duplicated(data)
is.na(data)
str(data)

game_moves <- data$moves
players <- c(data$white_id, data$black_id)
date_of_games <- data$created_at
# remove unrelated data
data$moves <- NULL
data$id <- NULL
data$black_id <- NULL
data$white_id <- NULL
data$last_move_at <- NULL
data$created_at <- NULL
data$opening_eco <- NULL
str(data)
# slicing main opening names
data <- separate(data, opening_name, into = c("main_opening", "variation"), sep = ":", extra = "merge", fill = "right")
data$variation <- NULL
str(data)
data$variation <- NULL
# slicing time
data <- separate(data, increment_code, into = c("time_control", "increment"), sep = "\\+")
data$increment_code <- NULL
data$time_control <- as.numeric(data$time_control)
data$increment <- as.numeric(data$increment)
###########################################################################
#data for clustering
library(caret)
library(dplyr)
clustering_data <- data
# rated (TRUE, FALSE) -> 1, 0
clustering_data$rated <- ifelse(data$rated == "TRUE", 1, 0)
# count encode opening names
counts <- table(clustering_data$main_opening)
clustering_data <- clustering_data %>%
  mutate(opening = counts[main_opening])
clustering_data$opening <- as.numeric(clustering_data$opening)
clustering_data$main_opening <- NULL
#create dummy variables for victory status and winner
encoded_data <- dummyVars(formula = ~victory_status+winner-1, data =  clustering_data, sep = '_')
encoded_df <- data.frame(predict(encoded_data, newdata = data))
str(encoded_df)
final_clustering_data = cbind(clustering_data, encoded_df)
final_clustering_data$victory_status <- NULL
final_clustering_data$winner <- NULL

