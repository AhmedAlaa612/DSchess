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
str(data)
# slicing main opening names
data <- separate(data, opening_name, into = c("main_opening", "variation"), sep = ":")
data$variation <- NULL
str(data)