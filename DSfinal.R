data <- read.csv("games.csv", header = TRUE)
head(data)
duplicated(data)
is.na(data)
str(data)

game_moves <- data["moves"]

data$moves <- NULL
# 