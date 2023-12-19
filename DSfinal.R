library(tidyr)
data <- read.csv("games.csv", header = TRUE)
#head(data)
#duplicated(data)
#is.na(data)
#str(data)
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
#str(data)
# slicing main opening names
data <- separate(data, opening_name, into = c("main_opening", "variation"), sep = ":", extra = "merge", fill = "right")
data$variation <- NULL
#str(data)
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
# diff in rating 
clustering_data$rating_difference <- ifelse(data$winner == "draw", -0.5 * abs(data$white_rating - data$black_rating),
                                 ifelse(data$winner == "white", data$white_rating - data$black_rating, data$black_rating - data$white_rating))

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
#str(encoded_df)
final_clustering_data = cbind(clustering_data, encoded_df)
#final_clustering_data <- clustering_data
final_clustering_data$victory_status <- NULL
final_clustering_data$winner <- NULL
# get mean rating
final_clustering_data$mean_rating <- (final_clustering_data$white_rating + final_clustering_data$black_rating)/2
final_clustering_data$white_rating <-NULL
final_clustering_data$black_rating <-NULL
# Perform k-means clustering (let's use k = 3 for this example)
library(ggpubr)
library(factoextra)
set.seed(123)
pca_result <- prcomp(final_clustering_data, scale. = TRUE)

# Extract the transformed data (scores) from PCA
pca_data <- as.data.frame(pca_result$x)
kmeans_result <- kmeans(pca_data, centers = k, nstart = 25)
#summary(kmeans_result)
############################################################################
###################################################
#           understand the data                   #
###################################################
# rated -> true or false -> 1, 0
# turns -> 1 : 349 -> log to be taken
# victory status -> one hot encoding
# winner (target)-> factor as numeric
# time control -> 1 : 180 -> log to be taken
# increment -> 0 : 180 -> log to be taken
# white rating, black rating -> 780 : 2700 -> log to be taken
# main opening -> label encoding -> log
# opening ply -> 1:28 -> log to be taken
#str(data)
#min(data$opening_ply)
#hist(data$turns, xlab= "values", ylab="freq")
###################################################
#              pre-processing                     #
###################################################
# encode categorical features
supervised_data <- data
supervised_data <- supervised_data %>%
  group_by(main_opening) %>%
  mutate(opening_freq = n()) %>%
  ungroup()
supervised_data$winner <- ifelse(supervised_data$winner == "white", 0,
                                 ifelse(supervised_data$winner == "black", 1, 0.5))
supervised_data <- supervised_data %>%
  mutate(rated = as.numeric(tolower(rated) == "true"))
encoded_columns <- model.matrix(~victory_status - 1, data = supervised_data)
supervised_data <- cbind(supervised_data, encoded_columns)
supervised_data$victory_status <- NULL
supervised_data$main_opening <- NULL
# normalize numerical features
skewed <- c("turns", "time_control", "increment", "white_rating", "black_rating", "opening_ply", "opening_freq")
supervised_data_transformed <- supervised_data
supervised_data_transformed[, skewed] <- lapply(supervised_data_transformed[, skewed], function(x) log(x + 1))
library(caret)
final_supervised_data <- supervised_data_transformed
preprocess_range <- preProcess(supervised_data_transformed[, skewed], method = "range")
final_supervised_data[, skewed] <- predict(preprocess_range, newdata = supervised_data_transformed[, skewed])
###################################################
#           splitting the data                    #
###################################################
set.seed(123)
final_supervised_data$winner <- factor(final_supervised_data$winner)
index <- createDataPartition(final_supervised_data$winner, p = 0.7, list = FALSE)
train_data <- final_supervised_data[index, ]
test_data <- final_supervised_data[-index, ]
###################################################
#                modeling                         #
###################################################
# decision trees
library(rpart)
library(rpart.plot)
model <- rpart(winner~., data = train_data) 
prediction <- predict(model, newdata = test_data, type = "class")
conf_matrix_tree <-confusionMatrix(prediction, test_data$winner)

library(e1071)
model_nb <- naiveBayes(winner ~ ., data = train_data)
prediction_nb <- predict(model_nb, newdata = test_data)
conf_matrix_nb <-confusionMatrix(prediction_nb, test_data$winner)
# Print confusion matrices
#print("Confusion Matrix - Decision Tree:")
#print(conf_matrix_tree)

#print("Confusion Matrix - Naive Bayes:")
#print(conf_matrix_nb)

# Compare accuracy
#acc_tree <- conf_matrix_tree$overall["Accuracy"]
#acc_nb <- conf_matrix_nb$overall["Accuracy"]

#print(paste("Accuracy - Decision Tree: ", acc_tree))
#print(paste("Accuracy - Naive Bayes: ", acc_nb))

# Plotting
#par(mfrow=c(1,2))

# Plot for Decision Tree
#plot(test_data$winner, prediction,
     #xlab = "Actual Winner", ylab = "Predicted Winner",
     #main = paste("Decision Tree\nAccuracy: ", round(acc_tree, 2)))

# Plot for Naive Bayes
#plot(test_data$winner, prediction_nb,
#     xlab = "Actual Winner", ylab = "Predicted Winner",
#     main = paste("Naive Bayes\nAccuracy: ", round(acc_nb, 2)))

############################################################################
# get common patterns in openings with Apriori Algorithm
opening_moves <- mapply(function(moves, ply) moves[1:ply], strsplit(game_moves, " "), data$opening_ply)
library(arules)
opening_transactions <- as(opening_moves, "transactions")
summary(opening_transactions)
rules <- apriori(opening_transactions, parameter = list(support = 0.1, confidence = 0.8))
inspect(rules)
