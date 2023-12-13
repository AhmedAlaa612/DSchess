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
str(encoded_df)
final_clustering_data = cbind(clustering_data, encoded_df)
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
k <- 3
pca_result <- prcomp(final_clustering_data, scale. = TRUE)

# Extract the transformed data (scores) from PCA
pca_data <- as.data.frame(pca_result$x)
kmeans_result <- kmeans(pca_data, centers = k, nstart = 25)

# Plot the cluster plot using clusplot
fviz_cluster(kmeans_result, data = final_clustering_data,
             palette = c("#2E9FDF", "#000000","#FF7F50", "#8A2BE2", "#32CD32", "#800400"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
############################################################################
#predicting the winner before the game with supervised learning
#decision trees
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
supervised_data <- data

head(data)
tree <- rpart(winner ~., data = data)
rf_data <- clustering_data
rf_data$victory_status <- NULL
rf <- randomForest(rated ~ turns + time_control, data = rf_data)
rpart.plot(tree)
############################################################################
# get common patterns in openings with Apriori Algorithm
