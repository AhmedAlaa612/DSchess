source("DSfinal.r")
library(factoextra)
library(ggplot2)
library(dbplyr)
getUnsupervised <- function() {
  
  plot_output <- fviz_cluster(
    kmeans_result, data = final_clustering_data,
    geom = "point",
    ellipse.type = "convex",
    palette = c("#2E9FDF", "#FF7F50", "#32CD32"),
    ggtheme = theme_bw()
  ) +
    labs(
      title = paste("K-Means Clustering (k = 3)"),
      x = "Principal Component 1",
      y = "Principal Component 2"
    ) +
    theme_bw()
  
  return(plot_output)
}
plot_results <- function(player_type) {
  data <- data 
  if(player_type == "black"){
    data %>%
      mutate(white_elo_level = case_when(
        white_rating < 1000 ~ "< 1000",
        between(white_rating, 1000, 1100) ~ "1000-1100",
        between(white_rating, 1100, 1200) ~ "1100-1200",
        between(white_rating, 1200, 1300) ~ "1200-1300",
        between(white_rating, 1300, 1400) ~ "1300-1400",
        between(white_rating, 1400, 1500) ~ "1400-1500",
        between(white_rating, 1500, 1600) ~ "1500-1600",
        between(white_rating, 1600, 1700) ~ "1600-1700",
        between(white_rating, 1700, 1800) ~ "1700-1800",
        between(white_rating, 1800, 1900) ~ "1800-1900",
        between(white_rating, 1900, 2000) ~ "1900-2000",
        between(white_rating, 2000, 2100) ~ "2000-2100",
        between(white_rating, 2100, 2200) ~ "2100-2200",
        between(white_rating, 2200, 2300) ~ "2200-2300",
        between(white_rating, 2300, 2400) ~ "2300-2400",
        white_rating > 2400 ~ "> 2400",
        TRUE ~ NA_character_
      )) %>%
      mutate(black_elo_level = case_when(
        black_rating < 1000 ~ "< 1000",
        between(black_rating, 1000, 1100) ~ "1000-1100",
        between(black_rating, 1100, 1200) ~ "1100-1200",
        between(black_rating, 1200, 1300) ~ "1200-1300",
        between(black_rating, 1300, 1400) ~ "1300-1400",
        between(black_rating, 1400, 1500) ~ "1400-1500",
        between(black_rating, 1500, 1600) ~ "1500-1600",
        between(black_rating, 1600, 1700) ~ "1600-1700",
        between(black_rating, 1700, 1800) ~ "1700-1800",
        between(black_rating, 1800, 1900) ~ "1800-1900",
        between(black_rating, 1900, 2000) ~ "1900-2000",
        between(black_rating, 2000, 2100) ~ "2000-2100",
        between(black_rating, 2100, 2200) ~ "2100-2200",
        between(black_rating, 2200, 2300) ~ "2200-2300",
        between(black_rating, 2300, 2400) ~ "2300-2400",
        black_rating > 2400 ~ "> 2400",
        TRUE ~ NA_character_
      )) %>%
      mutate(outcome = case_when(
        winner == "white" ~ "Loss",
        winner == "black" ~ "Win",
        winner == "draw" ~ "Draw",
        TRUE ~ NA_character_
      )) %>%
      mutate(
        black_elo_level = factor(black_elo_level, levels = c(
          "< 1000", "1000-1100", "1100-1200", "1200-1300", "1300-1400",
          "1400-1500", "1500-1600", "1600-1700", "1700-1800", "1800-1900",
          "1900-2000", "2000-2100", "2100-2200", "2200-2300", "2300-2400", "> 2400"
        ))
      ) %>%
      group_by(black_elo_level, outcome) %>%
      summarise(count = n(), .groups = 'drop') %>%
      ggplot(aes(reorder(black_elo_level, as.numeric(black_elo_level)), count, fill = outcome)) +
      geom_bar(stat = "identity", position = "fill", alpha = 0.95, width = 0.7) +
      scale_fill_grey(start = 0.2, end = 0.9) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Result of games by lichess rating (black player)",
        x = "Average rating",
        fill = "Outcome",
        y = "Share of games",
        caption = ""
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }else{
    data %>%
      mutate(white_elo_level = case_when(
        white_rating < 1000 ~ "< 1000",
        between(white_rating, 1000, 1100) ~ "1000-1100",
        between(white_rating, 1100, 1200) ~ "1100-1200",
        between(white_rating, 1200, 1300) ~ "1200-1300",
        between(white_rating, 1300, 1400) ~ "1300-1400",
        between(white_rating, 1400, 1500) ~ "1400-1500",
        between(white_rating, 1500, 1600) ~ "1500-1600",
        between(white_rating, 1600, 1700) ~ "1600-1700",
        between(white_rating, 1700, 1800) ~ "1700-1800",
        between(white_rating, 1800, 1900) ~ "1800-1900",
        between(white_rating, 1900, 2000) ~ "1900-2000",
        between(white_rating, 2000, 2100) ~ "2000-2100",
        between(white_rating, 2100, 2200) ~ "2100-2200",
        between(white_rating, 2200, 2300) ~ "2200-2300",
        between(white_rating, 2300, 2400) ~ "2300-2400",
        white_rating > 2400 ~ "> 2400",
        TRUE ~ NA_character_
      )) %>%
      mutate(black_elo_level = case_when(
        black_rating < 1000 ~ "< 1000",
        between(black_rating, 1000, 1100) ~ "1000-1100",
        between(black_rating, 1100, 1200) ~ "1100-1200",
        between(black_rating, 1200, 1300) ~ "1200-1300",
        between(black_rating, 1300, 1400) ~ "1300-1400",
        between(black_rating, 1400, 1500) ~ "1400-1500",
        between(black_rating, 1500, 1600) ~ "1500-1600",
        between(black_rating, 1600, 1700) ~ "1600-1700",
        between(black_rating, 1700, 1800) ~ "1700-1800",
        between(black_rating, 1800, 1900) ~ "1800-1900",
        between(black_rating, 1900, 2000) ~ "1900-2000",
        between(black_rating, 2000, 2100) ~ "2000-2100",
        between(black_rating, 2100, 2200) ~ "2100-2200",
        between(black_rating, 2200, 2300) ~ "2200-2300",
        between(black_rating, 2300, 2400) ~ "2300-2400",
        black_rating > 2400 ~ "> 2400",
        TRUE ~ NA_character_
      )) %>%
      mutate(outcome = case_when(
        winner == "white" ~ "Win",
        winner == "black" ~ "Loss",
        winner == "draw" ~ "Draw",
        TRUE ~ NA_character_
      )) %>%
      mutate(
        white_elo_level = factor(white_elo_level, levels = c(
          "< 1000", "1000-1100", "1100-1200", "1200-1300", "1300-1400",
          "1400-1500", "1500-1600", "1600-1700", "1700-1800", "1800-1900",
          "1900-2000", "2000-2100", "2100-2200", "2200-2300", "2300-2400", "> 2400"
        ))
      ) %>%
      group_by(white_elo_level, outcome) %>%
      summarise(count = n(), .groups = 'drop') %>%
      ggplot(aes(reorder(white_elo_level, as.numeric(white_elo_level)), count, fill = outcome)) +
      geom_bar(stat = "identity", position = "fill", alpha = 0.95, width = 0.7) +
      scale_fill_grey(start = 0.2, end = 0.9) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Result of games by lichess rating (white player)",
        x = "Average rating",
        fill = "Outcome",
        y = "Share of games",
        caption = ""
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}
most_openings <- function(){
  opening_counts <- data %>%
    group_by(main_opening) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice_head(n = 20)
  
  # Plot the top 20 most played openings
  ggplot(opening_counts, aes(x = reorder(main_opening, -count), y = count)) +
    geom_bar(stat = 'identity', fill = 'skyblue') +
    coord_flip() +
    labs(title = 'Top 20 Most Played Openings', x = 'Opening', y = 'Frequency') +
    theme_minimal()
}
calculate_time <- function() {
  # Mutate to create white and black Elo levels
  mutated_data <- data %>%
    mutate(white_elo_level = case_when(
      white_rating < 1000 ~ "< 1000",
      between(white_rating, 1000, 1100) ~ "1000-1100",
      between(white_rating, 1100, 1200) ~ "1100-1200",
      between(white_rating, 1200, 1300) ~ "1200-1300",
      between(white_rating, 1300, 1400) ~ "1300-1400",
      between(white_rating, 1400, 1500) ~ "1400-1500",
      between(white_rating, 1500, 1600) ~ "1500-1600",
      between(white_rating, 1600, 1700) ~ "1600-1700",
      between(white_rating, 1700, 1800) ~ "1700-1800",
      between(white_rating, 1800, 1900) ~ "1800-1900",
      between(white_rating, 1900, 2000) ~ "1900-2000",
      between(white_rating, 2000, 2100) ~ "2000-2100",
      between(white_rating, 2100, 2200) ~ "2100-2200",
      between(white_rating, 2200, 2300) ~ "2200-2300",
      between(white_rating, 2300, 2400) ~ "2300-2400",
      white_rating > 2400 ~ "> 2400",
      TRUE ~ NA_character_
    ),
    black_elo_level = case_when(
      black_rating < 1000 ~ "< 1000",
      between(black_rating, 1000, 1100) ~ "1000-1100",
      between(black_rating, 1100, 1200) ~ "1100-1200",
      between(black_rating, 1200, 1300) ~ "1200-1300",
      between(black_rating, 1300, 1400) ~ "1300-1400",
      between(black_rating, 1400, 1500) ~ "1400-1500",
      between(black_rating, 1500, 1600) ~ "1500-1600",
      between(black_rating, 1600, 1700) ~ "1600-1700",
      between(black_rating, 1700, 1800) ~ "1700-1800",
      between(black_rating, 1800, 1900) ~ "1800-1900",
      between(black_rating, 1900, 2000) ~ "1900-2000",
      between(black_rating, 2000, 2100) ~ "2000-2100",
      between(black_rating, 2100, 2200) ~ "2100-2200",
      between(black_rating, 2200, 2300) ~ "2200-2300",
      between(black_rating, 2300, 2400) ~ "2300-2400",
      black_rating > 2400 ~ "> 2400",
      TRUE ~ NA_character_
    ))
  
  # Reshape the data to combine white and black ratings into a single column
  combined_ratings_data <- mutated_data %>%
    select(white_elo_level, black_elo_level, time_control) %>%
    gather(key = "player", value = "elo_level", white_elo_level, black_elo_level)
  # Order Elo levels properly
  ordered_levels <- c(
    "< 1000", "1000-1100", "1100-1200", "1200-1300", "1300-1400", "1400-1500",
    "1500-1600", "1600-1700", "1700-1800", "1800-1900", "1900-2000", "2000-2100",
    "2100-2200", "2200-2300", "2300-2400", "> 2400"
  )
  
  # Reorder the levels in the dataframe
  combined_ratings_data$elo_level <- factor(
    combined_ratings_data$elo_level,
    levels = ordered_levels,
    ordered = TRUE
  )
  
  # Grouping data by Elo level and calculating the average time_taken
  average_time_by_elo <- combined_ratings_data %>%
    group_by(elo_level) %>%
    summarise(average_time = mean(time_control, na.rm = TRUE))
  
  # Plotting the average time taken based on Elo level
  ggplot(average_time_by_elo, aes(x = elo_level, y = average_time)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = "Average Time Taken Based on Rating",
         x = "Rating",
         y = "Average Time Taken(mins)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}
getSupervised <- function() {
  par(mfrow = c(1, 2))  # Set up a 1x2 grid for plots
  
  # Plot for Decision Tree
  plot(test_data$winner, prediction,
       xlab = "Actual Winner", ylab = "Predicted Winner",
       main = paste("Decision Tree\nAccuracy: ", round(acc_tree, 2)))
  
  # Plot for Naive Bayes
  plot(test_data$winner, prediction_nb,
       xlab = "Actual Winner", ylab = "Predicted Winner",
       main = paste("Naive Bayes\nAccuracy: ", round(acc_nb, 2)))
  
  par(mfrow = c(1, 1))  # Reset the plotting layout to the default
}
getArules<- function(){
  library(arulesViz)
  plot(rules, method="grouped matrix", k = 5)
}
