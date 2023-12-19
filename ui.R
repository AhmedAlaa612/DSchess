# Define UI
ui <- fluidPage(
  titlePanel("Chess dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Select Type:", choices = c("Visualization", "Models"), selected = "Visualization"),
      selectInput("mode", "Select Mode:", choices = c("Openings","win-loss-draw","Time taken"), selected = "Openings"),
      uiOutput("player_mode_ui"),
    ),
    mainPanel(
      plotOutput("cluster_plot")
    )
  )
)
