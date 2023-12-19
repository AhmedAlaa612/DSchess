# app.R

# Load necessary packages
library(shiny)

# Run the Shiny app
shinyApp(ui = source("ui.R")$value, server = source("server.R")$value)
