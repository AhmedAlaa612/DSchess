source("getGraphs.r")

server <- function(input, output,session) {
  observe({
    if (input$type == "Visualization") {
      updateSelectInput(session, "mode", choices = c( "Openings","win-loss-draw", "Time taken"), selected = "Openings")
    } else {
      updateSelectInput(session, "mode", choices = c("Supervised", "Unsupervised","arules"), selected = "Unsupervised")
      output$player_mode_ui <- renderUI({})
    }
  })
  observeEvent(input$mode ,{
    if(input$mode== "win-loss-draw"){
      output$player_mode_ui <- renderUI({
        radioButtons("player_mode", "Select Player:", choices = c("White", "Black"), selected = "White")
      })

    }else{
      output$player_mode_ui <- renderUI({})
    }
  })

 mode <- reactive({
    input$mode
  })
  type <- reactive({
    input$type
  })
  player_mode <- reactive({
    input$player_mode
  })
  plot_output <- reactive({
    if(type() == "Visualization"){
      if(mode()=="win-loss-draw"){
      if(player_mode()=="White"){
        plot_results("white")
      }else if(player_mode()=="Black"){
        plot_results("black")
      }
      }else if(mode() == "Openings"){
        most_openings()
      }else if(mode()=="Time taken"){
        calculate_time()
        }
    }else{
    if (mode() == "Unsupervised") {
      getUnsupervised()
    } else if (mode() == "Supervised"){
      getSupervised()
    }else if(mode()=="arules"){
      getArules()
    }
      }
  })
  
  output$plot <- renderPlot({
    plot_output()
  })
  
}
