user_app <- function() {
  library(shiny)
  shinyApp(ui = user_ui(), server = user_server)
}

