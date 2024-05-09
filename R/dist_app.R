dist_app<- function() {
  library(shiny)
  shinyApp(ui = dist_ui(), server = dist_server)
}
