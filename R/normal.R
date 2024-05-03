norm_density <- function(x, p1, p2) {
  x0 <- dnorm(x, input$p1, input$p2)
  return(x0)
}
norm_cdf <- function(x, p1, p2){
  x0 <- pnorm(x, input$p1, input$p2)
  return(x0)
}
norm_hazard <- function(x, p1, p2){
  x0 <- norm_density(x, input$p1, input$p2) / (1-norm_cdf(x, input$p1, input$p2))
  return(x0)
}
