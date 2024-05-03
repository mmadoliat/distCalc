t_density <- function(x, p1) {
  x0 <- dt(x, input$p1)
  return(x0)
}
t_cdf <- function(x, p1) {
  x0 <- pt(x, input$p1)
  return(x0)
}
t_hazard <- function(x, p1){
  x0 <- t_density(x, input$p1) / (1- t_cdf(x, input$p1))
  return(x0)
}
