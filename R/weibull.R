weibull_density <- function(x, p1, p2) {
  return(dweibull(x, p1, p2))
}

weibull_cdf <- function(x, p1, p2) {
  return(pweibull(x, p1, p2))
}

weibull_hazard <- function(x, p1, p2) {
  return(weibull_density(x, p1, p2) / (1 - weibull_cdf(x, p1, p2)))
}

