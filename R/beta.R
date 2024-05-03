beta_density <- function(x, p1, p2) {
  return(dbeta(x, p1, p2))
}

beta_cdf <- function(x, p1, p2) {
  return(pbeta(x, p1, p2))
}

beta_hazard <- function(x, p1, p2) {
  return(beta_density(x, p1, p2) / (1 - beta_cdf(x, p1, p2)))
}

