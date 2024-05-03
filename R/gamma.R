gamma_density <- function(x, p1, p2) {
  return(dgamma(x, p1, p2))
}

gamma_cdf <- function(x, p1, p2) {
  return(pgamma(x, p1, p2))
}

gamma_hazard <- function(x, p1, p2) {
  return(gamma_density(x, p1, p2) / (1 - gamma_cdf(x, p1, p2)))
}

