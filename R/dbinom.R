dbinom_density <- function(x, p1, p2) {
  return(dnbinom(x, p1, p2))
}

dbinom_cdf <- function(x, p1, p2) {
  return(pbinom(x, p1, p2))
}

dbinom_hazard <- function(x, p1, p2) {
  return(dbinom_density(x, p1, p2) / (1 - dbinom_cdf(x, p1, p2)))
}

