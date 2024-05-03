nbinom_density <- function(x, p1, p2) {
  return(dnbinom(x, p1, p2))
}

nbinom_cdf <- function(x, p1, p2) {
  return(pnbinom(x, p1, p2))
}

nbinom_hazard <- function(x, p1, p2) {
  return(nbinom_density(x, p1, p2) / (1 - nbinom_cdf(x, p1, p2)))
}

