binom_density <- function(x, p1, p2) {
  return(dbinom(x, p1, p2))
}

binom_cdf <- function(x, p1, p2) {
  return(pbinom(x, p1, p2))
}

binom_hazard <- function(x, p1, p2) {
  return(binom_density(x, p1, p2) / (1 - binom_cdf(x, p1, p2)))
}

