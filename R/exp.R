exp_density <- function(x, p1) {
  return(dexp(x, p1))
}

exp_cdf <- function(x, p1) {
  return(pexp(x, p1))
}

exp_hazard <- function(x, p1) {
  return(exp_density(x, p1) / (1 - exp_cdf(x, p1)))
}

