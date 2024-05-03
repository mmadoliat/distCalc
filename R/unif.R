unif_density <- function(x, p1, p2) {
  return(dunif(x, p1, p2))
}

unif_cdf <- function(x, p1, p2) {
  return(punif(x, p1, p2))
}

unif_hazard <- function(x, p1, p2) {
  return(unif_density(x, p1, p2) / (1 - unif_cdf(x, p1, p2)))
}

