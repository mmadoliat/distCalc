hyper_density <- function(x, p1, p2, p3) {
  return(dhyper(x, p1, p2, p3))
}

hyper_cdf <- function(x, p1, p2, p3) {
  return(phyper(x, p1, p2, p3))
}

hyper_hazard <- function(x, p1, p2, p3) {
  return(hyper_density(x, p1, p2, p3) / (1 - hyper_cdf(x, p1, p2, p3)))
}

