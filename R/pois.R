pois_density <- function(x, p1) {
  return(dpois(x, p1))
}

pois_cdf <- function(x, p1) {
  return(ppois(x, p1))
}

pois_hazard <- function(x, p1) {
  return(pois_density(x, p1) / (1 - pois_cdf(x, p1)))
}

