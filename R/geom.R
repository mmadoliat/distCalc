geom_density <- function(x, p1) {
  return(dgeom(x, p1))
}

geom_cdf <- function(x, p1) {
  return(pgeom(x, p1))
}

geom_hazard <- function(x, p1) {
  return(geom_density(x, p1) / (1 - geom_cdf(x, p1)))
}

